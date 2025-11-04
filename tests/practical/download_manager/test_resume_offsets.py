from __future__ import annotations

import importlib.util
import json
import sys
from pathlib import Path
from typing import Iterable, List

import pytest

try:  # pragma: no cover - fallback for minimal environments
    import requests  # type: ignore
except ModuleNotFoundError:  # pragma: no cover
    import types

    fake_requests = types.ModuleType("requests")

    class RequestException(Exception):
        pass

    class ConnectionError(RequestException):
        pass

    class Response:  # minimal placeholder for type import
        pass

    class Session:  # pragma: no cover - not used in tests
        def __init__(self, *args, **kwargs):
            pass

    fake_requests.RequestException = RequestException
    fake_requests.ConnectionError = ConnectionError
    fake_requests.Response = Response
    fake_requests.Session = Session
    sys.modules["requests"] = fake_requests
    requests = fake_requests

try:  # pragma: no cover - used to avoid optional dependency failures
    from tqdm import tqdm  # type: ignore
except ModuleNotFoundError:  # pragma: no cover
    import types

    fake_tqdm = types.ModuleType("tqdm")

    class DummyTqdm:
        def __init__(self, *args, **kwargs):
            pass

        def update(self, value):
            return None

        def __enter__(self):
            return self

        def __exit__(self, exc_type, exc, tb):
            return False

    fake_tqdm.tqdm = DummyTqdm
    sys.modules["tqdm"] = fake_tqdm


def load_dmanager_module():
    module_path = (
        Path(__file__).resolve().parents[3]
        / "challenges"
        / "Practical"
        / "Download Manager"
        / "dManager.py"
    )
    spec = importlib.util.spec_from_file_location(
        "download_manager_module", module_path
    )
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class DummyPbar:
    def __init__(self) -> None:
        self.total = 0

    def update(self, value: int) -> None:
        self.total += value


class RecordingSession:
    def __init__(self, responses: List["FakeResponse"]) -> None:
        self._responses = responses
        self.calls: List[dict[str, str]] = []

    def get(
        self,
        url: str,
        headers: dict[str, str],
        stream: bool,
        timeout: float,
        verify: bool,
    ):  # noqa: D401 - signature matches requests
        self.calls.append(headers.copy())
        if not self._responses:
            raise AssertionError("No more responses configured")
        return self._responses.pop(0)


class FakeResponse:
    def __init__(self, chunks: Iterable[bytes], fail_after: int | None = None) -> None:
        self._chunks = list(chunks)
        self._fail_after = fail_after
        self.headers: dict[str, str] = {}

    def __enter__(self) -> "FakeResponse":
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        return False

    def raise_for_status(self) -> None:
        return None

    def iter_content(self, chunk_size: int) -> Iterable[bytes]:
        emitted = 0
        for chunk in self._chunks:
            if not chunk:
                continue
            yield chunk
            emitted += 1
            if self._fail_after is not None and emitted >= self._fail_after:
                raise requests.ConnectionError("Simulated interruption")


def test_resume_state_skips_previously_downloaded_bytes(tmp_path):
    module = load_dmanager_module()
    Config = module.Config
    ResumeState = module.ResumeState
    download_part = module.download_part

    target = tmp_path / "partial.bin"
    part = (0, 4)
    other_part = (5, 9)
    target.write_bytes(b"\x00" * 10)

    resume_state = ResumeState.initialize(target, 10, [part, other_part])
    cfg = Config(
        url="https://example.com/file.bin",
        threads=2,
        output=target,
        retries=0,
        timeout=1,
        backoff=0.01,
        resume=True,
        chunk_size=4,
        expected_sha256=None,
        json_path=None,
        verify_tls=True,
        user_agent="pytest-agent",
    )

    first_session = RecordingSession([FakeResponse([b"abc", b"de"], fail_after=1)])
    pbar = DummyPbar()

    downloaded_first = download_part(
        first_session,
        cfg,
        part,
        pbar,
        retries=0,
        timeout=cfg.timeout,
        file_path=target,
        resume_state=resume_state,
    )

    assert downloaded_first == 3
    assert resume_state.get_offset(part) == 3
    assert not resume_state.is_complete(part)

    with open(resume_state.meta_path, "r", encoding="utf-8") as fh:
        metadata = json.load(fh)
    assert metadata["parts"][f"{part[0]}-{part[1]}"]["offset"] == 3
    assert metadata["parts"][f"{part[0]}-{part[1]}"]["done"] is False

    second_session = RecordingSession([FakeResponse([b"de"], fail_after=None)])

    downloaded_second = download_part(
        second_session,
        cfg,
        part,
        pbar,
        retries=0,
        timeout=cfg.timeout,
        file_path=target,
        resume_state=resume_state,
    )

    assert downloaded_second == 2
    assert resume_state.is_complete(part)
    assert resume_state.get_offset(part) == 0
    assert second_session.calls[0]["Range"] == "bytes=3-4"
    assert target.read_bytes()[:5] == b"abcde"

    with open(resume_state.meta_path, "r", encoding="utf-8") as fh:
        final_metadata = json.load(fh)
    assert final_metadata["parts"][f"{part[0]}-{part[1]}"]["offset"] == 0
    assert final_metadata["parts"][f"{part[0]}-{part[1]}"]["done"] is True


def test_resume_state_invalid_utf8_metadata(tmp_path):
    module = load_dmanager_module()
    ResumeState = module.ResumeState

    target = tmp_path / "partial.bin"
    target.write_bytes(b"\x00" * 10)
    meta_path = target.with_name(target.name + ".resume.json")
    meta_path.write_bytes("résumé".encode("latin-1"))

    with pytest.raises(ValueError) as excinfo:
        ResumeState.initialize(target, 10, [(0, 9)])

    assert "not valid UTF-8" in str(excinfo.value)
