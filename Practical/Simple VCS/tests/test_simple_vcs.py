"""Tests for the simple VCS implementation."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from simple_vcs.cli import main as cli_main
from simple_vcs.repository import Repository


@pytest.fixture()
def repo_dir(tmp_path: Path) -> Path:
    (tmp_path / "workspace").mkdir()
    return tmp_path / "workspace"


def run_cli(repo_dir: Path, *args: str) -> int:
    argv = ["--path", str(repo_dir), *args]
    return cli_main(argv)


def test_init_and_commit_cycle(repo_dir: Path) -> None:
    run_cli(repo_dir, "init")
    target_file = repo_dir / "example.txt"
    target_file.write_text("first", encoding="utf8")
    assert run_cli(repo_dir, "commit", "-m", "Initial", "example.txt") == 0

    target_file.write_text("second", encoding="utf8")
    assert run_cli(repo_dir, "commit", "-m", "Update", "example.txt") == 0

    # Only two revisions should be present by default
    repo = Repository(repo_dir)
    revisions = repo.list_revisions(target_file)
    assert len(revisions) == 2

    # Checkout previous revision
    run_cli(repo_dir, "checkout", "example.txt", "--revision", revisions[0].revision_id)
    assert target_file.read_text(encoding="utf8") == "first"

    # Checkout head
    run_cli(repo_dir, "checkout", "example.txt")
    assert target_file.read_text(encoding="utf8") == "second"


def test_revision_limit_enforced(repo_dir: Path) -> None:
    run_cli(repo_dir, "init")
    target = repo_dir / "limited.txt"
    target.write_text("zero", encoding="utf8")

    run_cli(repo_dir, "config", "set-limit", "limited.txt", "2")

    for idx in range(4):
        target.write_text(f"value-{idx}", encoding="utf8")
        run_cli(repo_dir, "commit", "-m", f"commit {idx}", "limited.txt")

    repo = Repository(repo_dir)
    revisions = repo.list_revisions(target)
    assert len(revisions) == 2
    assert [rev.message for rev in revisions] == ["commit 2", "commit 3"]


def test_lock_prevents_commits(repo_dir: Path) -> None:
    run_cli(repo_dir, "init")
    target = repo_dir / "locked.txt"
    target.write_text("content", encoding="utf8")
    run_cli(repo_dir, "commit", "-m", "Initial", "locked.txt")

    run_cli(repo_dir, "config", "lock", "locked.txt")
    target.write_text("content 2", encoding="utf8")
    exit_code = run_cli(repo_dir, "commit", "-m", "Update", "locked.txt")
    assert exit_code == 1


def test_diff_and_log_commands(
    repo_dir: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    run_cli(repo_dir, "init")
    target = repo_dir / "log.txt"
    target.write_text("one", encoding="utf8")
    run_cli(repo_dir, "commit", "-m", "Add one", "log.txt")
    target.write_text("two", encoding="utf8")
    run_cli(repo_dir, "commit", "-m", "Add two", "log.txt")

    run_cli(repo_dir, "log", "log.txt")
    captured = capsys.readouterr()
    assert "Add one" in captured.out
    assert "Add two" in captured.out

    target.write_text("two plus", encoding="utf8")
    run_cli(repo_dir, "diff", "log.txt")
    captured = capsys.readouterr()
    assert "two plus" in captured.out


def test_config_show_outputs_json(
    repo_dir: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    run_cli(repo_dir, "init")
    file_path = repo_dir / "data.txt"
    file_path.write_text("data", encoding="utf8")
    run_cli(repo_dir, "commit", "-m", "Initial", "data.txt")
    capsys.readouterr()  # Clear previous command output
    exit_code = run_cli(repo_dir, "config", "show")
    captured = capsys.readouterr()
    assert exit_code == 0, captured.err
    parsed = json.loads(captured.out)
    assert "data.txt" in parsed
    assert parsed["data.txt"]["revisions"]
