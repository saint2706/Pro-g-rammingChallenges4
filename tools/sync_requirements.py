#!/usr/bin/env python3
"""Synchronize requirements.txt with pyproject optional dependencies."""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError as exc:  # pragma: no cover - fallback for old interpreters
    raise SystemExit("Python 3.11+ is required to run this script") from exc

ROOT = Path(__file__).resolve().parents[1]
PYPROJECT = ROOT / "pyproject.toml"
REQUIREMENTS = ROOT / "requirements.txt"


def load_optional_dependencies() -> dict[str, list[str]]:
    data = tomllib.loads(PYPROJECT.read_text(encoding="utf-8"))
    project = data.get("project", {})
    extras = project.get("optional-dependencies")
    if extras is None:
        raise SystemExit(
            "[project.optional-dependencies] section is missing in pyproject.toml"
        )
    return extras


NAME_RE = re.compile(r"^[A-Za-z0-9_.-]+")


def normalize_requirement(requirement: str) -> str:
    """Normalize requirement for consistent key lookup."""
    requirement = requirement.strip()
    if not requirement:
        return ""
    # Split off environment markers if present.
    marker_split = requirement.split(";", 1)
    req_part = marker_split[0]
    # Remove extras specification for the key.
    bracket_split = req_part.split("[", 1)
    name_part = bracket_split[0].strip()
    match = NAME_RE.match(name_part)
    if not match:
        raise SystemExit(
            f"Unable to determine requirement name for entry: '{requirement}'"
        )
    return match.group(0).lower()


def build_requirement_set(extras: dict[str, list[str]]) -> list[str]:
    seen: dict[str, str] = {}
    for group_name, deps in extras.items():
        for dep in deps:
            key = normalize_requirement(dep)
            if not key:
                continue
            if key in seen:
                # All duplicates should agree on the exact specifier; if not, fail fast.
                if seen[key] != dep:
                    raise SystemExit(
                        "Conflicting requirement specifications detected for "
                        f"'{key}': '{seen[key]}' vs '{dep}'."
                    )
            else:
                seen[key] = dep
    return sorted(seen.values(), key=lambda value: value.lower())


def render_requirements(requirements: list[str]) -> str:
    header = (
        "# This file is auto-generated from pyproject.toml optional dependencies.\n"
        "# Do not edit by hand; run `python tools/sync_requirements.py`.\n\n"
    )
    return header + "\n".join(requirements) + "\n"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="verify requirements.txt is up-to-date without modifying it",
    )
    args = parser.parse_args(argv)

    extras = load_optional_dependencies()
    requirements = build_requirement_set(extras)
    content = render_requirements(requirements)

    if args.check:
        current = (
            REQUIREMENTS.read_text(encoding="utf-8") if REQUIREMENTS.exists() else ""
        )
        if current != content:
            print(
                "requirements.txt is out of sync with pyproject.toml optional dependencies."
            )
            print("Run `python tools/sync_requirements.py` to regenerate the file.")
            return 1
        print("requirements.txt is up to date.")
        return 0

    REQUIREMENTS.write_text(content, encoding="utf-8")
    print("requirements.txt regenerated.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
