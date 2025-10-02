#!/usr/bin/env python3
"""Build and smoke-test native Producer/Consumer demos."""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Sequence

BASE_DEFAULT = Path("Practical/Producer Consumer")


@dataclass(frozen=True)
class Project:
    name: str
    source: Path
    language: str
    run_args: Sequence[str]
    main_class: str | None = None
    skip_ci: bool = False

    @property
    def id(self) -> str:
        return self.name


PROJECT_CATALOGUE: List[Project] = [
    Project(
        name="pc-c",
        source=BASE_DEFAULT / "pc.c",
        language="c",
        run_args=["-h"],
    ),
    Project(
        name="pc-cpp",
        source=BASE_DEFAULT / "pc.cpp",
        language="c++",
        run_args=["-n", "1", "-P", "1", "-C", "1", "-p", "0", "-q", "0", "-c", "2"],
    ),
    Project(
        name="pcLL-java",
        source=BASE_DEFAULT / "pcLL.java",
        language="java",
        run_args=[
            "quiet=true",
            "timestamps=false",
            "itemsPerProducer=1",
            "producers=1",
            "consumers=1",
            "prodDelayMs=0",
            "consDelayMs=0",
        ],
        main_class="pcLL",
    ),
    Project(
        name="pcSem-java",
        source=BASE_DEFAULT / "pcSem.java",
        language="java",
        run_args=[
            "quiet=true",
            "timestamps=false",
            "itemsPerProducer=1",
            "producers=1",
            "consumers=1",
            "prodDelayMs=0",
            "consDelayMs=0",
        ],
        main_class="pcSem",
    ),
]


def discover_projects(base: Path) -> List[Project]:
    projects: List[Project] = []
    for project in PROJECT_CATALOGUE:
        candidate = base / project.source.name
        if candidate.exists():
            projects.append(
                Project(
                    name=project.name,
                    source=candidate,
                    language=project.language,
                    run_args=project.run_args,
                    main_class=project.main_class,
                    skip_ci=project.skip_ci,
                )
            )
    return projects


def run(cmd: Sequence[str], *, cwd: Path | None = None) -> None:
    print(f"$ {' '.join(cmd)}")
    subprocess.run(cmd, check=True, cwd=str(cwd) if cwd else None)


def compile_c(project: Project, build_dir: Path) -> Path:
    output = build_dir / project.source.stem
    run(
        [
            "gcc",
            "-O2",
            "-Wall",
            "-Wextra",
            "-pthread",
            str(project.source),
            "-o",
            str(output),
        ]
    )
    return output


def compile_cpp(project: Project, build_dir: Path) -> Path:
    output = build_dir / project.source.stem
    run(
        [
            "g++",
            "-std=c++17",
            "-O2",
            "-Wall",
            "-Wextra",
            "-pthread",
            str(project.source),
            "-o",
            str(output),
        ]
    )
    return output


def compile_java(project: Project, build_dir: Path) -> Path:
    classes_dir = build_dir / "classes"
    classes_dir.mkdir(parents=True, exist_ok=True)
    run(["javac", "-d", str(classes_dir), str(project.source)])
    # Return the directory containing classes; the runner will know how to use it.
    return classes_dir


def smoke_test(project: Project, artifact: Path) -> None:
    if project.language in {"c", "c++"}:
        cmd = [str(artifact), *project.run_args]
    elif project.language == "java":
        if not project.main_class:
            raise RuntimeError(
                f"Java project {project.name} is missing main_class metadata"
            )
        cmd = [
            "java",
            "-cp",
            str(artifact),
            project.main_class,
            *project.run_args,
        ]
    else:
        raise ValueError(
            f"Unsupported language for project {project.name}: {project.language}"
        )
    run(cmd)


def parse_args(argv: Iterable[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--base",
        type=Path,
        default=BASE_DEFAULT,
        help="Root directory that contains the native projects",
    )
    parser.add_argument(
        "--only",
        nargs="*",
        help="Limit the run to the given project identifiers",
    )
    parser.add_argument(
        "--list",
        action="store_true",
        help="List discovered projects and exit",
    )
    parser.add_argument(
        "--keep-build",
        action="store_true",
        help="Keep the temporary build directory instead of deleting it",
    )
    parser.add_argument(
        "--ci",
        action="store_true",
        help="Enable CI mode (skips projects flagged as incompatible)",
    )
    return parser.parse_args(argv)


def main(argv: Iterable[str] | None = None) -> int:
    args = parse_args(argv or sys.argv[1:])
    base = args.base
    projects = discover_projects(base)
    if args.only:
        whitelist = set(args.only)
        projects = [p for p in projects if p.name in whitelist]
    if not projects:
        print(f"No native projects found under {base}")
        return 0
    if args.list:
        print("Discovered native projects:")
        for project in projects:
            print(f"- {project.name}: {project.source} [{project.language}]")
        return 0

    temp_dir_manager: tempfile.TemporaryDirectory[str] | None = None
    try:
        if args.keep_build:
            build_root = Path("build/native")
            build_root.mkdir(parents=True, exist_ok=True)
        else:
            temp_dir_manager = tempfile.TemporaryDirectory(prefix="native-build-")
            build_root = Path(temp_dir_manager.name)

        print(f"Using build root: {build_root}")

        for project in projects:
            if args.ci and project.skip_ci:
                print(f"Skipping {project.name} (marked as unsupported on CI)")
                continue
            print(f"\n=== Building {project.name} ({project.language}) ===")
            project_build_dir = build_root / project.name
            project_build_dir.mkdir(parents=True, exist_ok=True)

            if project.language == "c":
                artifact = compile_c(project, project_build_dir)
            elif project.language == "c++":
                artifact = compile_cpp(project, project_build_dir)
            elif project.language == "java":
                artifact = compile_java(project, project_build_dir)
            else:
                raise ValueError(f"Unsupported language: {project.language}")

            print(f"--- Running smoke test for {project.name} ---")
            smoke_test(project, artifact)
        print("\nAll native projects built and tested successfully.")
        return 0
    finally:
        if temp_dir_manager is not None:
            temp_dir_manager.cleanup()


if __name__ == "__main__":
    sys.exit(main())
