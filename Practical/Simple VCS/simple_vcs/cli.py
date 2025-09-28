"""Command line interface for the simple VCS."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Iterable, List

from .repository import Repository


def _parse_files(values: Iterable[str], root: Path) -> List[Path]:
    return [root / value for value in values]


def _cmd_init(args: argparse.Namespace) -> None:
    repo = Repository(Path(args.path))
    repo.init(force=args.force)
    print(f"Initialised repository at {repo.config_dir}")


def _cmd_commit(args: argparse.Namespace) -> None:
    repo = Repository(Path(args.path))
    files = _parse_files(args.files, Path(args.path))
    revisions = repo.commit(files, message=args.message)
    for path, revision in zip(args.files, revisions):
        print(f"Committed {path} as {revision.revision_id}: {revision.message}")


def _cmd_checkout(args: argparse.Namespace) -> None:
    repo = Repository(Path(args.path))
    revision = repo.checkout(Path(args.path) / args.file, revision_id=args.revision)
    print(f"Checked out {args.file} revision {revision.revision_id}")


def _cmd_config(args: argparse.Namespace) -> None:
    repo = Repository(Path(args.path))
    file_arg = getattr(args, "file", None)
    target = Path(args.path) / file_arg if file_arg else None
    if args.action == "set-limit":
        repo.set_revision_limit(target, args.limit)  # type: ignore[arg-type]
        print(f"Updated revision limit for {args.file} to {args.limit}")
    elif args.action == "lock":
        repo.lock_file(target)  # type: ignore[arg-type]
        print(f"Locked {args.file}")
    elif args.action == "unlock":
        repo.unlock_file(target)  # type: ignore[arg-type]
        print(f"Unlocked {args.file}")
    elif args.action == "show":
        status = repo.show_status()
        data = {
            path: {
                "revision_limit": entry.revision_limit,
                "locked": entry.locked,
                "revisions": [rev.to_dict() for rev in entry.revisions],
            }
            for path, entry in status.items()
        }
        print(json.dumps(data, indent=2, sort_keys=True))
    else:  # pragma: no cover - argparse should prevent this
        raise ValueError(f"Unknown config action: {args.action}")


def _cmd_diff(args: argparse.Namespace) -> None:
    repo = Repository(Path(args.path))
    diff_output = repo.diff(Path(args.path) / args.file, revision_id=args.revision)
    if diff_output:
        print(diff_output)
    else:
        print("No differences found.")


def _cmd_log(args: argparse.Namespace) -> None:
    repo = Repository(Path(args.path))
    revisions = repo.list_revisions(Path(args.path) / args.file)
    if not revisions:
        print("No revisions recorded yet.")
        return
    for revision in revisions:
        print(f"{revision.revision_id}\t{revision.timestamp:.0f}\t{revision.message}")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Simple file-based version control tool"
    )
    parser.add_argument(
        "--path",
        default=".",
        help="Path to the repository root (defaults to current directory)",
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    # init
    parser_init = subparsers.add_parser("init", help="Initialise a new repository")
    parser_init.add_argument(
        "--force", action="store_true", help="Overwrite existing repository"
    )
    parser_init.set_defaults(func=_cmd_init)

    # commit
    parser_commit = subparsers.add_parser("commit", help="Commit one or more files")
    parser_commit.add_argument("-m", "--message", required=True, help="Commit message")
    parser_commit.add_argument("files", nargs="+", help="Files to commit")
    parser_commit.set_defaults(func=_cmd_commit)

    # checkout
    parser_checkout = subparsers.add_parser(
        "checkout", help="Restore a committed revision"
    )
    parser_checkout.add_argument("file", help="File to restore")
    parser_checkout.add_argument("--revision", help="Specific revision identifier")
    parser_checkout.set_defaults(func=_cmd_checkout)

    # config
    parser_config = subparsers.add_parser(
        "config", help="Inspect or modify repository configuration"
    )
    config_subparsers = parser_config.add_subparsers(dest="action", required=True)

    parser_config_show = config_subparsers.add_parser(
        "show", help="Display tracked files"
    )
    parser_config_show.set_defaults(func=_cmd_config)

    parser_config_limit = config_subparsers.add_parser(
        "set-limit", help="Set revision limit for a file"
    )
    parser_config_limit.add_argument("file", help="File to update")
    parser_config_limit.add_argument(
        "limit", type=int, help="Number of revisions to keep"
    )
    parser_config_limit.set_defaults(func=_cmd_config)

    parser_config_lock = config_subparsers.add_parser(
        "lock", help="Lock a file against commits"
    )
    parser_config_lock.add_argument("file", help="File to lock")
    parser_config_lock.set_defaults(func=_cmd_config)

    parser_config_unlock = config_subparsers.add_parser("unlock", help="Unlock a file")
    parser_config_unlock.add_argument("file", help="File to unlock")
    parser_config_unlock.set_defaults(func=_cmd_config)

    # diff
    parser_diff = subparsers.add_parser(
        "diff", help="Show differences from a committed revision"
    )
    parser_diff.add_argument("file", help="File to compare")
    parser_diff.add_argument("--revision", help="Specific revision identifier")
    parser_diff.set_defaults(func=_cmd_diff)

    # log
    parser_log = subparsers.add_parser("log", help="List revisions for a file")
    parser_log.add_argument("file", help="File to inspect")
    parser_log.set_defaults(func=_cmd_log)

    return parser


def main(argv: Iterable[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    try:
        args.func(args)
    except Exception as exc:  # pragma: no cover - CLI convenience
        print(f"Error: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())
