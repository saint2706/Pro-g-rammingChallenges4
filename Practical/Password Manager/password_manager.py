"""Command line interface for the Practical Password Manager."""

from __future__ import annotations

import argparse
import csv
import json
import sys
from getpass import getpass
from pathlib import Path
from typing import Iterable, List, Optional

from vault import PasswordVault, generate_password


def prompt_password(confirm: bool = False) -> str:
    while True:
        password = getpass("Master password: ")
        if confirm:
            confirm_pw = getpass("Confirm password: ")
            if password != confirm_pw:
                print("Passwords do not match. Try again.")
                continue
        if not password:
            print("Password cannot be empty.")
            continue
        return password


def load_vault(path: Path, password: Optional[str]) -> PasswordVault:
    if password is None:
        password = prompt_password()
    return PasswordVault.load(path, password)


def cmd_init(args: argparse.Namespace) -> None:
    path = Path(args.vault)
    if path.exists() and not args.overwrite:
        print(f"Vault already exists at {path}. Use --overwrite to replace.")
        return
    if path.exists() and args.overwrite:
        path.unlink()

    password = args.password or prompt_password(confirm=True)
    vault = PasswordVault.create(path, password)
    print(f"Created vault at {vault._path}")


def _print_entries(entries: Iterable) -> None:
    entries = list(entries)
    if not entries:
        print("No entries found.")
        return
    width_name = max(len(e.name) for e in entries)
    width_user = max(len(e.username) for e in entries)
    width_cat = max(len(e.category) for e in entries)
    header = f"{'ID':36}  {'Name'.ljust(width_name)}  {'Username'.ljust(width_user)}  {'Category'.ljust(width_cat)}"
    print(header)
    print("-" * len(header))
    for e in entries:
        print(
            f"{e.entry_id}  {e.name.ljust(width_name)}  {e.username.ljust(width_user)}  {e.category.ljust(width_cat)}"
        )


def cmd_list(args: argparse.Namespace) -> None:
    vault = load_vault(Path(args.vault), args.password)
    entries = vault.list_entries(category=args.category)
    _print_entries(entries)


def cmd_show(args: argparse.Namespace) -> None:
    vault = load_vault(Path(args.vault), args.password)
    try:
        entry = vault.get_entry(args.entry_id)
    except KeyError as exc:
        print(exc)
        return
    print(json.dumps(entry.to_dict(), indent=2, ensure_ascii=False))


def cmd_add(args: argparse.Namespace) -> None:
    vault = load_vault(Path(args.vault), args.password)
    password = args.entry_password
    generated = False
    if not password:
        password = generate_password(length=args.length)
        generated = True
    entry = vault.add_entry(
        name=args.name,
        username=args.username,
        password=password,
        category=args.category,
        notes=args.notes or "",
    )
    print(f"Added entry {entry.entry_id} ({entry.name})")
    if generated:
        print("Generated password:", password)


def cmd_update(args: argparse.Namespace) -> None:
    vault = load_vault(Path(args.vault), args.password)
    updates = {
        "name": args.name,
        "username": args.username,
        "password": args.entry_password,
        "category": args.category,
        "notes": args.notes,
    }
    try:
        entry = vault.update_entry(args.entry_id, **updates)
    except KeyError as exc:
        print(exc)
        return
    print(f"Updated entry {entry.entry_id}")


def cmd_delete(args: argparse.Namespace) -> None:
    vault = load_vault(Path(args.vault), args.password)
    try:
        vault.delete_entry(args.entry_id)
    except KeyError as exc:
        print(exc)
        return
    print(f"Deleted entry {args.entry_id}")


def cmd_generate(args: argparse.Namespace) -> None:
    password = generate_password(
        length=args.length,
        use_upper=not args.no_upper,
        use_digits=not args.no_digits,
        use_symbols=not args.no_symbols,
    )
    print(password)


def _read_json(path: Path) -> List[dict]:
    return json.loads(path.read_text("utf-8"))


def _read_csv(path: Path) -> List[dict]:
    with path.open("r", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        return list(reader)


def cmd_export(args: argparse.Namespace) -> None:
    vault = load_vault(Path(args.vault), args.password)
    entries = vault.export_entries()
    path = Path(args.output)
    fmt = args.format or path.suffix.lstrip(".").lower()
    if fmt == "json":
        path.write_text(
            json.dumps(entries, indent=2, ensure_ascii=False), encoding="utf-8"
        )
    elif fmt == "csv":
        fieldnames = ["name", "username", "password", "category", "notes"]
        with path.open("w", encoding="utf-8", newline="") as fh:
            writer = csv.DictWriter(fh, fieldnames=fieldnames)
            writer.writeheader()
            for entry in entries:
                writer.writerow({key: entry.get(key, "") for key in fieldnames})
    else:
        print(f"Unsupported format: {fmt}")
        return
    print(f"Exported {len(entries)} entries to {path}")


def cmd_import(args: argparse.Namespace) -> None:
    vault = load_vault(Path(args.vault), args.password)
    path = Path(args.input)
    fmt = args.format or path.suffix.lstrip(".").lower()
    if fmt == "json":
        entries = _read_json(path)
    elif fmt == "csv":
        entries = _read_csv(path)
    else:
        print(f"Unsupported format: {fmt}")
        return
    count = vault.import_entries(entries, replace=args.replace, source=str(path))
    print(f"Imported {count} entries from {path}")


def cmd_audit(args: argparse.Namespace) -> None:
    vault = load_vault(Path(args.vault), args.password)
    events = vault.audit_log(limit=args.limit)
    if args.reverse:
        events = list(reversed(events))
    for event in events:
        print(
            f"{event.timestamp} | {event.action:<6} | {event.entry_id or '-'} | {event.detail}"
        )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Password Manager CLI")
    parser.add_argument(
        "--vault", default="vault.pm", help="Path to the encrypted vault"
    )
    parser.add_argument(
        "--password", help="Master password (discouraged; prefer prompt)"
    )
    sub = parser.add_subparsers(dest="command", required=True)

    p_init = sub.add_parser("init", help="Create a new vault")
    p_init.add_argument(
        "--overwrite", action="store_true", help="Overwrite existing vault"
    )
    p_init.add_argument("--password", help="Provide master password via CLI")
    p_init.set_defaults(func=cmd_init)

    p_list = sub.add_parser("list", help="List entries")
    p_list.add_argument("--category", help="Filter by category")
    p_list.set_defaults(func=cmd_list)

    p_show = sub.add_parser("show", help="Show entry details")
    p_show.add_argument("entry_id")
    p_show.set_defaults(func=cmd_show)

    p_add = sub.add_parser("add", help="Add a new entry")
    p_add.add_argument("name")
    p_add.add_argument("username")
    p_add.add_argument(
        "--entry-password", dest="entry_password", help="Password for the entry"
    )
    p_add.add_argument(
        "--length",
        type=int,
        default=16,
        help="Generated password length if not provided",
    )
    p_add.add_argument("--category", default="general")
    p_add.add_argument("--notes")
    p_add.set_defaults(func=cmd_add)

    p_update = sub.add_parser("update", help="Update an entry")
    p_update.add_argument("entry_id")
    p_update.add_argument("--name")
    p_update.add_argument("--username")
    p_update.add_argument("--entry-password", dest="entry_password")
    p_update.add_argument("--category")
    p_update.add_argument("--notes")
    p_update.set_defaults(func=cmd_update)

    p_delete = sub.add_parser("delete", help="Delete an entry")
    p_delete.add_argument("entry_id")
    p_delete.set_defaults(func=cmd_delete)

    p_gen = sub.add_parser("generate", help="Generate a password")
    p_gen.add_argument("--length", type=int, default=16)
    p_gen.add_argument("--no-upper", action="store_true")
    p_gen.add_argument("--no-digits", action="store_true")
    p_gen.add_argument("--no-symbols", action="store_true")
    p_gen.set_defaults(func=cmd_generate)

    p_export = sub.add_parser("export", help="Export entries")
    p_export.add_argument("--output", required=True)
    p_export.add_argument("--format", choices=["json", "csv"])
    p_export.set_defaults(func=cmd_export)

    p_import = sub.add_parser("import", help="Import entries")
    p_import.add_argument("--input", required=True)
    p_import.add_argument("--format", choices=["json", "csv"])
    p_import.add_argument(
        "--replace",
        action="store_true",
        help="Replace current entries with imported ones",
    )
    p_import.set_defaults(func=cmd_import)

    p_audit = sub.add_parser("audit", help="Show audit log")
    p_audit.add_argument("--limit", type=int)
    p_audit.add_argument("--reverse", action="store_true", help="Show newest first")
    p_audit.set_defaults(func=cmd_audit)

    return parser


def main(argv: Optional[List[str]] = None) -> None:
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        args.func(args)
    except ValueError as exc:
        print(exc)
        sys.exit(1)


if __name__ == "__main__":
    main()
