"""Command line interface for the educational relational database."""

from __future__ import annotations

import sys
from typing import Any, Dict, Iterable, List

from .executor import SQLExecutor

PROMPT = "db> "
CONTINUATION_PROMPT = "... "


def _format_result(result: Dict[str, Any]) -> str:
    rtype = result.get("type")
    if rtype == "message":
        return result.get("message", "")
    if rtype == "rowcount":
        count = result.get("count", 0)
        return f"{count} row(s) affected"
    if rtype == "result_set":
        columns: List[str] = result.get("columns", [])
        rows: Iterable[Iterable[Any]] = result.get("rows", [])
        if not rows:
            return "(no rows)"
        widths = [len(col) for col in columns]
        rows = list(rows)
        for row in rows:
            for idx, value in enumerate(row):
                widths[idx] = max(widths[idx], len(str(value)))
        header = " | ".join(col.ljust(widths[idx]) for idx, col in enumerate(columns))
        separator = "-+-".join("-" * widths[idx] for idx in range(len(columns)))
        body_lines = []
        for row in rows:
            body_lines.append(
                " | ".join(str(value).ljust(widths[idx]) for idx, value in enumerate(row))
            )
        return "\n".join([header, separator, *body_lines])
    return str(result)


def repl(executor: SQLExecutor | None = None) -> None:
    executor = executor or SQLExecutor()
    buffer = ""
    try:
        while True:
            prompt = PROMPT if not buffer else CONTINUATION_PROMPT
            try:
                line = input(prompt)
            except EOFError:
                print()
                break
            command = line.strip()
            if not command and not buffer:
                continue
            if command.startswith(".") and not buffer:
                if command in {".quit", ".exit"}:
                    break
                if command == ".tables":
                    tables = executor.database.list_tables()
                    print(", ".join(sorted(tables)) if tables else "(no tables)")
                else:
                    print("Unknown command. Available commands: .tables, .quit")
                continue
            buffer = f"{buffer} {line}".strip()
            if ";" not in buffer:
                continue
            try:
                results = executor.execute(buffer)
                for result in results:
                    output = _format_result(result)
                    if output:
                        print(output)
            except Exception as exc:  # pragma: no cover - interactive error path
                print(f"Error: {exc}")
            finally:
                buffer = ""
    except KeyboardInterrupt:  # pragma: no cover - interactive convenience
        print("\nInterrupted")
    finally:
        pass


def main(argv: List[str] | None = None) -> int:
    repl()
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main(sys.argv[1:]))
