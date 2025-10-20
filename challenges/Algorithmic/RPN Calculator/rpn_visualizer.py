"""Interactive stack visualizer for the enhanced RPN calculator.

The module exposes a ``capture_stack_snapshots`` helper that hooks into
``evaluate_rpn`` and records the operand stack after each processed token.
It also provides a small CLI utility capable of animating the stack evolution
as ASCII bar charts or textual tables and exporting/importing the snapshot log
as JSON.
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional, Sequence, Tuple

from postifx_evaluator import EvalConfig, RPNError, evaluate_rpn

Number = float


@dataclass
class StackSnapshot:
    """Token and stack state captured after one evaluation step."""

    token: str
    stack: List[Number] = field(default_factory=list)

    def __post_init__(self) -> None:
        # Store values as floats to ensure JSON serialisability and consistency.
        self.stack = [float(value) for value in self.stack]

    def as_dict(self) -> dict:
        return {"token": self.token, "stack": self.stack}

    @classmethod
    def from_dict(cls, payload: dict) -> "StackSnapshot":
        token = str(payload["token"])
        stack = [float(value) for value in payload.get("stack", [])]
        return cls(token=token, stack=stack)


SnapshotLog = List[StackSnapshot]


def capture_stack_snapshots(
    expression: str, *, config: Optional[EvalConfig] = None
) -> Tuple[SnapshotLog, Number]:
    """Evaluate *expression* while capturing stack snapshots."""

    snapshots: SnapshotLog = []

    def _hook(token: str, stack: List[Number]) -> None:
        snapshots.append(StackSnapshot(token=token, stack=stack))

    result = evaluate_rpn(expression, config=config, trace_hook=_hook)
    return snapshots, result


def snapshots_to_json(
    snapshots: SnapshotLog,
    *,
    expression: Optional[str] = None,
    result: Optional[Number] = None,
) -> dict:
    """Serialise captured snapshots into a JSON-compatible dictionary."""

    return {
        "expression": expression,
        "result": None if result is None else float(result),
        "frames": [snapshot.as_dict() for snapshot in snapshots],
    }


def snapshots_from_json(payload: dict) -> Tuple[Optional[str], Optional[Number], SnapshotLog]:
    """Recreate snapshots, expression and result from *payload*."""

    expression = payload.get("expression")
    result = payload.get("result")
    frames_payload = payload.get("frames", [])
    snapshots = [StackSnapshot.from_dict(frame) for frame in frames_payload]
    return expression, result if result is None else float(result), snapshots


def render_bar_chart(snapshot: StackSnapshot) -> str:
    values = snapshot.stack
    header = f"Token: {snapshot.token}"
    if not values:
        return f"{header}\n(empty stack)"

    max_val = max((abs(v) for v in values), default=1.0)
    scale = 20.0 / max_val if max_val else 1.0
    lines = [header]
    for idx, value in enumerate(values):
        length = int(round(abs(value) * scale))
        if length == 0:
            bar = "·"
        else:
            bar = "█" * length
        prefix = "-" if value < 0 else ""
        lines.append(f"{idx:>2}: {prefix}{bar} {value:g}")
    return "\n".join(lines)


def render_table(snapshot: StackSnapshot) -> str:
    header = f"Token: {snapshot.token}"
    if not snapshot.stack:
        return f"{header}\n└─ empty"

    lines = [header, "┌──────┬────────────┐", "│ index│ value      │", "├──────┼────────────┤"]
    for idx, value in enumerate(snapshot.stack):
        lines.append(f"│ {idx:>4} │ {value:>10g} │")
    lines.append("└──────┴────────────┘")
    return "\n".join(lines)


def _clear_screen(stream) -> None:
    if hasattr(stream, "isatty") and stream.isatty():
        stream.write("\033[2J\033[H")
    else:
        stream.write("\n" * 2)
    stream.flush()


def animate_snapshots(
    snapshots: SnapshotLog,
    *,
    mode: str = "bars",
    delay: float = 0.6,
    animate: bool = True,
    stream=sys.stdout,
) -> None:
    if not snapshots:
        stream.write("No snapshots captured.\n")
        stream.flush()
        return

    renderers = []
    if mode in {"bars", "both"}:
        renderers.append(render_bar_chart)
    if mode in {"table", "both"}:
        renderers.append(render_table)
    if not renderers:
        raise ValueError(f"Unsupported render mode: {mode}")

    for index, snapshot in enumerate(snapshots):
        block = "\n\n".join(renderer(snapshot) for renderer in renderers)
        if animate:
            _clear_screen(stream)
            stream.write(block + "\n")
            stream.flush()
            time.sleep(max(0.0, delay))
        else:
            if index:
                stream.write("\n" + "-" * 40 + "\n")
            stream.write(block + "\n")
    if animate:
        stream.write("\n")
        stream.flush()


def _load_expression_from_file(path: Path) -> str:
    try:
        return path.read_text(encoding="utf-8").strip()
    except OSError as exc:
        raise RPNError(f"Failed to read expression file: {exc}") from exc


def _resolve_expression(args: argparse.Namespace) -> str:
    sources = [bool(args.expr), bool(args.file), args.stdin]
    if sum(sources) > 1:
        raise RPNError("Specify only one of --expr/--file/--stdin")
    if sum(sources) == 0:
        raise RPNError("Provide an expression via --expr/--file/--stdin")
    if args.expr:
        return args.expr
    if args.file:
        return _load_expression_from_file(Path(args.file))
    if args.stdin:
        return sys.stdin.read().strip()
    raise RPNError("No expression source provided")


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Visualise RPN stack evolution")
    parser.add_argument("--expr", help="Evaluate and visualise this expression")
    parser.add_argument("--file", help="Read expression from file")
    parser.add_argument("--stdin", action="store_true", help="Read expression from stdin")
    parser.add_argument("--load", help="Load a previously exported JSON snapshot log")
    parser.add_argument("--export", help="Write the captured log to JSON")
    parser.add_argument(
        "--mode",
        choices=["bars", "table", "both"],
        default="bars",
        help="Rendering mode for the stack visualisation",
    )
    parser.add_argument("--delay", type=float, default=0.6, help="Delay between frames in seconds")
    parser.add_argument("--no-animate", action="store_true", help="Print frames without clearing the screen")
    parser.add_argument("--deg", action="store_true", dest="degrees", help="Interpret trig inputs as degrees")
    return parser


def _export_snapshots(path: Path, payload: dict) -> None:
    try:
        path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    except OSError as exc:
        raise RPNError(f"Failed to write export file: {exc}") from exc


def _load_snapshots(path: Path) -> Tuple[Optional[str], Optional[Number], SnapshotLog]:
    try:
        content = path.read_text(encoding="utf-8")
    except OSError as exc:
        raise RPNError(f"Failed to read snapshot log: {exc}") from exc
    try:
        payload = json.loads(content)
    except json.JSONDecodeError as exc:
        raise RPNError(f"Invalid JSON payload: {exc}") from exc
    return snapshots_from_json(payload)


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(argv)

    try:
        snapshots: SnapshotLog
        result: Optional[Number]
        expression: Optional[str]

        if args.load:
            expression, result, snapshots = _load_snapshots(Path(args.load))
        else:
            expression = _resolve_expression(args)
            config = EvalConfig(degrees=bool(args.degrees))
            snapshots, result = capture_stack_snapshots(expression, config=config)

        if args.export:
            payload = snapshots_to_json(snapshots, expression=expression, result=result)
            _export_snapshots(Path(args.export), payload)

        animate_snapshots(
            snapshots,
            mode=args.mode,
            delay=args.delay,
            animate=not args.no_animate,
            stream=sys.stdout,
        )

        if result is not None:
            print(f"Result: {result:g}")

        return 0
    except RPNError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":  # pragma: no cover - CLI entry point
    sys.exit(main())
