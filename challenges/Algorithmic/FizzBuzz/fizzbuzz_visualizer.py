"""Visual tooling for inspecting FizzBuzz rule coverage.

This module builds on :mod:`fizzbuzz` by consuming the streaming generator and
tracking which rules fired for each integer.  The collected metadata can then
be rendered as a stacked bar chart to quickly spot coverage gaps when tweaking
rules.

The module is intentionally import-friendly for unit tests as well as usable as
an interactive CLI utility::

    python fizzbuzz_visualizer.py -n 30 \
        --rule 3:Fizz --rule 5:Buzz --rule 7:Pop \
        --output fizzbuzz.png

The CLI mirrors the key options from ``fizzbuzz.py`` (limit selection,
custom rules, and number suppression) and adds switches for exporting a PNG and
the raw metadata.
"""

from __future__ import annotations

import argparse
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Iterator, List, Sequence, TYPE_CHECKING

try:  # pragma: no cover - optional dependency for headless environments
    import matplotlib.pyplot as plt
except ModuleNotFoundError:  # pragma: no cover - optional dependency for headless environments
    plt = None  # type: ignore[assignment]

if TYPE_CHECKING:  # pragma: no cover - typing helper
    from matplotlib.figure import Figure

import fizzbuzz


@dataclass(frozen=True)
class FizzBuzzVisualizationEntry:
    """Container describing the outcome of processing a single integer."""

    value: int
    output: str | None
    applied_rules: tuple[str, ...]
    emitted: bool


def _consume_stream(
    limit: int,
    rules: Sequence[fizzbuzz.FizzBuzzRule],
    include_numbers: bool,
) -> Iterator[str]:
    """Return an iterator over the fizzbuzz stream with validation."""

    stream = fizzbuzz.fizzbuzz_stream(limit, rules, include_numbers=include_numbers)
    return iter(stream)


def generate_rule_metadata(
    limit: int,
    rules: Sequence[fizzbuzz.FizzBuzzRule] | None = None,
    *,
    include_numbers: bool = True,
) -> List[FizzBuzzVisualizationEntry]:
    """Capture rule metadata for integers ``1..limit``.

    The function consumes :func:`fizzbuzz.fizzbuzz_stream` to guarantee parity
    with the plain textual output while additionally tracking which rules were
    triggered for each integer.
    """

    if limit < 1:
        raise ValueError("limit must be >= 1")

    chosen_rules: Sequence[fizzbuzz.FizzBuzzRule] = (
        tuple(rules) if rules is not None else fizzbuzz.DEFAULT_RULES
    )

    stream_iter = _consume_stream(limit, chosen_rules, include_numbers)
    entries: List[FizzBuzzVisualizationEntry] = []

    for value in range(1, limit + 1):
        applied = tuple(
            rule.label for rule in chosen_rules if value % rule.divisor == 0
        )
        emitted = bool(applied or include_numbers)
        output: str | None = None

        if emitted:
            try:
                output = next(stream_iter)
            except StopIteration as exc:  # pragma: no cover - defensive
                raise RuntimeError(
                    "fizzbuzz_stream terminated before yielding expected values"
                ) from exc

        entries.append(
            FizzBuzzVisualizationEntry(
                value=value, output=output, applied_rules=applied, emitted=emitted
            )
        )

    # Ensure the underlying generator is exhausted (defensive programming).
    try:  # pragma: no cover - defensive
        next(stream_iter)
    except StopIteration:
        pass
    else:  # pragma: no cover - defensive
        raise RuntimeError("fizzbuzz_stream yielded more items than expected")

    return entries


def _select_xticks(values: List[int]) -> List[int]:
    """Down-sample tick labels to a reasonable amount for large ranges."""

    if len(values) <= 20:
        return values

    step = max(1, len(values) // 20)
    return values[::step]


def render_stacked_bar_chart(
    entries: Sequence[FizzBuzzVisualizationEntry],
    *,
    include_numbers: bool,
    figsize: tuple[float, float] | None = None,
    dpi: int | None = None,
) -> "Figure":
    """Render a stacked bar chart summarising rule coverage.

    ``entries`` should be produced by :func:`generate_rule_metadata`.
    """

    if plt is None:  # pragma: no cover - requires optional dependency
        raise RuntimeError("Matplotlib is required to render visualisations.")

    emitted_entries = [entry for entry in entries if entry.emitted]

    if not emitted_entries:
        raise ValueError("No values to render – did every number get suppressed?")

    # Preserve the order of rules based on their first appearance.
    seen_labels: List[str] = []
    for entry in emitted_entries:
        for label in entry.applied_rules:
            if label not in seen_labels:
                seen_labels.append(label)

    values = [entry.value for entry in emitted_entries]

    fig, ax = plt.subplots(figsize=figsize, dpi=dpi)
    ax.set_title(f"FizzBuzz rule coverage up to {max(values)}")
    ax.set_xlabel("Value")
    ax.set_ylabel("Rule layers")

    color_cycle = plt.get_cmap("tab20")(range(20))
    color_map = {label: color_cycle[idx % 20] for idx, label in enumerate(seen_labels)}

    bottoms = [0] * len(emitted_entries)

    for label in seen_labels:
        heights = [1 if label in entry.applied_rules else 0 for entry in emitted_entries]
        if any(heights):
            ax.bar(
                values,
                heights,
                bottom=bottoms,
                width=0.8,
                label=label,
                color=color_map[label],
                edgecolor="black",
                linewidth=0.2,
            )
            bottoms = [b + h for b, h in zip(bottoms, heights)]

    if include_numbers:
        numbers_heights = [1 if not entry.applied_rules else 0 for entry in emitted_entries]
        if any(numbers_heights):
            ax.bar(
                values,
                numbers_heights,
                bottom=bottoms,
                width=0.8,
                label="Number",
                color="#d3d3d3",
                edgecolor="black",
                linewidth=0.2,
            )

    ax.set_xticks(_select_xticks(values))
    ax.legend(loc="upper right", frameon=True)
    fig.tight_layout()
    return fig


def _export_metadata(entries: Iterable[FizzBuzzVisualizationEntry], path: Path) -> None:
    serialisable = [
        {
            "value": entry.value,
            "output": entry.output,
            "applied_rules": list(entry.applied_rules),
            "emitted": entry.emitted,
        }
        for entry in entries
    ]
    path.write_text(json.dumps(serialisable, indent=2), encoding="utf-8")


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Visualise FizzBuzz rule coverage as a stacked bar chart.",
    )
    parser.add_argument(
        "-n",
        "--limit",
        type=int,
        default=100,
        help="Upper bound (inclusive). Default: %(default)s",
    )
    parser.add_argument(
        "--rule",
        action="append",
        type=fizzbuzz.parse_rule,
        help="Custom rule DIVISOR:LABEL. Overrides defaults when provided.",
    )
    parser.add_argument(
        "--no-numbers",
        action="store_true",
        help="Suppress raw numbers when no rule matches.",
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="Optional path to save the rendered figure as a PNG.",
    )
    parser.add_argument(
        "--metadata",
        type=Path,
        help="Optional path to export the gathered metadata as JSON.",
    )
    parser.add_argument(
        "--width",
        type=float,
        help="Figure width in inches (matplotlib).",
    )
    parser.add_argument(
        "--height",
        type=float,
        help="Figure height in inches (matplotlib).",
    )
    parser.add_argument(
        "--dpi",
        type=int,
        help="Dots-per-inch for the exported figure.",
    )
    parser.add_argument(
        "--show",
        action="store_true",
        help="Display the figure in an interactive window after rendering.",
    )
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = _build_parser()
    args = parser.parse_args(argv)

    rules: Sequence[fizzbuzz.FizzBuzzRule] | None = tuple(args.rule) if args.rule else None

    try:
        entries = generate_rule_metadata(
            args.limit,
            rules=rules,
            include_numbers=not args.no_numbers,
        )
    except ValueError as exc:
        parser.error(str(exc))

    try:
        fig = render_stacked_bar_chart(
            entries,
            include_numbers=not args.no_numbers,
            figsize=(args.width, args.height) if args.width and args.height else None,
            dpi=args.dpi,
        )
    except RuntimeError as exc:
        parser.error(str(exc))
        raise AssertionError  # pragma: no cover - parser.error exits

    if args.output:
        fig.savefig(args.output, bbox_inches="tight")

    if args.metadata:
        _export_metadata(entries, args.metadata)

    if args.show:
        if plt is None:  # pragma: no cover - requires optional dependency
            parser.error("Matplotlib is not available for interactive display.")
        else:
            plt.show()

    if plt is not None:
        plt.close(fig)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
