"""passgen_visualizer.py - Batch analytics and plotting for PassGen.

This utility wraps :mod:`passgen` to generate multiple batches of passwords,
collect per-character frequency statistics, estimate entropy, and optionally
render matplotlib visualisations (histogram, category pie chart, entropy
"gauge").  It mirrors the CLI of ``passgen.py`` so existing scripts can toggle
between raw generation and analytical runs with the same flags.

Example usage
-------------
Generate 5 passwords per batch for 10 batches and view plots:

    python passgen_visualizer.py --length 16 --letters --digits --count 5 --batches 10

Emit the summary JSON without opening any matplotlib windows (useful on CI):

    python passgen_visualizer.py --length 20 --letters --digits --symbols --json --no-plot

The JSON payload contains the character frequency table, normalised
probabilities, category breakdown, and entropy estimates so automated checks can
validate policy adherence.
"""

from __future__ import annotations

import argparse
import json
import math
import string
import sys
from collections import Counter, OrderedDict
from typing import Dict, Iterable, Mapping, Optional

import passgen


FULL_CHARSET = string.ascii_letters + string.digits + string.punctuation
BASELINE_POOL_SIZE = len(set(FULL_CHARSET))


def build_arg_parser() -> argparse.ArgumentParser:
    """Reuse the PassGen CLI while adding analytics controls."""

    parser = passgen.build_arg_parser()
    parser.description = (
        "Generate batches of passwords using passgen.py and collect frequency "
        "statistics. Optionally render matplotlib charts or export JSON summaries."
    )

    # The original --json option prints generated passwords. Here it emits the
    # analytics summary instead, so update the help text for clarity.
    for action in parser._actions:
        if action.dest == "json":
            action.help = "Output JSON analytics summary (frequency + entropy)"
            break

    parser.add_argument(
        "--batches",
        type=int,
        default=10,
        help="Number of batches to generate (each batch produces --count passwords)",
    )
    parser.add_argument(
        "--no-plot",
        action="store_true",
        help="Skip matplotlib visualisations (useful for headless environments)",
    )
    parser.add_argument(
        "--save-figure",
        type=str,
        default=None,
        metavar="PATH",
        help="Optional path to save the generated figure (PNG, PDF, etc.)",
    )
    return parser


def collect_passwords(spec: passgen.PasswordSpec, batches: int) -> Counter[str]:
    """Generate ``batches`` of passwords and tally character frequencies."""

    if batches <= 0:
        raise ValueError("Number of batches must be >= 1")

    frequencies: Counter[str] = Counter()
    for _ in range(batches):
        for password in passgen.generate_passwords(spec):
            frequencies.update(password)
    return frequencies


def category_breakdown(frequencies: Mapping[str, int]) -> Dict[str, int]:
    """Aggregate frequencies into letter/digit/symbol buckets."""

    letters = set(string.ascii_letters)
    digits = set(string.digits)
    symbols_total = 0
    letters_total = 0
    digits_total = 0

    for char, count in frequencies.items():
        if char in letters:
            letters_total += count
        elif char in digits:
            digits_total += count
        else:
            symbols_total += count

    return {
        "letters": letters_total,
        "digits": digits_total,
        "symbols": symbols_total,
    }


def summarize_generation(
    spec: passgen.PasswordSpec,
    batches: int,
) -> Dict[str, object]:
    """Produce a serialisable summary for analytics and testing."""

    spec.validate()
    pool = passgen.build_pool(spec)
    frequencies = collect_passwords(spec, batches)
    total_chars = sum(frequencies.values())

    ordered_freq = OrderedDict(sorted(frequencies.items(), key=lambda item: item[0]))
    probabilities = OrderedDict(
        (char, count / total_chars) for char, count in ordered_freq.items()
    )

    entropy_estimate = passgen.estimate_entropy_bits(len(pool), spec.length)
    baseline_entropy = spec.length * math.log2(BASELINE_POOL_SIZE)

    summary = {
        "spec": {
            "length": spec.length,
            "count": spec.count,
            "letters": spec.letters,
            "digits": spec.digits,
            "symbols": spec.symbols,
            "exclude_ambiguous": spec.exclude_ambiguous,
            "min_categories": spec.min_categories,
        },
        "batches": batches,
        "total_passwords": spec.count * batches,
        "total_characters": total_chars,
        "pool_size": len(pool),
        "estimated_entropy_bits": round(entropy_estimate, 4),
        "baseline_entropy_bits": round(baseline_entropy, 4),
        "character_frequencies": ordered_freq,
        "character_probabilities": OrderedDict(
            (char, round(prob, 6)) for char, prob in probabilities.items()
        ),
        "category_frequencies": category_breakdown(frequencies),
    }

    return summary


def ensure_matplotlib(headless: bool = False):
    """Import matplotlib lazily, forcing an Agg backend when headless."""

    try:
        import matplotlib

        if headless:
            matplotlib.use("Agg")
        from matplotlib import pyplot as plt  # type: ignore
    except Exception as exc:  # pragma: no cover - import guard
        raise RuntimeError(
            "matplotlib is required for plotting. Install it or pass --no-plot."
        ) from exc

    return plt


def plot_histogram(ax, frequencies: Mapping[str, int]) -> None:
    """Render a bar chart of per-character counts."""

    if not frequencies:
        ax.text(0.5, 0.5, "No data", ha="center", va="center", fontsize=12)
        ax.set_axis_off()
        return

    chars = list(frequencies.keys())
    counts = [frequencies[ch] for ch in chars]
    ax.bar(range(len(chars)), counts, color="tab:blue")
    ax.set_title("Character Frequency")
    ax.set_ylabel("Count")
    ax.set_xticks(range(len(chars)))
    ax.set_xticklabels(chars, rotation=90, fontsize=8)


def plot_category_pie(ax, categories: Mapping[str, int]) -> None:
    """Render a pie chart for category distribution."""

    total = sum(categories.values())
    if total == 0:
        ax.text(0.5, 0.5, "No data", ha="center", va="center", fontsize=12)
        ax.set_axis_off()
        return

    labels = []
    sizes = []
    for label, count in categories.items():
        if count > 0:
            labels.append(f"{label} ({count})")
            sizes.append(count)

    ax.pie(sizes, labels=labels, autopct="%1.1f%%", startangle=90)
    ax.set_title("Category Breakdown")


def plot_entropy_gauge(ax, summary: Mapping[str, object]) -> None:
    """Draw a simple horizontal gauge showing relative entropy."""

    est = float(summary["estimated_entropy_bits"])
    baseline = float(summary["baseline_entropy_bits"])
    ratio = 0.0 if baseline == 0 else min(est / baseline, 1.0)

    ax.barh([0], [1.0], color="#d0d0d0", height=0.3, edgecolor="black")
    ax.barh([0], [ratio], color="tab:green", height=0.3)
    ax.set_xlim(0, 1)
    ax.set_yticks([])
    ax.set_xlabel("Relative Entropy vs Full ASCII Pool")
    ax.set_title("Entropy Gauge")
    ax.text(
        ratio if ratio < 0.9 else 0.9,
        0,
        f"{est:.2f} bits",
        va="center",
        ha="left" if ratio < 0.9 else "right",
        color="black",
        fontsize=10,
    )


def render_plots(
    summary: Mapping[str, object], save_path: Optional[str], show: bool
) -> None:
    """Render matplotlib visualisations based on the summary."""

    plt = ensure_matplotlib(headless=not show and save_path is not None)
    fig, axes = plt.subplots(2, 2, figsize=(12, 8))
    ax_hist, ax_pie, ax_gauge = axes[0, 0], axes[0, 1], axes[1, 0]

    # Hide the unused subplot
    axes[1, 1].set_axis_off()

    frequencies = summary["character_frequencies"]
    categories = summary["category_frequencies"]

    plot_histogram(ax_hist, frequencies)
    plot_category_pie(ax_pie, categories)
    plot_entropy_gauge(ax_gauge, summary)

    fig.tight_layout()

    if save_path:
        fig.savefig(save_path)
    if show:
        plt.show()
    plt.close(fig)


def summary_to_json(summary: Mapping[str, object], indent: int = 2) -> str:
    """Serialise the analytics summary to a JSON string."""

    def convert(value):
        if isinstance(value, OrderedDict):
            return {k: convert(v) for k, v in value.items()}
        if isinstance(value, dict):
            return {k: convert(v) for k, v in value.items()}
        return value

    serialisable = convert(dict(summary))
    return json.dumps(serialisable, indent=indent)


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)

    any_flag = args.letters or args.digits or args.symbols
    spec = passgen.PasswordSpec(
        length=args.length,
        letters=args.letters if any_flag else True,
        digits=args.digits if any_flag else True,
        symbols=args.symbols,
        count=args.count,
        exclude_ambiguous=args.no_ambiguous,
        json_output=args.json,
        min_categories=args.min_categories,
    )

    try:
        summary = summarize_generation(spec, args.batches)
    except Exception as exc:  # pragma: no cover - CLI level error handling
        print(f"Error: {exc}", file=sys.stderr)
        return 1

    if args.json:
        print(summary_to_json(summary))

    if not args.no_plot or args.save_figure:
        show_plots = not args.no_plot
        try:
            render_plots(summary, args.save_figure, show=show_plots)
        except RuntimeError as exc:  # pragma: no cover - matplotlib missing
            print(str(exc), file=sys.stderr)
            if show_plots:
                return 1

    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
