import argparse
import json
from pathlib import Path
from typing import List

import matplotlib.pyplot as plt


def load_metrics(path: Path) -> dict:
    data = json.loads(path.read_text())
    events = data.get("events", [])
    tag_times = [event["time"] for event in events]
    return {
        "path": path,
        "config": data.get("config", {}),
        "tag_times": tag_times,
        "tag_count": len(events),
        "duration": data.get("duration"),
        "tag_order": data.get("tag_order", []),
    }


def plot_runs(metrics: List[dict], output: Path) -> None:
    plt.style.use("seaborn-v0_8")
    fig, axes = plt.subplots(2, 1, figsize=(8, 8), constrained_layout=True)

    for metric in metrics:
        label = f"{metric['path'].stem} (tags={metric['tag_count']})"
        axes[0].plot(metric["tag_times"], marker="o", label=label)
        axes[1].bar(label, metric["duration"], label=label)

    axes[0].set_title("Tag timing per run")
    axes[0].set_xlabel("Tag index")
    axes[0].set_ylabel("Time (s)")
    axes[0].legend()

    axes[1].set_title("Total duration per run")
    axes[1].set_ylabel("Seconds")
    axes[1].tick_params(axis="x", rotation=30)

    fig.suptitle("Multi-Agent Tag metrics", fontsize=14)
    fig.savefig(output)
    print(f"Saved analysis chart to {output}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Plot metrics from multi-agent tag simulations")
    parser.add_argument("paths", type=Path, nargs="+", help="Recorded simulation JSON files")
    parser.add_argument("--output", type=Path, default=Path("analysis.png"), help="Output image path")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    metrics = [load_metrics(path) for path in args.paths]
    plot_runs(metrics, args.output)


if __name__ == "__main__":
    main()
