"""rnp.py - Random name picker with modern CLI & features.

Enhancements over the original script:
* Dataclass configuration object
* argparse CLI with JSON output option
* Optional weighted selection via "name,weight" lines
* Support picking with or without replacement
* Reproducibility via --seed
* Automatic default file creation
* Clean separation of parsing, selection, and I/O for testability

File Format
===========
Each non-empty line is either:
  Name
  Name,weight   (weight must be a positive number)

Examples:
  Alice,3
  Bob,1
  Charlie

CLI Examples
------------
Pick 3 unique names:
    python rnp.py -c 3

Allow repeats (with replacement):
    python rnp.py -c 5 --with-replacement

Weighted selection (if weights in file) and JSON output:
    python rnp.py -c 4 --json

Deterministic run:
    python rnp.py -c 4 --seed 42
"""

from __future__ import annotations

import argparse
import json
import os
import random
import sys
from dataclasses import dataclass
from typing import Iterable, List, Optional, Sequence, Tuple


# ---------------------------- Data Model ---------------------------- #
@dataclass(slots=True)
class NamePickerConfig:
    path: str = "names.txt"
    count: int = 1
    with_replacement: bool = False
    json_output: bool = False
    seed: Optional[int] = None
    create_default: bool = True

    def validate(self, total_names: int) -> None:
        if self.count <= 0:
            raise ValueError("Count must be positive")
        if not self.with_replacement and self.count > total_names:
            raise ValueError(
                f"Cannot pick {self.count} unique names from {total_names}"
            )


DEFAULT_NAMES = [
    "Alice",
    "Bob",
    "Charlie",
    "David",
    "Eve",
    "Frank",
    "Grace",
    "Heidi",
    "Ivan",
    "Judy",
]


def ensure_default_file(path: str) -> None:
    if os.path.exists(path):
        return
    try:
        with open(path, "w", encoding="utf-8") as f:
            for name in DEFAULT_NAMES:
                f.write(name + "\n")
        print(f"Created default name file at '{path}'")
    except OSError as e:  # pragma: no cover - unlikely
        print(f"Error creating default file: {e}", file=sys.stderr)
        sys.exit(1)


def parse_names_file(path: str) -> Tuple[List[str], Optional[List[float]]]:
    """Parse names (and optional weights) from file.

    Returns (names, weights) where weights may be None if no weights present.
    If any line has a weight, missing weights default to 1.0.
    """
    names: List[str] = []
    weights: List[float] = []
    saw_weight = False
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            if "," in line:
                name, w_str = [part.strip() for part in line.split(",", 1)]
                try:
                    w = float(w_str)
                    if w <= 0:
                        raise ValueError
                except ValueError:
                    print(
                        f"Warning: invalid weight '{w_str}' for name '{name}' – skipping line",
                        file=sys.stderr,
                    )
                    continue
                names.append(name)
                weights.append(w)
                saw_weight = True
            else:
                names.append(line)
                weights.append(1.0)
    if not names:
        raise ValueError("Name file is empty")
    if not saw_weight:
        return names, None
    return names, weights


def pick_names(
    names: Sequence[str],
    count: int,
    *,
    with_replacement: bool,
    weights: Optional[Sequence[float]],
    rng: random.Random,
) -> List[str]:
    if with_replacement:
        # random.choices uses weights for replacement scenario
        return rng.choices(names, weights=weights, k=count)
    # Without replacement: use sample; weights necessitate manual approach.
    if weights is None:
        return rng.sample(list(names), count)
    # Weighted without replacement: perform sequential weighted selection.
    chosen: List[str] = []
    names_mut = list(names)
    weights_mut = list(weights)
    for _ in range(count):
        total = sum(weights_mut)
        r = rng.random() * total
        acc = 0.0
        for i, w in enumerate(weights_mut):
            acc += w
            if r <= acc:
                chosen.append(names_mut.pop(i))
                weights_mut.pop(i)
                break
    return chosen


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Random name picker with optional weighting"
    )
    p.add_argument("-c", "--count", type=int, default=1, help="Number of names to pick")
    p.add_argument(
        "-f", "--file", dest="path", default="names.txt", help="Path to names file"
    )
    p.add_argument(
        "--with-replacement",
        action="store_true",
        help="Allow picking the same name multiple times",
    )
    p.add_argument(
        "--json", action="store_true", help="Output JSON instead of plain text"
    )
    p.add_argument("--seed", type=int, help="Seed RNG for reproducibility")
    p.add_argument(
        "--no-create-default",
        action="store_true",
        help="Do not auto-create default file if missing",
    )
    return p


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    cfg = NamePickerConfig(
        path=args.path,
        count=args.count,
        with_replacement=args.with_replacement,
        json_output=args.json,
        seed=args.seed,
        create_default=not args.no_create_default,
    )

    if cfg.create_default:
        ensure_default_file(cfg.path)
    if not os.path.exists(cfg.path):
        print(f"Error: Name file '{cfg.path}' not found", file=sys.stderr)
        return 1

    try:
        names, weights = parse_names_file(cfg.path)
        cfg.validate(len(names))
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    rng = random.Random(cfg.seed)
    picked = pick_names(
        names,
        cfg.count,
        with_replacement=cfg.with_replacement,
        weights=weights,
        rng=rng,
    )

    if cfg.json_output:
        payload = {
            "picked": picked,
            "count": cfg.count,
            "with_replacement": cfg.with_replacement,
            "seed": cfg.seed,
            "total_names": len(names),
            "weights_used": weights is not None,
        }
        print(json.dumps(payload, indent=2))
    else:
        print(f"Picked {len(picked)} name(s):")
        for name in picked:
            print(f"  - {name}")
        if cfg.seed is not None:
            print(f"(Deterministic with seed {cfg.seed})")
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
