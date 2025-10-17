"""iris.py – Gaussian Naive Bayes demo with optional custom CSV input.

Modernizations:
  * Config dataclass & structured pipeline (load -> split -> train -> metrics -> report)
  * Deterministic behavior via --seed (default 42)
  * Optional JSON metrics export (--json FILE)
  * Confusion matrix printing & micro/macro averaging
  * Feature subset selection (--features col1,col2,...)
  * Graceful error handling with exit codes
  * Type hints & docstrings for educational clarity

Example:
  python iris.py                             # built-in Iris dataset
  python iris.py -f my.csv -t species --features sepal_length,sepal_width --json metrics.json
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import List, Optional, Tuple, Dict, Any

import pandas as pd

# Dependency guard
try:
    from sklearn.datasets import load_iris
    from sklearn.model_selection import train_test_split
    from sklearn.naive_bayes import GaussianNB
    from sklearn.metrics import (
        accuracy_score,
        classification_report,
        confusion_matrix,
        precision_recall_fscore_support,
    )
except ImportError as e:  # pragma: no cover
    lib = str(e).split("'")[1]
    print(
        f"Error: Missing dependency '{lib}'. Install with: pip install scikit-learn pandas",
        file=sys.stderr,
    )
    raise SystemExit(1)


# ---------------------------- Data Classes ---------------------------- #


@dataclass(slots=True)
class Config:
    file: Optional[Path]
    target: str
    test_size: float
    seed: int
    json_path: Optional[Path]
    features: Optional[List[str]]
    verbose: bool


@dataclass(slots=True)
class Metrics:
    accuracy: float
    macro_precision: float
    macro_recall: float
    macro_f1: float
    micro_precision: float
    micro_recall: float
    micro_f1: float
    classes: List[str]
    confusion: List[List[int]]


# ---------------------------- Loading Logic ---------------------------- #


def load_dataset(cfg: Config) -> Tuple[pd.DataFrame, pd.Series, List[str]]:
    """Load dataset from CSV or fall back to built-in Iris.

    Returns features (X), targets (y), and class names.
    """
    if cfg.file:
        if not cfg.file.exists():
            raise FileNotFoundError(f"File not found: {cfg.file}")
        if cfg.verbose:
            print(f"Loading data from {cfg.file} ...")
        df = pd.read_csv(cfg.file)
        if cfg.target not in df.columns:
            raise KeyError(f"Target column '{cfg.target}' not present in file")
        y = df[cfg.target]
        X = df.drop(columns=[cfg.target])
        target_names = sorted(list(map(str, y.unique())))
    else:
        if cfg.verbose:
            print("Loading built-in Iris dataset ...")
        # Use mapping access to avoid static analyzer confusion over Bunch attribute types
    from typing import Any as _Any  # local import to limit scope

    iris_bunch = load_iris()  # dataset Bunch
    bunch_any = iris_bunch  # type: _Any
    X = pd.DataFrame(bunch_any.data, columns=list(bunch_any.feature_names))  # type: ignore[attr-defined]
    y = pd.Series(bunch_any.target)  # type: ignore[attr-defined]
    target_names = list(bunch_any.target_names)  # type: ignore[attr-defined]
    if cfg.features:
        missing = [f for f in cfg.features if f not in X.columns]
        if missing:
            raise KeyError(f"Requested feature(s) not found: {missing}")
        X = X[cfg.features]
    return X, y, target_names


# ---------------------------- Training Logic ---------------------------- #


def train_model(X: pd.DataFrame, y: pd.Series, cfg: Config):
    X_train, X_test, y_train, y_test = train_test_split(
        X,
        y,
        test_size=cfg.test_size,
        random_state=cfg.seed,
        stratify=y if len(y.unique()) > 1 else None,
    )
    if cfg.verbose:
        print(f"Train size: {len(X_train)} | Test size: {len(X_test)}")
    model = GaussianNB()
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    return model, (X_test, y_test, y_pred)


# ---------------------------- Metrics Logic ---------------------------- #


def compute_metrics(y_true: pd.Series, y_pred, class_names: List[str]) -> Metrics:
    acc = float(accuracy_score(y_true, y_pred))
    pr_macro, rc_macro, f1_macro, _ = precision_recall_fscore_support(
        y_true, y_pred, average="macro", zero_division=0
    )
    pr_micro, rc_micro, f1_micro, _ = precision_recall_fscore_support(
        y_true, y_pred, average="micro", zero_division=0
    )
    cm = confusion_matrix(y_true, y_pred)
    return Metrics(
        accuracy=float(acc),
        macro_precision=float(pr_macro),
        macro_recall=float(rc_macro),
        macro_f1=float(f1_macro),
        micro_precision=float(pr_micro),
        micro_recall=float(rc_micro),
        micro_f1=float(f1_micro),
        classes=class_names,
        confusion=cm.tolist(),
    )


def print_report(
    metrics: Metrics, y_true: pd.Series, y_pred, class_names: List[str]
) -> None:
    print("\n--- Metrics ---")
    print(f"Accuracy: {metrics.accuracy:.4f}")
    print(f"Macro Precision: {metrics.macro_precision:.4f}")
    print(f"Macro Recall:    {metrics.macro_recall:.4f}")
    print(f"Macro F1:        {metrics.macro_f1:.4f}")
    print(f"Micro F1:        {metrics.micro_f1:.4f}")
    print("\nClassification Report:")
    # Build dynamic target_names mapping if numbers
    report = classification_report(
        y_true, y_pred, target_names=class_names, zero_division=0
    )
    print(report)
    print("Confusion Matrix (rows=true, cols=pred):")
    for row in metrics.confusion:
        print("  " + " ".join(f"{v:4d}" for v in row))


# ---------------------------- CLI & Main ---------------------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Gaussian Naive Bayes classifier for Iris or custom CSV"
    )
    p.add_argument(
        "-f",
        "--file",
        type=Path,
        help="Path to CSV file (if omitted uses Iris dataset)",
    )
    p.add_argument(
        "-t",
        "--target",
        default="target",
        help="Target column name in CSV (default 'target')",
    )
    p.add_argument(
        "--test-size", type=float, default=0.2, help="Test split proportion (0 < p < 1)"
    )
    p.add_argument(
        "--seed", type=int, default=42, help="Random seed for reproducibility"
    )
    p.add_argument("--json", type=Path, help="Write metrics JSON to this path")
    p.add_argument(
        "--features", help="Comma-separated subset of feature columns to use"
    )
    p.add_argument("--verbose", action="store_true", help="Verbose logging")
    return p


def parse_args(argv: Optional[List[str]]) -> Config:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not (0 < args.test_size < 1):
        parser.error("--test-size must be between 0 and 1 (exclusive)")
    feature_list = (
        [f.strip() for f in args.features.split(",")] if args.features else None
    )
    return Config(
        file=args.file,
        target=args.target,
        test_size=args.test_size,
        seed=args.seed,
        json_path=args.json,
        features=feature_list,
        verbose=args.verbose,
    )


def main(argv: Optional[List[str]] = None) -> int:
    try:
        cfg = parse_args(argv)
        X, y, class_names = load_dataset(cfg)
        model, (X_test, y_test, y_pred) = train_model(X, y, cfg)
        metrics = compute_metrics(y_test, y_pred, class_names)
        print_report(metrics, y_test, y_pred, class_names)
        if cfg.json_path:
            data: Dict[str, Any] = asdict(metrics)
            cfg.json_path.write_text(json.dumps(data, indent=2), encoding="utf-8")
            if cfg.verbose:
                print(f"Wrote metrics JSON to {cfg.json_path}")
        return 0
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 2
    except KeyError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 3
    except Exception as e:  # pragma: no cover
        print(f"Unexpected error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
