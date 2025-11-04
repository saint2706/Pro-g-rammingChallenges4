"""stock.py - Lightweight stock time-series visualization helper.

Enhancements over original version:
  * Dataclass `ChartConfig` for reproducibility & testing
  * CLI with flexible input: local CSV or remote URL
  * Optional moving averages (SMA/EMA) and daily returns
  * JSON summary output (`--json`) without launching a browser
  * Headless / no-show mode and HTML export (`--html`)
  * Graceful error handling with clear messages
  * Column auto-detection (OHLC/Volume) with fallback names

Usage examples:
  python stock.py --source finance.csv --sma 20 50 --ema 100 --no-show --html chart.html --json
  python stock.py --url https://raw.githubusercontent.com/plotly/datasets/master/finance-charts-apple.csv --returns

The script expects at minimum a Date column plus any subset of Open High Low Close Volume columns.
If columns are prefixed (e.g. AAPL.Open) that prefix is stripped automatically.
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass, asdict
from typing import List, Optional, Sequence

import pandas as pd
import plotly.graph_objects as go
from urllib.error import URLError

REQUIRED_DATE = "Date"
POSSIBLE_PRICE_COLS = ["Open", "High", "Low", "Close"]
VOLUME_COL = "Volume"


@dataclass(slots=True)
class ChartConfig:
    source: Optional[str] = None  # local CSV path
    url: Optional[str] = None  # remote CSV URL
    sma: Optional[List[int]] = None
    ema: Optional[List[int]] = None
    returns: bool = False
    json_output: bool = False
    html: Optional[str] = None
    no_show: bool = False
    ticker: Optional[str] = None

    def validate(self) -> None:
        if (self.source is None) == (self.url is None):
            raise ValueError("Provide exactly one data source: --source or --url")
        if self.sma:
            for w in self.sma:
                if w <= 0:
                    raise ValueError("SMA window must be positive")
        if self.ema:
            for w in self.ema:
                if w <= 0:
                    raise ValueError("EMA window must be positive")


def load_dataframe(cfg: ChartConfig) -> pd.DataFrame:
    try:
        if cfg.source:
            df = pd.read_csv(cfg.source)
        else:
            assert cfg.url is not None
            df = pd.read_csv(cfg.url)
    except URLError:
        raise ValueError("Network error retrieving remote CSV")
    except FileNotFoundError:
        raise ValueError(f"File not found: {cfg.source}")
    except pd.errors.EmptyDataError:
        raise ValueError("CSV is empty")
    if REQUIRED_DATE not in df.columns:
        raise ValueError("CSV missing Date column")
    df[REQUIRED_DATE] = pd.to_datetime(df[REQUIRED_DATE])
    # Normalize column names: strip prefix like AAPL.
    rename_map = {}
    for col in df.columns:
        if "." in col:
            base = col.split(".")[-1]
            rename_map[col] = base
    if rename_map:
        df = df.rename(columns=rename_map)
    # Ensure chronological order for downstream calculations while preserving
    # the original relative ordering of rows with identical timestamps.
    df = df.sort_values(REQUIRED_DATE, kind="mergesort").reset_index(drop=True)
    return df


def add_indicators(df: pd.DataFrame, cfg: ChartConfig) -> None:
    if cfg.sma:
        for w in cfg.sma:
            if "Close" in df:
                df[f"SMA_{w}"] = df["Close"].rolling(window=w, min_periods=1).mean()
    if cfg.ema:
        for w in cfg.ema:
            if "Close" in df:
                df[f"EMA_{w}"] = df["Close"].ewm(span=w, adjust=False).mean()
    if cfg.returns and "Close" in df:
        df["Returns"] = df["Close"].pct_change().fillna(0.0)


def build_figure(df: pd.DataFrame, cfg: ChartConfig) -> go.Figure:
    fig = go.Figure()
    # Price traces
    for col in POSSIBLE_PRICE_COLS:
        if col in df.columns:
            fig.add_trace(
                go.Scatter(
                    name=col,
                    x=df[REQUIRED_DATE],
                    y=df[col],
                    mode="lines",
                )
            )
    # Indicators
    for col in df.columns:
        if col.startswith("SMA_") or col.startswith("EMA_"):
            fig.add_trace(
                go.Scatter(name=col, x=df[REQUIRED_DATE], y=df[col], mode="lines")
            )
    # Returns bar (secondary axis) if present
    if "Returns" in df.columns:
        fig.add_trace(
            go.Bar(
                name="Returns",
                x=df[REQUIRED_DATE],
                y=df["Returns"],
                yaxis="y2",
                opacity=0.3,
            )
        )
    # Volume bar
    if VOLUME_COL in df.columns:
        fig.add_trace(
            go.Bar(
                name="Volume",
                x=df[REQUIRED_DATE],
                y=df[VOLUME_COL],
                yaxis="y3",
                opacity=0.4,
            )
        )
    title = cfg.ticker or "Stock Data"
    fig.update_layout(
        title=title,
        xaxis_title="Date",
        yaxis_title="Price",
        yaxis=dict(showgrid=True),
        yaxis2=dict(
            title="Returns", overlaying="y", side="right", showgrid=False, position=1.0
        ),
        yaxis3=dict(
            title="Volume",
            overlaying="y",
            side="right",
            showgrid=False,
            anchor="x",
            position=0.97,
        ),
        legend_title_text="Series",
        bargap=0.1,
        margin=dict(t=50, l=60, r=60, b=40),
    )
    return fig


from typing import Dict, Any


def summarize(df: pd.DataFrame) -> Dict[str, Any]:
    out: Dict[str, Any] = {"rows": int(len(df))}
    for col in ["Open", "High", "Low", "Close"]:
        if col in df.columns:
            out[col] = {
                "min": float(df[col].min()),
                "max": float(df[col].max()),
                "mean": float(df[col].mean()),
            }
    if "Returns" in df.columns:
        out["Returns"] = {
            "min": float(df["Returns"].min()),
            "max": float(df["Returns"].max()),
            "mean": float(df["Returns"].mean()),
        }
    return out


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Stock data visualization")
    p.add_argument("--source", help="Local CSV file path")
    p.add_argument("--url", help="Remote CSV URL")
    p.add_argument(
        "--sma", type=int, nargs="*", help="Simple moving average window sizes"
    )
    p.add_argument(
        "--ema", type=int, nargs="*", help="Exponential moving average window sizes"
    )
    p.add_argument(
        "--returns", action="store_true", help="Include daily percent returns"
    )
    p.add_argument("--ticker", help="Ticker symbol for chart title")
    p.add_argument(
        "--json",
        action="store_true",
        help="Emit JSON summary (no chart unless HTML requested)",
    )
    p.add_argument("--html", help="Export chart to HTML file")
    p.add_argument(
        "--no-show", action="store_true", help="Do not display interactive window"
    )
    return p


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    cfg = ChartConfig(
        source=args.source,
        url=args.url,
        sma=args.sma,
        ema=args.ema,
        returns=args.returns,
        json_output=args.json,
        html=args.html,
        no_show=args.no_show,
        ticker=args.ticker,
    )
    try:
        cfg.validate()
        df = load_dataframe(cfg)
        add_indicators(df, cfg)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    fig = build_figure(df, cfg)

    if cfg.html:
        try:
            fig.write_html(cfg.html)
        except OSError as e:
            print(f"Error writing HTML: {e}", file=sys.stderr)
            return 1

    if cfg.json_output:
        summary = summarize(df)
        print(json.dumps({"summary": summary, "config": asdict(cfg)}, indent=2))
        # Only show chart if explicitly not suppressed and not headless
        if not cfg.no_show and not cfg.html:
            fig.show()
    else:
        if not cfg.no_show:
            fig.show()
        else:
            print("Chart generation complete (no-show).")
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
