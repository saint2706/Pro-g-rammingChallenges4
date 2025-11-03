# Stock Price Visualizer

## Problem Statement
Load historical price data from a CSV file or URL, compute optional indicators (SMA, EMA, returns), and render interactive candlestick-style charts while optionally emitting JSON summaries.

## Usage

### Python helper (`stock.py`)
- Plot a local CSV without launching a browser window:
  ```bash
  python stock.py --source finance.csv --no-show --html chart.html
  ```
- Fetch remote data and compute moving averages:
  ```bash
  python stock.py --url https://example.com/data.csv --sma 20 50 --ema 100
  ```
- Generate indicator statistics only:
  ```bash
  python stock.py --source finance.csv --returns --json
  ```

### Haskell helper (`Stock.hs`)
- Build (or run with `runghc`) and export an interactive Plotly HTML dashboard:
  ```bash
  ghc -O2 Stock.hs && ./Stock --source finance.csv --sma 20 --sma 50 --ema 100 --html chart.html
  ```
- Stream remote CSV data, request daily returns, and print the JSON summary:
  ```bash
  runghc Stock.hs --url https://example.com/data.csv --returns --json --no-show
  ```
- Supply a custom title for the chart output:
  ```bash
  runghc Stock.hs --source finance.csv --ticker AAPL --html aapl.html
  ```

> **Indicator flags**: Repeat `--sma` or `--ema` for each window you need (e.g. `--sma 20 --sma 50`). This mirrors the semantics of the Python CLI while keeping the command-line interface consistent across platforms.

## Prerequisites

The Python script depends on `pandas` and `plotly`. The Haskell helper uses `aeson`, `cassava`, `http-conduit` (for `Network.HTTP.Simple`), `optparse-applicative`, `text`, `vector`, and `bytestring`. Install them via Cabal:

```bash
cabal install aeson cassava http-conduit optparse-applicative text vector
```

## Debugging Tips
- Ensure the CSV contains a `Date` column; otherwise the parser will raise a descriptive error.
- Run the automated tests:
  ```bash
  pytest test_stock.py
  ```
  They cover configuration validation, moving average calculations, and JSON serialization.
- When diagnosing chart issues, pass `--log DEBUG` to trace column detection and indicator computation.

## Implementation Notes
- Python: a `ChartConfig` dataclass validates CLI options, pandas ingests CSV/URL sources, and Plotly renders the interactive figure (or saves HTML when running headless).
- Haskell: `Stock.hs` mirrors the CLI, loading CSVs with `cassava`, computing SMA/EMA/returns in pure vectors, and exporting Plotly-powered HTML (or JSON summaries) without opening a browser when `--no-show` is present.

## Further Reading
- [Pandas User Guide: Time Series / Date Functionality](https://pandas.pydata.org/docs/user_guide/timeseries.html)
- [Plotly Financial Charts Documentation](https://plotly.com/python/financial-charts/)
