# Stock Price Visualizer

## Problem Statement
Load historical price data from a CSV file or URL, compute optional indicators (SMA, EMA, returns), and render interactive candlestick-style charts while optionally emitting JSON summaries.

## Usage
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

## Debugging Tips
- Ensure the CSV contains a `Date` column; otherwise the parser will raise a descriptive error.
- Run the automated tests:
  ```bash
  pytest test_stock.py
  ```
  They cover configuration validation, moving average calculations, and JSON serialization.
- When diagnosing chart issues, pass `--log DEBUG` to trace column detection and indicator computation.

## Implementation Notes
- Uses a `ChartConfig` dataclass to validate CLI options and enforce mutually exclusive data sources (`--source` vs `--url`).
- Pandas handles data ingestion while Plotly renders the interactive figure (or saves HTML when running headless).
- Indicator helpers append SMA/EMA/return columns directly to the DataFrame before chart construction.

## Further Reading
- [Pandas User Guide: Time Series / Date Functionality](https://pandas.pydata.org/docs/user_guide/timeseries.html)
- [Plotly Financial Charts Documentation](https://plotly.com/python/financial-charts/)
