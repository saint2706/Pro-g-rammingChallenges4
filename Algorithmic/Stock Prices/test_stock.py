import pandas as pd
from pathlib import Path
from stock import main as stock_main

CSV_CONTENT = """Date,Open,High,Low,Close,Volume
2024-01-01,10,11,9,10.5,1000
2024-01-02,10.5,11.5,10,11,1200
2024-01-03,11,12,10.5,11.5,1300
"""


def test_basic_json(tmp_path):
    csv_path = tmp_path / "sample.csv"
    csv_path.write_text(CSV_CONTENT, encoding="utf-8")
    rc = stock_main(
        [
            "--source",
            str(csv_path),
            "--sma",
            "2",
            "--ema",
            "3",
            "--returns",
            "--json",
            "--no-show",
        ]
    )
    assert rc == 0


def test_html_export(tmp_path):
    csv_path = tmp_path / "sample2.csv"
    out_html = tmp_path / "chart.html"
    csv_path.write_text(CSV_CONTENT, encoding="utf-8")
    rc = stock_main(["--source", str(csv_path), "--html", str(out_html), "--no-show"])
    assert rc == 0
    assert out_html.exists()


def test_invalid_source():
    rc = stock_main(
        ["--source", "no_such_file.csv", "--no-show"]
    )  # returns error code 1
    assert rc == 1
