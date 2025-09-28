"""Yahoo Finance data access + local caching utilities."""

from __future__ import annotations

import datetime as dt
import hashlib
import io
import json
from pathlib import Path
from typing import Any, Dict, Optional

import pandas as pd
import requests

HistoricalPriceFrame = pd.DataFrame


class FileResponseCache:
    """Very small file-based cache for HTTP responses."""

    def __init__(self, cache_dir: Path, ttl: dt.timedelta) -> None:
        self.cache_dir = cache_dir
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        self.ttl = ttl

    def _path_for(self, key: str) -> Path:
        digest = hashlib.sha256(key.encode("utf-8")).hexdigest()
        return self.cache_dir / f"{digest}.json"

    def get(self, key: str) -> Optional[str]:
        path = self._path_for(key)
        if not path.exists():
            return None
        try:
            payload = json.loads(path.read_text("utf-8"))
            fetched_at = dt.datetime.fromisoformat(payload["fetched_at"])
            if fetched_at.tzinfo is None:
                fetched_at = fetched_at.replace(tzinfo=dt.timezone.utc)
        except (json.JSONDecodeError, KeyError, ValueError):
            # Corrupt cache entry – delete and treat as miss.
            path.unlink(missing_ok=True)
            return None

        now_utc = dt.datetime.now(dt.timezone.utc)
        if now_utc - fetched_at > self.ttl:
            path.unlink(missing_ok=True)
            return None
        return payload.get("text")

    def set(self, key: str, text: str) -> None:
        payload = {
            "fetched_at": dt.datetime.now(dt.timezone.utc).isoformat(),
            "text": text,
        }
        path = self._path_for(key)
        path.write_text(json.dumps(payload), encoding="utf-8")


class YahooFinanceError(RuntimeError):
    """Raised when Yahoo Finance returns an error response."""


class YahooFinanceDataClient:
    """Fetches historical price data from Yahoo Finance with caching."""

    BASE_DOWNLOAD_URL = "https://query1.finance.yahoo.com/v7/finance/download/{symbol}"

    def __init__(
        self,
        *,
        cache_dir: Optional[Path] = None,
        cache_ttl: dt.timedelta = dt.timedelta(hours=6),
        session: Optional[requests.Session] = None,
        user_agent: str = "StockSimulator/1.0 (+https://github.com/saintwithataint/Pro-g-rammingChallenges4)",
    ) -> None:
        cache_path = cache_dir or Path.home() / ".cache" / "stock_simulator"
        self.cache = FileResponseCache(cache_path, cache_ttl)
        self.session = session or requests.Session()
        self.user_agent = user_agent

    @staticmethod
    def _to_timestamp(value: dt.date | dt.datetime | str) -> int:
        if isinstance(value, str):
            value = dt.datetime.fromisoformat(value)
        if isinstance(value, dt.date) and not isinstance(value, dt.datetime):
            value = dt.datetime.combine(value, dt.time.min)
        if isinstance(value, dt.datetime):
            if value.tzinfo is None:
                value = value.replace(tzinfo=dt.timezone.utc)
            return int(value.timestamp())
        raise TypeError("Unsupported date value type")

    def _build_params(
        self,
        start: dt.date | dt.datetime | str,
        end: dt.date | dt.datetime | str,
        interval: str,
        adjust_close: bool,
        events: str,
    ) -> Dict[str, Any]:
        return {
            "period1": self._to_timestamp(start),
            "period2": self._to_timestamp(end),
            "interval": interval,
            "events": events,
            "includeAdjustedClose": str(adjust_close).lower(),
        }

    def fetch_price_history(
        self,
        symbol: str,
        *,
        start: dt.date | dt.datetime | str,
        end: dt.date | dt.datetime | str,
        interval: str = "1d",
        adjust_close: bool = True,
        events: str = "history",
    ) -> HistoricalPriceFrame:
        """Return a dataframe of historical prices for ``symbol``.

        Parameters
        ----------
        symbol:
            Yahoo Finance ticker symbol.
        start, end:
            Inclusive date range. Strings are parsed using ``datetime.fromisoformat``.
        interval:
            Bar interval supported by Yahoo (e.g., ``1d``, ``1wk``, ``1mo``).
        adjust_close:
            When True, ensures ``Adj Close`` column is included.
        events:
            Yahoo "events" selector (default ``history``).
        """

        params = self._build_params(start, end, interval, adjust_close, events)
        url = self.BASE_DOWNLOAD_URL.format(symbol=symbol)
        cache_key = f"{url}?" + "&".join(f"{k}={v}" for k, v in sorted(params.items()))

        cached = self.cache.get(cache_key)
        if cached is not None:
            csv_text = cached
        else:
            response = self.session.get(
                url,
                params=params,
                headers={"User-Agent": self.user_agent},
                timeout=30,
            )
            if response.status_code != 200:
                message = response.text.strip() or response.reason
                raise YahooFinanceError(
                    f"Yahoo Finance request failed for {symbol} ({response.status_code}): {message}"
                )
            csv_text = response.text
            self.cache.set(cache_key, csv_text)

        data = (
            pd.read_csv(io.StringIO(csv_text), parse_dates=["Date"])
            .set_index("Date")
            .sort_index()
        )
        required_columns = {"Open", "High", "Low", "Close", "Volume"}
        missing = required_columns.difference(set(data.columns))
        if missing:
            raise YahooFinanceError(
                f"Missing expected columns from Yahoo response: {', '.join(sorted(missing))}"
            )
        return data


__all__ = [
    "YahooFinanceDataClient",
    "YahooFinanceError",
    "HistoricalPriceFrame",
]
