"""Producer-Consumer demonstration (modern Python version).

Features:
  - Configurable counts via CLI (argparse) instead of hard-coded globals
  - Uses queue.Queue for built-in thread-safe bounded buffer
  - Graceful shutdown using sentinel objects (one per consumer)
  - Structured logging with optional timestamps and quiet mode
  - Dataclasses for configuration and statistics
  - Type hints & clear documentation for new developers

Examples:
  python pc.py --items-per-producer 20 --producers 2 --consumers 3 \
               --capacity 8 --prod-delay 0.05 --cons-delay 0.1
"""

from __future__ import annotations

import argparse
import queue
import threading
import time
from dataclasses import dataclass, field
from typing import Any, Optional

# Sentinel used to signal consumers to exit. A unique object so it won't
# collide with any normal produced value.
SENTINEL = object()


@dataclass
class Config:
    capacity: int = 10
    items_per_producer: int = 20
    producers: int = 1
    consumers: int = 1
    prod_delay: float = 0.1  # seconds
    cons_delay: float = 0.1  # seconds
    quiet: bool = False
    timestamps: bool = True


@dataclass
class Stats:
    produced: int = 0
    consumed: int = 0
    _lock: threading.Lock = field(default_factory=threading.Lock, init=False, repr=False)

    def increment_produced(self) -> None:
        with self._lock:
            self.produced += 1

    def increment_consumed(self) -> None:
        with self._lock:
            self.consumed += 1


class Logger:
    def __init__(self, enabled: bool = True, timestamps: bool = True) -> None:
        self._enabled = enabled
        self._timestamps = timestamps
        self._lock = threading.Lock()
        self._t0 = time.perf_counter()

    def log(self, msg: str) -> None:
        if not self._enabled:
            return
        with self._lock:
            if self._timestamps:
                elapsed_ms = int((time.perf_counter() - self._t0) * 1000)
                print(f"[{elapsed_ms:05d} ms] {msg}")
            else:
                print(msg)


def parse_args(argv: Optional[list[str]] = None) -> Config:
    parser = argparse.ArgumentParser(description="Producer-Consumer demo")
    parser.add_argument(
        "--capacity", type=int, default=10, help="Bounded buffer capacity"
    )
    parser.add_argument(
        "--items-per-producer",
        type=int,
        default=20,
        help="Items each producer generates",
    )
    parser.add_argument(
        "--producers", type=int, default=1, help="Number of producer threads"
    )
    parser.add_argument(
        "--consumers", type=int, default=1, help="Number of consumer threads"
    )
    parser.add_argument(
        "--prod-delay",
        type=float,
        default=0.1,
        help="Delay (s) after producing an item",
    )
    parser.add_argument(
        "--cons-delay",
        type=float,
        default=0.1,
        help="Delay (s) after consuming an item",
    )
    parser.add_argument(
        "--quiet", action="store_true", help="Suppress per-item log lines"
    )
    parser.add_argument(
        "--no-timestamps", action="store_true", help="Disable timestamp prefix"
    )
    args = parser.parse_args(argv)
    return Config(
        capacity=args.capacity,
        items_per_producer=args.items_per_producer,
        producers=args.producers,
        consumers=args.consumers,
        prod_delay=args.prod_delay,
        cons_delay=args.cons_delay,
        quiet=args.quiet,
        timestamps=not args.no_timestamps,
    )


def producer_task(
    idx: int, cfg: Config, stats: Stats, q: "queue.Queue[Any]", log: Logger
) -> None:
    for i in range(cfg.items_per_producer):
        value = (idx, i)  # tuple: (producer id, sequence)
        # blocks if queue full
        q.put(value)
        stats.increment_produced()
        log.log(f"P{idx} produced seq={i} size={q.qsize()}")
        if cfg.prod_delay > 0:
            time.sleep(cfg.prod_delay)


def consumer_task(
    idx: int, cfg: Config, stats: Stats, q: "queue.Queue[Any]", log: Logger
) -> None:
    while True:
        item = q.get()  # blocks if empty
        if item is SENTINEL:  # termination signal
            q.task_done()
            log.log(f"C{idx} received sentinel -> exit")
            break
        stats.increment_consumed()
        prod_id, seq = item  # unpack tuple produced
        log.log(f"C{idx} consumed P{prod_id} seq={seq} size={q.qsize()}")
        if cfg.cons_delay > 0:
            time.sleep(cfg.cons_delay)
        q.task_done()


def main(argv: Optional[list[str]] = None) -> int:
    cfg = parse_args(argv)
    stats = Stats()
    log = Logger(enabled=not cfg.quiet, timestamps=cfg.timestamps)
    q: "queue.Queue[Any]" = queue.Queue(maxsize=cfg.capacity)

    start = time.perf_counter()

    threads: list[threading.Thread] = []
    # Launch producers
    for p in range(cfg.producers):
        t = threading.Thread(
            target=producer_task, args=(p, cfg, stats, q, log), daemon=False
        )
        threads.append(t)
        t.start()

    # Launch consumers
    for c in range(cfg.consumers):
        t = threading.Thread(
            target=consumer_task, args=(c, cfg, stats, q, log), daemon=False
        )
        threads.append(t)
        t.start()

    # Wait for all producers to finish then inject one sentinel per consumer.
    for t in threads[: cfg.producers]:
        t.join()
    for _ in range(cfg.consumers):
        q.put(SENTINEL)

    # Wait until queue tasks complete then wait for consumers to exit
    q.join()  # ensures all non-sentinel tasks processed
    for t in threads[cfg.producers :]:
        t.join()

    elapsed = (time.perf_counter() - start) * 1000
    expected = cfg.producers * cfg.items_per_producer
    ok = (stats.produced == expected) and (stats.consumed == expected)

    print("\nSummary:")
    print(f"  Producers:           {cfg.producers} (each {cfg.items_per_producer})")
    print(f"  Consumers:           {cfg.consumers}")
    print(f"  Buffer capacity:     {cfg.capacity}")
    print(f"  Produced total:      {stats.produced}")
    print(f"  Consumed total:      {stats.consumed}")
    print(f"  Elapsed (ms):        {elapsed:.2f}")
    print(f"  Success:             {ok}")

    return 0 if ok else 1


if __name__ == "__main__":  # pragma: no cover - manual execution path
    raise SystemExit(main())
