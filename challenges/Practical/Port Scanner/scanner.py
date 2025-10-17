"""scanner.py - Concurrent TCP port scanner (modernized).

Features / Improvements:
  * Dataclass `PortScanConfig` for structured configuration
  * Robust port parsing: ranges (1-1024), comma lists (22,80,443), mixed (1-25,80,443,8000-8100)
  * `--top N` common ports shortcut (uses built-in ranked list) if no explicit --ports provided
  * Concurrency via ThreadPoolExecutor with graceful cancellation (Ctrl+C) using an Event
  * Optional service name resolution with `--services`
  * Optional JSON summary export and CSV open ports export
  * Exit codes: 0 success, 1 operational error, 2 bad arguments / validation, 3 dependency / internal error
  * Type hints & extensive inline comments for maintainability
  * Bounded connection timeout per port for responsiveness

Examples:
  python scanner.py example.com --top 100 --services --json scan.json
  python scanner.py 192.168.1.10 -p 22,80,443,8000-8100 -t 500 --csv open_ports.csv
"""

from __future__ import annotations

import argparse
import csv
import json
import socket
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from threading import Event
from typing import Iterable, List, Sequence, Set

# ---------------- Data & Config ---------------- #

COMMON_PORTS_RANKED: List[int] = [
    80,
    443,
    22,
    21,
    25,
    110,
    143,
    53,
    23,
    3389,
    3306,
    8080,
    445,
    139,
    995,
    993,
    587,
    5900,
    1723,
    111,
    995,
    465,
    135,
    1025,
    8888,
    8000,
    8443,
    53,
    389,
    636,
    2049,
    32768,
    49152,
    49153,
    49154,
    49155,
    49156,
    49157,
]


@dataclass(slots=True)
class PortScanConfig:
    target: str
    ports: List[int]
    threads: int = 200
    timeout: float = 0.75
    resolve_services: bool = False
    json_path: Path | None = None
    csv_path: Path | None = None
    top: int | None = None

    def validate(self) -> None:
        if not self.ports:
            raise ValueError("No ports specified after parsing")
        if self.threads < 1 or self.threads > 5000:
            raise ValueError("threads must be between 1 and 5000")
        if self.timeout <= 0 or self.timeout > 10:
            raise ValueError("timeout must be in (0, 10]")


# ---------------- Parsing ---------------- #


def parse_port_spec(spec: str) -> List[int]:
    """Parse port specification into a sorted unique list of ints.

    Supports: "80", "22,80,443", "1-1024", or mixed: "1-25,80,443,8000-8100".
    Raises ValueError on invalid tokens or out-of-range ports.
    """
    ports: Set[int] = set()
    for token in spec.split(","):
        token = token.strip()
        if not token:
            continue
        if "-" in token:
            try:
                start_s, end_s = token.split("-", 1)
                start, end = int(start_s), int(end_s)
            except ValueError as e:
                raise ValueError(f"Invalid range: {token}") from e
            if start > end:
                start, end = end, start
            for p in range(start, end + 1):
                _validate_port(p)
                ports.add(p)
        else:
            try:
                p = int(token)
            except ValueError as e:
                raise ValueError(f"Invalid port: {token}") from e
            _validate_port(p)
            ports.add(p)
    return sorted(ports)


def _validate_port(p: int) -> None:
    if p < 1 or p > 65535:
        raise ValueError(f"Port out of range: {p}")


# ---------------- Scanning Core ---------------- #


class PortScanner:
    """Concurrent TCP connect scanner.

    Each port attempt performs a TCP connect with a bounded timeout. Open ports are
    collected; optionally resolved service names are attached later to avoid DNS lookups
    during critical scanning loops.
    """

    def __init__(self, cfg: PortScanConfig, stop_event: Event | None = None):
        self.cfg = cfg
        self.stop_event = stop_event or Event()
        self.open_ports: List[int] = []

    def _scan_single(self, port: int) -> bool:
        if self.stop_event.is_set():
            return False
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.settimeout(self.cfg.timeout)
                return s.connect_ex((self.cfg.target, port)) == 0
        except OSError:
            return False

    def run(self) -> List[int]:
        ports = self.cfg.ports
        open_found: List[int] = []
        # Use min(len(ports), threads) workers at most
        workers = min(len(ports), self.cfg.threads)
        with ThreadPoolExecutor(max_workers=workers) as ex:
            future_map = {ex.submit(self._scan_single, p): p for p in ports}
            for fut in as_completed(future_map):
                if self.stop_event.is_set():
                    break
                port = future_map[fut]
                try:
                    if fut.result():
                        open_found.append(port)
                except Exception:  # pragma: no cover - protective
                    pass
        self.open_ports = sorted(open_found)
        return self.open_ports


# ---------------- Service Resolution ---------------- #


def resolve_services(ports: Iterable[int]) -> dict[int, str | None]:
    result: dict[int, str | None] = {}
    for p in ports:
        try:
            result[p] = socket.getservbyport(p)
        except OSError:
            result[p] = None
    return result


# ---------------- CLI ---------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Concurrent TCP port scanner with JSON export."
    )
    p.add_argument("target", help="Target host (domain or IP)")
    p.add_argument(
        "-p",
        "--ports",
        help="Port spec (e.g., 1-1024,22,80,443). If omitted and --top not used, default 1-1024.",
    )
    p.add_argument(
        "--top",
        type=int,
        help="Scan top N common ports (overrides default if no --ports provided)",
    )
    p.add_argument(
        "-t",
        "--threads",
        type=int,
        default=300,
        help="Max concurrent workers (default 300)",
    )
    p.add_argument(
        "--timeout",
        type=float,
        default=0.75,
        help="Per-port timeout seconds (default 0.75)",
    )
    p.add_argument(
        "--services", action="store_true", help="Resolve service names for open ports"
    )
    p.add_argument("--json", type=Path, help="Write JSON summary to file")
    p.add_argument(
        "--csv", type=Path, help="Write CSV of open ports (columns: port,service)"
    )
    return p


def parse_args(argv: Sequence[str] | None) -> PortScanConfig:
    parser = build_parser()
    args = parser.parse_args(argv)

    # Resolve target to IP early for clarity
    try:
        target_ip = socket.gethostbyname(args.target)
    except socket.gaierror:
        print(f"Hostname could not be resolved: {args.target}", file=sys.stderr)
        raise SystemExit(2)

    # Determine port list
    if args.ports:
        try:
            ports_list = parse_port_spec(args.ports)
        except ValueError as e:
            print(f"Port specification error: {e}", file=sys.stderr)
            raise SystemExit(2)
    else:
        if args.top:
            ports_list = COMMON_PORTS_RANKED[: max(1, args.top)]
        else:
            ports_list = list(range(1, 1025))

    cfg = PortScanConfig(
        target=target_ip,
        ports=ports_list,
        threads=args.threads,
        timeout=args.timeout,
        resolve_services=args.services,
        json_path=args.json,
        csv_path=args.csv,
        top=args.top,
    )
    try:
        cfg.validate()
    except ValueError as e:
        print(f"Configuration error: {e}", file=sys.stderr)
        raise SystemExit(2)
    return cfg


# ---------------- Orchestration ---------------- #


def write_json(
    cfg: PortScanConfig,
    open_ports: List[int],
    elapsed: float,
    services: dict[int, str | None] | None,
) -> None:
    if not cfg.json_path:
        return
    data = {
        "target": cfg.target,
        "scanned_ports": len(cfg.ports),
        "open_ports_count": len(open_ports),
        "open_ports": open_ports,
        "threads": cfg.threads,
        "timeout": cfg.timeout,
        "services": services if services else None,
        "elapsed_seconds": round(elapsed, 3),
        "top_mode": cfg.top is not None,
    }
    try:
        with open(cfg.json_path, "w", encoding="utf-8") as fh:
            json.dump(data, fh, indent=2)
        print(f"JSON summary written to {cfg.json_path}")
    except OSError as e:
        print(f"Warning: Could not write JSON file: {e}", file=sys.stderr)


def write_csv(
    cfg: PortScanConfig, open_ports: List[int], services: dict[int, str | None] | None
) -> None:
    if not cfg.csv_path or not open_ports:
        return
    try:
        with open(cfg.csv_path, "w", newline="", encoding="utf-8") as fh:
            writer = csv.writer(fh)
            writer.writerow(["port", "service"])
            for p in open_ports:
                svc = services.get(p) if services else None
                writer.writerow([p, svc or ""])
        print(f"CSV written to {cfg.csv_path}")
    except OSError as e:
        print(f"Warning: Could not write CSV file: {e}", file=sys.stderr)


def main(argv: Sequence[str] | None = None) -> int:
    stop_event = Event()
    try:
        cfg = parse_args(argv)
    except SystemExit as e:
        return int(e.code) if isinstance(e.code, int) else 2

    start = time.time()
    scanner = PortScanner(cfg, stop_event=stop_event)
    try:
        open_ports = scanner.run()
    except KeyboardInterrupt:  # graceful cancel
        stop_event.set()
        print("Scan cancelled by user.", file=sys.stderr)
        return 1

    services_map = (
        resolve_services(open_ports) if cfg.resolve_services and open_ports else None
    )
    elapsed = time.time() - start

    print("-" * 50)
    print(f"Target: {cfg.target}")
    print(
        f"Scanned ports: {len(cfg.ports)} | Open: {len(open_ports)} | Threads: {cfg.threads} | Timeout: {cfg.timeout}s"
    )
    if cfg.top:
        print(f"Top mode: first {cfg.top} common ports")
    print(f"Elapsed: {elapsed:.2f}s")
    if open_ports:
        print("Open ports:")
        for p in open_ports:
            if services_map:
                svc = services_map.get(p) or "-"
                print(f"  {p:>5}  {svc}")
            else:
                print(f"  {p:>5}")
    else:
        print("No open ports detected.")

    write_json(cfg, open_ports, elapsed, services_map)
    write_csv(cfg, open_ports, services_map)

    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
