"""scanner_gui.py - Tkinter GUI for the modernized concurrent port scanner.

Features:
  * Reuses core logic from scanner.py (PortScanConfig, PortScanner, parse_port_spec, resolve_services)
  * Start / Stop scanning with graceful cancellation
  * Progress bar updates as ports complete
  * Optional service name resolution
  * Optional JSON summary & CSV open port export
  * Input validation with error dialogs
  * Thread-safe message queue for GUI updates
"""

from __future__ import annotations

import queue
import socket
import sys
import threading
import time
import tkinter as tk
from tkinter import ttk, scrolledtext, messagebox, filedialog
from pathlib import Path
from typing import List

try:
    from scanner import (
        PortScanConfig,
        PortScanner,
        parse_port_spec,
        resolve_services,
        COMMON_PORTS_RANKED,
    )
except ImportError as e:  # pragma: no cover
    print("Error: 'scanner.py' (modernized) not found.", file=sys.stderr)
    sys.exit(1)


class PortScannerGUI:
    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("Concurrent Port Scanner")

        self.gui_queue: "queue.Queue[str]" = queue.Queue()
        self.stop_event = threading.Event()
        self.total_ports = 0
        self.completed_ports = 0
        self.current_thread: threading.Thread | None = None

        self._build_ui()
        self._poll_queue()

    # ---------------- UI ---------------- #
    def _build_ui(self):
        main = ttk.Frame(self.master, padding=10)
        main.pack(fill=tk.BOTH, expand=True)

        cfg_frame = ttk.LabelFrame(main, text="Configuration", padding=10)
        cfg_frame.pack(fill=tk.X)

        ttk.Label(cfg_frame, text="Target host:").grid(row=0, column=0, sticky=tk.W)
        self.target_entry = ttk.Entry(cfg_frame, width=28)
        self.target_entry.grid(row=0, column=1, sticky=tk.EW, padx=5)
        self.target_entry.insert(0, "scanme.nmap.org")

        ttk.Label(cfg_frame, text="Ports (spec or blank for default):").grid(
            row=1, column=0, sticky=tk.W
        )
        self.ports_entry = ttk.Entry(cfg_frame, width=28)
        self.ports_entry.grid(row=1, column=1, sticky=tk.EW, padx=5)

        ttk.Label(cfg_frame, text="Threads:").grid(row=2, column=0, sticky=tk.W)
        self.threads_entry = ttk.Entry(cfg_frame, width=10)
        self.threads_entry.insert(0, "300")
        self.threads_entry.grid(row=2, column=1, sticky=tk.W, padx=5)

        ttk.Label(cfg_frame, text="Timeout (s):").grid(row=3, column=0, sticky=tk.W)
        self.timeout_entry = ttk.Entry(cfg_frame, width=10)
        self.timeout_entry.insert(0, "0.75")
        self.timeout_entry.grid(row=3, column=1, sticky=tk.W, padx=5)

        self.services_var = tk.BooleanVar(value=False)
        self.services_chk = ttk.Checkbutton(
            cfg_frame, text="Resolve services", variable=self.services_var
        )
        self.services_chk.grid(row=0, column=2, padx=10, sticky=tk.W)

        ttk.Label(cfg_frame, text="Top N (optional):").grid(
            row=1, column=2, sticky=tk.W
        )
        self.top_entry = ttk.Entry(cfg_frame, width=10)
        self.top_entry.grid(row=1, column=3, sticky=tk.W, padx=5)

        ttk.Label(cfg_frame, text="JSON output:").grid(row=2, column=2, sticky=tk.W)
        self.json_entry = ttk.Entry(cfg_frame, width=20)
        self.json_entry.grid(row=2, column=3, sticky=tk.W, padx=5)
        ttk.Button(cfg_frame, text="...", width=3, command=self._browse_json).grid(
            row=2, column=4, sticky=tk.W
        )

        ttk.Label(cfg_frame, text="CSV output:").grid(row=3, column=2, sticky=tk.W)
        self.csv_entry = ttk.Entry(cfg_frame, width=20)
        self.csv_entry.grid(row=3, column=3, sticky=tk.W, padx=5)
        ttk.Button(cfg_frame, text="...", width=3, command=self._browse_csv).grid(
            row=3, column=4, sticky=tk.W
        )

        for c in range(0, 5):
            cfg_frame.columnconfigure(c, weight=0)
        cfg_frame.columnconfigure(1, weight=1)

        # Action buttons
        action = ttk.Frame(main)
        action.pack(fill=tk.X, pady=6)
        self.start_btn = ttk.Button(action, text="Start Scan", command=self.start_scan)
        self.start_btn.pack(side=tk.LEFT)
        self.stop_btn = ttk.Button(
            action, text="Stop", command=self.stop_scan, state=tk.DISABLED
        )
        self.stop_btn.pack(side=tk.LEFT, padx=6)

        # Progress bar
        self.progress = ttk.Progressbar(main, mode="determinate")
        self.progress.pack(fill=tk.X, pady=(0, 8))

        # Results
        results_frame = ttk.LabelFrame(main, text="Results", padding=10)
        results_frame.pack(fill=tk.BOTH, expand=True)
        self.result_text = scrolledtext.ScrolledText(
            results_frame, wrap=tk.WORD, state=tk.DISABLED, font=("Courier New", 10)
        )
        self.result_text.pack(fill=tk.BOTH, expand=True)

        # Status line
        self.status_var = tk.StringVar(value="Idle")
        self.status_label = ttk.Label(main, textvariable=self.status_var, anchor="w")
        self.status_label.pack(fill=tk.X)

    # ---------------- Helpers ---------------- #
    def _browse_json(self):
        path = filedialog.asksaveasfilename(
            defaultextension=".json",
            filetypes=(("JSON", "*.json"), ("All Files", "*.*")),
        )
        if path:
            self.json_entry.delete(0, tk.END)
            self.json_entry.insert(0, path)

    def _browse_csv(self):
        path = filedialog.asksaveasfilename(
            defaultextension=".csv", filetypes=(("CSV", "*.csv"), ("All Files", "*.*"))
        )
        if path:
            self.csv_entry.delete(0, tk.END)
            self.csv_entry.insert(0, path)

    def log(self, msg: str):
        self.gui_queue.put(msg)

    def _poll_queue(self):
        try:
            while True:
                msg = self.gui_queue.get_nowait()
                self.result_text.config(state=tk.NORMAL)
                self.result_text.insert(tk.END, msg + "\n")
                self.result_text.see(tk.END)
                self.result_text.config(state=tk.DISABLED)
        except queue.Empty:
            pass
        # Update progress bar
        if self.total_ports:
            self.progress["maximum"] = self.total_ports
            self.progress["value"] = self.completed_ports
        self.master.after(100, self._poll_queue)

    # ---------------- Scanning Workflow ---------------- #
    def start_scan(self):
        if self.current_thread and self.current_thread.is_alive():
            return
        target = self.target_entry.get().strip()
        ports_spec = self.ports_entry.get().strip()
        top_spec = self.top_entry.get().strip()
        try:
            threads = int(self.threads_entry.get().strip())
            timeout = float(self.timeout_entry.get().strip())
            if threads < 1 or threads > 5000:
                raise ValueError("Threads out of range")
            if timeout <= 0 or timeout > 10:
                raise ValueError("Timeout out of range")
        except ValueError:
            messagebox.showerror("Input Error", "Invalid threads or timeout value.")
            return
        if not target:
            messagebox.showerror("Input Error", "Target host required.")
            return
        self._reset_output()
        # Resolve target early
        try:
            target_ip = socket.gethostbyname(target)
        except socket.gaierror:
            messagebox.showerror("DNS Error", f"Could not resolve host: {target}")
            return
        # Determine ports
        ports: List[int]
        if ports_spec:
            try:
                ports = parse_port_spec(ports_spec)
            except ValueError as e:
                messagebox.showerror("Port Spec Error", str(e))
                return
        elif top_spec:
            try:
                n = int(top_spec)
                if n < 1:
                    raise ValueError
                ports = COMMON_PORTS_RANKED[:n]
            except ValueError:
                messagebox.showerror("Input Error", "Top N must be positive integer")
                return
        else:
            ports = list(range(1, 1025))
        self.total_ports = len(ports)
        self.completed_ports = 0
        self.stop_event.clear()
        json_path = Path(self.json_entry.get()) if self.json_entry.get() else None
        csv_path = Path(self.csv_entry.get()) if self.csv_entry.get() else None
        cfg = PortScanConfig(
            target=target_ip,
            ports=ports,
            threads=threads,
            timeout=timeout,
            resolve_services=self.services_var.get(),
            json_path=json_path,
            csv_path=csv_path,
            top=int(top_spec) if top_spec else None,
        )
        try:
            cfg.validate()
        except ValueError as e:
            messagebox.showerror("Config Error", str(e))
            return
        self.start_btn.config(state=tk.DISABLED)
        self.stop_btn.config(state=tk.NORMAL)
        self.status_var.set("Scanning...")
        self.log(f"Target {target_ip} | Ports {self.total_ports} | Threads {threads}")
        self.current_thread = threading.Thread(
            target=self._scan_worker, args=(cfg,), daemon=True
        )
        self.current_thread.start()

    def stop_scan(self):
        if self.current_thread and self.current_thread.is_alive():
            self.stop_event.set()
            self.status_var.set("Stopping...")
            self.log("Cancellation requested...")

    def _scan_worker(self, cfg: PortScanConfig):
        start = time.time()
        scanner = PortScanner(cfg, stop_event=self.stop_event)
        # Wrap original run to update progress via monkey patching _scan_single if needed
        orig_scan = scanner._scan_single

        def patched_scan(p: int):
            res = orig_scan(p)
            self.completed_ports += 1
            return res

        scanner._scan_single = patched_scan  # type: ignore
        try:
            open_ports = scanner.run()
        except Exception as e:
            self.log(f"ERROR: {e}")
            open_ports = []
        services_map = (
            resolve_services(open_ports)
            if cfg.resolve_services and open_ports
            else None
        )
        elapsed = time.time() - start
        if self.stop_event.is_set():
            self.log("Scan cancelled.")
        if open_ports:
            self.log("Open ports:")
            for p in open_ports:
                if services_map:
                    svc = services_map.get(p) or "-"
                    self.log(f"  {p:>5}  {svc}")
                else:
                    self.log(f"  {p:>5}")
        else:
            self.log("No open ports.")
        self.log(f"Elapsed: {elapsed:.2f}s")
        # Write outputs (GUI variant reimplements small part of CLI logic)
        if cfg.json_path:
            try:
                import json

                with open(cfg.json_path, "w", encoding="utf-8") as fh:
                    json.dump(
                        {
                            "target": cfg.target,
                            "scanned_ports": len(cfg.ports),
                            "open_ports": open_ports,
                            "open_ports_count": len(open_ports),
                            "services": services_map,
                            "elapsed_seconds": round(elapsed, 3),
                            "cancelled": self.stop_event.is_set(),
                        },
                        fh,
                        indent=2,
                    )
                self.log(f"JSON written: {cfg.json_path}")
            except OSError as e:
                self.log(f"JSON write failed: {e}")
        if cfg.csv_path and open_ports:
            try:
                import csv

                with open(cfg.csv_path, "w", newline="", encoding="utf-8") as fh:
                    writer = csv.writer(fh)
                    writer.writerow(["port", "service"])
                    for p in open_ports:
                        svc = services_map.get(p) if services_map else ""
                        writer.writerow([p, svc or ""])
                self.log(f"CSV written: {cfg.csv_path}")
            except OSError as e:
                self.log(f"CSV write failed: {e}")
        self.start_btn.config(state=tk.NORMAL)
        self.stop_btn.config(state=tk.DISABLED)
        self.status_var.set("Idle")

    def _reset_output(self):
        self.result_text.config(state=tk.NORMAL)
        self.result_text.delete("1.0", tk.END)
        self.result_text.config(state=tk.DISABLED)
        self.progress["value"] = 0


# ---------------- Main ---------------- #


def main():
    root = tk.Tk()
    app = PortScannerGUI(root)
    root.mainloop()


if __name__ == "__main__":  # pragma: no cover
    main()
