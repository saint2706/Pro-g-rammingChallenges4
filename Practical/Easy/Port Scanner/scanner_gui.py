import tkinter as tk
from tkinter import ttk, scrolledtext, messagebox
import threading
import queue
import sys
import socket

# Import the logic from the refactored command-line script
try:
    from scanner import PortScanner
except ImportError:
    print("Error: This GUI requires the 'scanner.py' script to be in the same directory.", file=sys.stderr)
    sys.exit(1)

class PortScannerGUI:
    """A Tkinter GUI for the multi-threaded Port Scanner."""
    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("Multi-threaded Port Scanner")

        # Queue for thread-safe communication from worker threads to the GUI
        self.gui_queue = queue.Queue()

        self._create_widgets()

    def _create_widgets(self):
        """Creates and lays out all the GUI widgets."""
        main_frame = ttk.Frame(self.master, padding="10")
        main_frame.pack(fill=tk.BOTH, expand=True)

        # --- Input Frame ---
        input_frame = ttk.LabelFrame(main_frame, text="Scan Configuration", padding="10")
        input_frame.pack(fill=tk.X)

        ttk.Label(input_frame, text="Target Host:").grid(row=0, column=0, sticky=tk.W, pady=2)
        self.target_entry = ttk.Entry(input_frame, width=30)
        self.target_entry.insert(0, "scanme.nmap.org") # A host for safe scanning practice
        self.target_entry.grid(row=0, column=1, sticky=tk.EW, padx=5)

        ttk.Label(input_frame, text="Port Range:").grid(row=1, column=0, sticky=tk.W, pady=2)
        self.ports_entry = ttk.Entry(input_frame, width=30)
        self.ports_entry.insert(0, "1-100")
        self.ports_entry.grid(row=1, column=1, sticky=tk.EW, padx=5)

        ttk.Label(input_frame, text="Threads:").grid(row=2, column=0, sticky=tk.W, pady=2)
        self.threads_entry = ttk.Entry(input_frame, width=10)
        self.threads_entry.insert(0, "100")
        self.threads_entry.grid(row=2, column=1, sticky=tk.W, padx=5)

        input_frame.columnconfigure(1, weight=1)

        # --- Action Button ---
        self.scan_button = ttk.Button(main_frame, text="Start Scan", command=self.start_scan)
        self.scan_button.pack(pady=10)

        # --- Results Display ---
        results_frame = ttk.LabelFrame(main_frame, text="Results", padding="10")
        results_frame.pack(fill=tk.BOTH, expand=True)

        self.result_text = scrolledtext.ScrolledText(results_frame, wrap=tk.WORD, state=tk.DISABLED, font=("Courier New", 10))
        self.result_text.pack(fill=tk.BOTH, expand=True)

    def start_scan(self):
        """Validates input and starts the scanning process in a new thread."""
        target = self.target_entry.get()
        ports_str = self.ports_entry.get()
        try:
            threads = int(self.threads_entry.get())
            if threads < 1: raise ValueError("Threads must be > 0")
        except ValueError:
            messagebox.showerror("Input Error", "Number of threads must be a positive integer.")
            return

        self.scan_button.config(state=tk.DISABLED)
        self.log_message("Starting scan...")

        # Run the scanner in a separate thread to keep the GUI responsive
        scan_thread = threading.Thread(
            target=self.run_scanner_thread,
            args=(target, ports_str, threads),
            daemon=True
        )
        scan_thread.start()

        # Start checking the queue for messages from the scanner thread
        self.master.after(100, self.process_queue)

    def run_scanner_thread(self, target: str, ports_str: str, num_threads: int):
        """The actual scanning logic that runs in the background."""
        try:
            target_ip = socket.gethostbyname(target)
            self.log_message(f"Host '{target}' resolved to {target_ip}")
        except socket.gaierror:
            self.log_message(f"ERROR: Hostname '{target}' could not be resolved.")
            self.log_message("--- Scan Finished ---")
            return

        try:
            ports_to_scan = []
            if '-' in ports_str:
                start, end = map(int, ports_str.split('-'))
                ports_to_scan = range(start, end + 1)
            else:
                ports_to_scan = [int(p) for p in ports_str.split(',')]
        except ValueError:
            self.log_message("ERROR: Invalid port specification.")
            self.log_message("--- Scan Finished ---")
            return

        # This is a thread-safe way to run the scanner and get results
        # We create a local queue for this scan instance
        port_queue = queue.Queue()
        for port in ports_to_scan:
            port_queue.put(port)

        open_ports = []
        lock = threading.Lock()

        def worker():
            while not port_queue.empty():
                port = port_queue.get()
                try:
                    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                        s.settimeout(0.5)
                        if s.connect_ex((target_ip, port)) == 0:
                            with lock:
                                open_ports.append(port)
                finally:
                    port_queue.task_done()

        for _ in range(num_threads):
            threading.Thread(target=worker, daemon=True).start()

        port_queue.join() # Wait for all ports to be scanned

        for port in sorted(open_ports):
            self.log_message(f"Port {port} is open")

        self.log_message("--- Scan Finished ---")

    def log_message(self, message: str):
        """Puts a message into the thread-safe queue for the GUI to display."""
        self.gui_queue.put(message)

    def process_queue(self):
        """Checks the GUI queue for messages and updates the text widget."""
        try:
            while True:
                msg = self.gui_queue.get_nowait()
                self.result_text.config(state=tk.NORMAL)
                if msg == "--- Scan Finished ---":
                    self.scan_button.config(state=tk.NORMAL)
                    self.result_text.insert(tk.END, msg + "\n")
                else:
                    self.result_text.insert(tk.END, msg + "\n")
                self.result_text.see(tk.END)
                self.result_text.config(state=tk.DISABLED)
                if msg == "--- Scan Finished ---":
                    return # Stop polling
        except queue.Empty:
            pass

        self.master.after(100, self.process_queue)

def main():
    root = tk.Tk()
    app = PortScannerGUI(root)
    root.mainloop()

if __name__ == "__main__":
    main()
