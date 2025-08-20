import socket
import threading
from queue import Queue
import argparse
import sys
from datetime import datetime

class PortScanner:
    """A multi-threaded port scanner."""
    def __init__(self, target: str, num_threads: int = 50):
        self.target = target
        self.num_threads = num_threads
        self.port_queue = Queue()
        self.open_ports = []
        self.lock = threading.Lock()

    def _scan_port(self, port: int):
        """
        Tries to connect to a specific port on the target host.
        If successful, the port is considered open.
        """
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.settimeout(1)
                if s.connect_ex((self.target, port)) == 0:
                    with self.lock:
                        self.open_ports.append(port)
        except (socket.timeout, socket.error):
            pass # Ignore ports that don't respond or cause errors

    def _worker(self):
        """The worker function for each thread. Pulls ports from the queue and scans them."""
        while not self.port_queue.empty():
            port = self.port_queue.get()
            self._scan_port(port)
            self.port_queue.task_done()

    def run_scan(self, ports: list[int]):
        """
        Starts the port scanning process using a pool of threads.
        """
        # Fill the queue with all ports to be scanned
        for port in ports:
            self.port_queue.put(port)

        threads = []
        for _ in range(self.num_threads):
            thread = threading.Thread(target=self._worker, daemon=True)
            threads.append(thread)
            thread.start()

        # Wait for all ports in the queue to be processed
        self.port_queue.join()

        # Ensure all threads have completed, though join() above should suffice
        for thread in threads:
            thread.join()

def main():
    """Main function to parse arguments and run the port scanner."""
    parser = argparse.ArgumentParser(description="A simple multi-threaded port scanner.")
    parser.add_argument("target", help="The target host to scan (e.g., 'example.com' or an IP address).")
    parser.add_argument("-p", "--ports", default="1-1024",
                        help="The port range to scan (e.g., '1-1024', '80,443'). Defaults to 1-1024.")
    parser.add_argument("-t", "--threads", type=int, default=100,
                        help="Number of threads to use for scanning. Defaults to 100.")

    args = parser.parse_args()

    try:
        target_ip = socket.gethostbyname(args.target)
    except socket.gaierror:
        print(f"Error: Hostname '{args.target}' could not be resolved.", file=sys.stderr)
        sys.exit(1)

    # Parse the port range
    try:
        ports_to_scan = []
        if '-' in args.ports:
            start, end = map(int, args.ports.split('-'))
            ports_to_scan = range(start, end + 1)
        else:
            ports_to_scan = [int(p) for p in args.ports.split(',')]
    except ValueError:
        print(f"Error: Invalid port specification. Use a range (e.g., 1-1024) or comma-separated values (e.g., 80,443).", file=sys.stderr)
        sys.exit(1)

    print("-" * 50)
    print(f"Scanning target: {args.target} ({target_ip})")
    print(f"Time started: {datetime.now()}")
    print("-" * 50)

    scanner = PortScanner(target_ip, args.threads)
    start_time = datetime.now()

    scanner.run_scan(ports_to_scan)

    end_time = datetime.now()

    print("\n--- Scan Complete ---")
    if scanner.open_ports:
        print("Open ports found:")
        for port in sorted(scanner.open_ports):
            print(f"  - Port {port} is open")
    else:
        print("No open ports found in the specified range.")

    print(f"\nScan duration: {end_time - start_time}")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nScan interrupted by user. Exiting.")
        sys.exit(0)
