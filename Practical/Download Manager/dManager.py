import click
import requests
import threading
import os
from typing import Optional

# Add dependency checks for a better user experience.
try:
    from tqdm import tqdm
except ImportError:
    print("Error: The 'tqdm' library is required for the progress bar.", file=sys.stderr)
    print("Please install it using: pip install tqdm", file=sys.stderr)
    sys.exit(1)

class Downloader:
    """A multi-threaded file downloader with progress bar."""
    def __init__(self, url: str, num_threads: int, output_filename: Optional[str] = None):
        self.url = url
        self.num_threads = num_threads
        self.output_filename = output_filename or self.url.split('/')[-1]

        try:
            self.response = requests.head(self.url, allow_redirects=True)
            self.file_size = int(self.response.headers.get('Content-Length', -1))
        except requests.exceptions.RequestException as e:
            print(f"Error: Could not connect to URL '{self.url}'. {e}", file=sys.stderr)
            sys.exit(1)

    def _download_chunk(self, start_byte: int, end_byte: int, pbar: tqdm):
        """Downloads a specific chunk of the file."""
        headers = {'Range': f'bytes={start_byte}-{end_byte}'}
        try:
            with requests.get(self.url, headers=headers, stream=True) as r:
                r.raise_for_status()
                with open(self.output_filename, "r+b") as f:
                    f.seek(start_byte)
                    for chunk in r.iter_content(chunk_size=8192):
                        f.write(chunk)
                        pbar.update(len(chunk))
        except (requests.exceptions.RequestException, IOError) as e:
            print(f"\nError downloading chunk: {e}", file=sys.stderr)

    def run(self):
        """Orchestrates the download process."""
        if self.file_size == -1 or 'bytes' not in self.response.headers.get('Accept-Ranges', ''):
            print("Server does not support ranged requests. Falling back to single-threaded download.")
            self._single_thread_download()
            return

        print(f"File size: {self.file_size / (1024 * 1024):.2f} MB")
        print(f"Using {self.num_threads} threads.")

        # Create an empty file of the correct size
        with open(self.output_filename, "wb") as f:
            f.truncate(self.file_size)

        part_size = self.file_size // self.num_threads
        threads = []

        with tqdm(total=self.file_size, unit='B', unit_scale=True, desc=self.output_filename) as pbar:
            for i in range(self.num_threads):
                start = i * part_size
                end = self.file_size - 1 if i == self.num_threads - 1 else start + part_size - 1

                thread = threading.Thread(target=self._download_chunk, args=(start, end, pbar))
                threads.append(thread)
                thread.start()

            for thread in threads:
                thread.join()

        print("\nDownload complete.")

    def _single_thread_download(self):
        """Fallback for servers that don't support ranged requests."""
        try:
            with requests.get(self.url, stream=True) as r:
                r.raise_for_status()
                with open(self.output_filename, 'wb') as f, \
                     tqdm(total=self.file_size, unit='B', unit_scale=True, desc=self.output_filename) as pbar:
                    for chunk in r.iter_content(chunk_size=8192):
                        f.write(chunk)
                        pbar.update(len(chunk))
            print("\nDownload complete.")
        except (requests.exceptions.RequestException, IOError) as e:
            print(f"Error during download: {e}", file=sys.stderr)

@click.command(help="A multi-threaded utility to download files quickly.")
@click.argument("url", type=str)
@click.option("-n", "--num_threads", default=8, help="Number of threads to use for downloading.")
@click.option("-o", "--output", default=None, help="Name of the output file.")
def main(url: str, num_threads: int, output: Optional[str]):
    """Main function to run the downloader from the command line."""
    downloader = Downloader(url, num_threads, output)
    downloader.run()

if __name__ == "__main__":
    main()
