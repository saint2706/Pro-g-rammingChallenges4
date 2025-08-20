import os
import requests
import urllib.parse
import concurrent.futures
import json
import argparse
import logging
from typing import List, Optional

# Add dependency checks for a better user experience.
try:
    from tqdm import tqdm
except ImportError:
    print("Error: The 'tqdm' library is required for the progress bar.", file=sys.stderr)
    print("Please install it using: pip install tqdm", file=sys.stderr)
    sys.exit(1)

# --- Setup Logging ---
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class BooruDownloader:
    """A concurrent image downloader for booru-style imageboards."""
    def __init__(self, base_url: str, download_path: str = "downloads", num_workers: int = 10):
        self.base_url = base_url
        self.download_path = download_path
        self.session = requests.Session()
        self.session.headers.update({'User-Agent': 'BooruDownloader/1.0'})
        self.num_workers = num_workers

    def get_image_urls(self, tags: str, limit: int) -> List[str]:
        """Fetches a list of image URLs from the booru API."""
        api_url = f"{self.base_url}/index.php?page=dapi&s=post&q=index&json=1"
        params = {'tags': tags, 'limit': limit}

        logging.info(f"Requesting image list from API with tags: {tags}")
        try:
            response = self.session.get(api_url, params=params)
            response.raise_for_status()
            data = response.json()

            if not data:
                logging.warning("API returned an empty response. No images found for the given tags.")
                return []

            # Handle different possible JSON structures (list vs. dict with 'post' key)
            posts = data if isinstance(data, list) else data.get("post", [])
            image_urls = [post.get("file_url") for post in posts if post.get("file_url")]

            logging.info(f"Found {len(image_urls)} images to download.")
            return image_urls

        except requests.exceptions.RequestException as e:
            logging.error(f"HTTP Error fetching data: {e}")
        except json.JSONDecodeError:
            logging.error("Error decoding JSON response. The site may not be a compatible booru.")
        return []

    def download_image(self, url: str) -> Optional[str]:
        """Downloads a single image from a URL."""
        if not url:
            return None

        filename = os.path.join(self.download_path, url.split("/")[-1])

        if os.path.exists(filename):
            return f"Skipped (already exists): {os.path.basename(filename)}"

        try:
            response = self.session.get(url, stream=True)
            response.raise_for_status()
            with open(filename, "wb") as f:
                for chunk in response.iter_content(chunk_size=8192):
                    f.write(chunk)
            return f"Downloaded: {os.path.basename(filename)}"
        except requests.exceptions.RequestException as e:
            return f"Failed to download {url}: {e}"

    def download_images(self, tags: str, limit: int):
        """
        Gets all image URLs and downloads them concurrently using a thread pool.
        """
        if not os.path.exists(self.download_path):
            os.makedirs(self.download_path)

        image_urls = self.get_image_urls(tags, limit)
        if not image_urls:
            return

        with concurrent.futures.ThreadPoolExecutor(max_workers=self.num_workers) as executor:
            # Use tqdm to create a progress bar
            results = list(tqdm(executor.map(self.download_image, image_urls), total=len(image_urls), desc="Downloading images"))

        logging.info("\n--- Download Summary ---")
        for res in results:
            if res: logging.info(res)

def main():
    """Main function to parse arguments and run the downloader."""
    parser = argparse.ArgumentParser(description="Download images from a booru-style imageboard.")
    parser.add_argument("tags", help="Tags to search for, separated by spaces (e.g., 'blue_hair 1girl').")
    parser.add_argument("-l", "--limit", type=int, default=100, help="Maximum number of images to download.")
    parser.add_argument("-d", "--path", default="downloads", help="Directory to save downloaded images.")
    parser.add_argument("-u", "--url", default="https://gelbooru.com", help="Base URL of the booru site.")
    parser.add_argument("-w", "--workers", type=int, default=10, help="Number of concurrent download threads.")

    args = parser.parse_args()

    downloader = BooruDownloader(base_url=args.url, download_path=args.path, num_workers=args.workers)
    downloader.download_images(tags=args.tags, limit=args.limit)

if __name__ == "__main__":
    main()
