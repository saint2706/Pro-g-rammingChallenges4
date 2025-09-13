import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin, urlparse
import sys
import argparse
from collections import deque

class WebCrawler:
    """
    A simple web crawler that starts from a URL, finds all links on the page,
    and recursively visits them up to a specified depth.
    """
    def __init__(self, start_url: str, max_depth: int = 2):
        self.start_url = start_url
        self.root_domain = urlparse(start_url).netloc
        self.max_depth = max_depth
        self.visited = set()
        self.queue = deque([(start_url, 0)])  # Queue stores (url, depth)

    def crawl(self):
        """Starts the crawling process."""
        while self.queue:
            url, depth = self.queue.popleft()

            if url in self.visited or depth > self.max_depth:
                continue

            try:
                self.visited.add(url)
                print(f"[{depth}] Crawling: {url}")

                response = requests.get(url, timeout=5)
                response.raise_for_status() # Raises an HTTPError for bad responses (4xx or 5xx)

                # Ensure we are only parsing HTML content
                if 'text/html' not in response.headers.get('Content-Type', ''):
                    print(f"  -> Skipping non-HTML content at {url}")
                    continue

                soup = BeautifulSoup(response.content, "html.parser")

                # Find all links and add them to the queue if they are valid
                self._find_and_queue_links(soup, url, depth)

            except requests.exceptions.RequestException as e:
                print(f"  -> Failed to fetch {url}: {e}")
            except Exception as e:
                print(f"  -> An unexpected error occurred at {url}: {e}")

    def _find_and_queue_links(self, soup: BeautifulSoup, base_url: str, current_depth: int):
        """Finds all links on a page and adds valid ones to the crawl queue."""
        for link in soup.find_all("a", href=True):
            href = link.get("href")
            # Resolve relative URLs (e.g., "/about") into absolute URLs
            absolute_url = urljoin(base_url, href)

            # Parse the URL to remove fragments and query parameters if desired
            parsed_url = urlparse(absolute_url)
            clean_url = parsed_url._replace(fragment="", params="", query="").geturl()

            # Add the link to the queue if it's within the same domain and not yet visited
            if self._is_valid_url(clean_url):
                if clean_url not in self.visited and clean_url not in [item[0] for item in self.queue]:
                    self.queue.append((clean_url, current_depth + 1))

    def _is_valid_url(self, url: str) -> bool:
        """Checks if a URL is valid for crawling (same domain, http/https)."""
        parsed = urlparse(url)
        # Check if the scheme is http/https and if it's in the same domain as the start URL
        return parsed.scheme in ["http", "https"] and parsed.netloc == self.root_domain

def main():
    """Main function to configure and run the web crawler."""
    parser = argparse.ArgumentParser(description="A simple web crawler.")
    parser.add_argument("url", help="The starting URL to crawl.")
    parser.add_argument("-d", "--depth", type=int, default=2, help="The maximum crawl depth.")

    args = parser.parse_args()

    if not urlparse(args.url).scheme:
        args.url = "http://" + args.url

    print(f"Starting crawl at {args.url} with max depth {args.depth}")
    crawler = WebCrawler(start_url=args.url, max_depth=args.depth)
    try:
        crawler.crawl()
    except KeyboardInterrupt:
        print("\nCrawling interrupted by user.")
    finally:
        print("\nCrawl finished.")
        print(f"Visited {len(crawler.visited)} unique pages.")

if __name__ == "__main__":
    main()
