# Web Page Crawler

## Problem Statement
Perform a breadth-first crawl starting from a seed URL, collecting hyperlinks up to a configurable depth while respecting optional domain, robots.txt, and rate-limit constraints.

## Usage
- Crawl a single domain to depth 1 and print a JSON summary:
  ```bash
  python wpc.py https://example.com --depth 1 --json
  ```
- Allow cross-domain traversal with a modest rate limit:
  ```bash
  python wpc.py example.com --no-same-domain --rate 0.5 --max-pages 100
  ```
- Export the discovered edges to a text file:
  ```bash
  python wpc.py https://example.com --depth 2 --edges links.txt
  ```

## Debugging Tips
- Use `--log DEBUG` to trace enqueue/dequeue activity and HTTP status handling when diagnosing crawl gaps.
- Enable `--robots` to check compliance; the script logs disallowed paths so you can differentiate between 403s and policy skips.
- Network failures surface as categorized error messages in the JSON output; inspect these to tune timeouts or rate limits.

## Implementation Notes
- Dataclass-based configuration normalizes URLs, enforces argument constraints, and captures rate limiting/timeouts.
- Core crawler uses a deque for BFS ordering and tracks visited URLs to prevent cycles; optional edge export writes `src -> dst` pairs.
- Relies on `requests` for HTTP fetching and BeautifulSoup for HTML parsing while filtering by content type/size.

## Further Reading
- [Norvig & Russell, *Artificial Intelligence: A Modern Approach*, Section 3.5 (Graph Search)](https://aima.cs.berkeley.edu/)
- [Olston & Najork, "Web Crawling" (Foundations and Trends in IR, 2010)](https://doi.org/10.1561/1500000017)
