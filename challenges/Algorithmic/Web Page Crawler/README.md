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

### Haskell crawler (`Crawler.hs`)
- Run the breadth-first crawler with JSON output (requires GHC plus packages `http-conduit`, `tagsoup`, `aeson`, and `optparse-applicative`; `bytestring`/`containers` ship with GHC):
  ```bash
  stack runghc Crawler.hs \
    --package http-conduit --package tagsoup --package aeson --package optparse-applicative \
    -- https://example.com --depth 1 --json
  ```
  With `cabal-install`, install the dependencies once (`cabal install http-conduit tagsoup aeson optparse-applicative`) and invoke `runghc Crawler.hs ...`.
- Respect robots.txt while rate limiting and exporting edges for the Python visualizer:
  ```bash
  runghc Crawler.hs https://example.com --robots --rate 0.5 --edges links.tsv --json
  ```
  The JSON summary mirrors `wpc.py`, and the tab-separated edge list (`src<TAB>dst`) feeds directly into `crawler_visualizer.py --edges links.tsv` or `build_graph_from_source`.

## Visualization
- Generate an interactive crawl map (requires `networkx` + `plotly`):
  ```bash
  python - <<'PY'
  from importlib import util
  from pathlib import Path

  base = Path(__file__).resolve().parents[2]
  viz_path = base / "challenges" / "Algorithmic" / "Web Page Crawler" / "crawler_visualizer.py"
  spec = util.spec_from_file_location("crawler_visualizer", viz_path)
  viz = util.module_from_spec(spec)
  assert spec and spec.loader
  spec.loader.exec_module(viz)

  graph, metadata = viz.build_graph_from_source({
      "start_url": "https://example.com",
      "edges": [
          ["https://example.com", "https://example.com/about"],
          ["https://example.com", "https://example.com/blog"],
      ],
  })
  figure = viz.build_plotly_figure(graph, metadata)
  output = viz.export_html(figure, Path("crawl_graph.html"))
  print(f"Saved visualization to {output}")
  PY
  ```
- Pass a running `WebCrawler` instance directly to `build_graph_from_source` to analyze live crawl results without saving a JSON file first.

## Debugging Tips
- Use `--log DEBUG` to trace enqueue/dequeue activity and HTTP status handling when diagnosing crawl gaps.
- Enable `--robots` to check compliance; the script logs disallowed paths so you can differentiate between 403s and policy skips.
- Network failures surface as categorized error messages in the JSON output; inspect these to tune timeouts or rate limits.

## Implementation Notes
- Dataclass-based configuration (Python) and `optparse-applicative` parsing (Haskell) normalize URLs, enforce argument constraints, and capture rate limiting/timeouts.
- Core crawlers use queues for BFS ordering and track visited URLs to prevent cycles; optional edge export writes `src -> dst` pairs consumed by the shared visualizer tooling.
- Python fetches pages via `requests` + BeautifulSoup; Haskell uses `http-conduit` for HTTP, TagSoup for HTML parsing, and emits equivalent JSON/edge data.

## Further Reading
- [Norvig & Russell, *Artificial Intelligence: A Modern Approach*, Section 3.5 (Graph Search)](https://aima.cs.berkeley.edu/)
- [Olston & Najork, "Web Crawling" (Foundations and Trends in IR, 2010)](https://doi.org/10.1561/1500000017)
