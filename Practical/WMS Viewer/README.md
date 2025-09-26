# Desktop WMS Viewer

This project implements challenge 144 from the practical section: a desktop viewer for [OGC Web Map Service (WMS)](https://www.ogc.org/standards/wms) endpoints. The viewer is implemented with Tkinter and demonstrates an offline-capable map experience backed by disk caching.

## Features

- **OGC compliant map requests** – generates `GetMap` requests against WMS 1.1.1 endpoints with configurable layers, coordinate reference systems, and styles.
- **Interactive navigation** – pan by dragging with the mouse, zoom with the scroll wheel or dedicated buttons, and switch between configured layers/endpoints.
- **Disk-backed cache** – tiles are cached locally the first time they are requested. Subsequent views reuse the cached PNG image to provide an offline-capable workflow.
- **Configurable coordinate systems** – default setup targets EPSG:3857 (Web Mercator), while the YAML configuration allows alternative CRSs per server or layer.
- **Reusable engine** – the rendering and fetching logic lives in `wms_engine.py`, making it easy to integrate into other desktop frameworks such as PyQt without rewriting the networking layer.

## Project Layout

```
Practical/WMS Viewer/
├── config.yaml           # Endpoint, layer, CRS, and display defaults
├── wms_engine.py         # Tile math, caching, and WMS client helpers
├── wms_viewer.py         # Tkinter GUI entry point
├── README.md             # This document
└── tests/
    └── test_wms_engine.py
```

## Requirements

Install dependencies with `pip install -r ../../Practical/requirements.txt`. The viewer relies on:

- `requests` – HTTP client for WMS `GetMap` calls.
- `pyproj` – coordinate transformations between WGS84 and target projected systems.
- `Pillow` – image decoding for displaying PNG/JPEG map tiles.
- `tkinter` – included with the standard CPython distribution on most platforms.

## Usage

1. Adjust `config.yaml` to include your WMS servers, preferred coordinate reference system, and default view.
2. Run the viewer:

   ```bash
   python wms_viewer.py
   ```

3. Interact with the map:
   - **Drag** with the left mouse button to pan.
   - **Scroll** the mouse wheel or use the `+`/`-` buttons to zoom.
   - **Switch servers/layers** using the dropdown menus at the top of the window.

Map responses are cached to `cache/` in the project directory. Delete this folder to clear stale tiles.

## Configuration

The provided `config.yaml` includes two public demo servers. Each server entry lets you set:

- `name`: Friendly label displayed in the UI.
- `url`: Base WMS endpoint.
- `version`: WMS protocol version (defaults to 1.1.1 for XY axis order).
- `crs`: Coordinate reference system used for map requests.
- `layers`: List of dictionaries with `name`, `title`, `styles`, and optional `format` overrides.
- `attribution`: Credit displayed in the status bar.

You can add additional endpoints or override defaults per server.

## Testing

Unit tests cover cache lookups, bounding-box math, and request construction without requiring live network access. Run them with:

```bash
python -m unittest discover -s tests -t .
```

The tests use mocked HTTP responses, so they do not rely on external WMS availability.

## Offline and Standards Notes

- The viewer speaks the OGC WMS `GetMap` protocol. Extend it with `GetCapabilities` parsing if you need dynamic layer discovery.
- Cached tiles are keyed by server, layer, bounding box, CRS, and requested image size. Stale tiles can be purged by deleting individual cache files.
- For true offline workflows, pre-warm the cache by iterating over the bounding boxes you anticipate needing (e.g., via a script that calls `WMSClient.fetch_map`).

## Extending to PyQt

Although the bundled GUI uses Tkinter to avoid extra dependencies, `wms_engine.py` is GUI-agnostic. You can integrate it into a PyQt `QGraphicsView` by:

1. Reusing `MapViewState` for pan/zoom math.
2. Loading cached tiles through `WMSClient`.
3. Displaying the returned bytes with `QPixmap.loadFromData`.

This separation keeps the networking and projection logic consistent across toolkits.

## Challenge Status

This implementation marks challenge 144 – "WMS viewer that isn't web based" – as complete.
