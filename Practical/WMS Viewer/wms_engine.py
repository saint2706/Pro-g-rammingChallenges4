"""Core WMS helpers shared by GUI implementations.

This module handles configuration loading, map-view math, disk caching, and
issuing OGC WMS `GetMap` requests.  The GUI layer (Tkinter, PyQt, etc.) can
remain thin by delegating the heavy lifting here.
"""
from __future__ import annotations

from dataclasses import dataclass, field
import hashlib
import json
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

import requests
from pyproj import CRS, Transformer
import yaml

# Constants specific to the common Web Mercator tiling scheme.
WEB_MERCATOR_BOUNDS = (
    -20037508.342789244,
    -20037508.342789244,
    20037508.342789244,
    20037508.342789244,
)
BASE_RESOLUTION = 156543.03392804097  # meters/pixel at zoom level 0


@dataclass
class LayerConfig:
    """Serializable configuration for an individual WMS layer."""

    name: str
    title: str = ""
    styles: str = ""
    format: str = "image/png"


@dataclass
class ServerConfig:
    """WMS server definition loaded from configuration."""

    name: str
    url: str
    version: str = "1.1.1"
    crs: str = "EPSG:3857"
    attribution: str = ""
    layers: List[LayerConfig] = field(default_factory=list)


@dataclass
class ViewConfig:
    width: int
    height: int
    default_zoom: int = 2
    default_center: Tuple[float, float] = (0.0, 0.0)  # lon, lat
    min_zoom: int = 0
    max_zoom: int = 18


@dataclass
class AppConfig:
    view: ViewConfig
    servers: List[ServerConfig]


class ConfigError(ValueError):
    """Raised when the configuration file is malformed."""


def load_config(path: Path) -> AppConfig:
    """Load the YAML configuration file."""

    with open(path, "r", encoding="utf-8") as handle:
        data = yaml.safe_load(handle)

    if not data or "servers" not in data:
        raise ConfigError("Configuration must include a 'servers' section")

    view_raw = data.get("view", {})
    view = ViewConfig(
        width=int(view_raw.get("width", 512)),
        height=int(view_raw.get("height", 512)),
        default_zoom=int(view_raw.get("default_zoom", 2)),
        default_center=tuple(view_raw.get("default_center", [0.0, 0.0])),
        min_zoom=int(view_raw.get("min_zoom", 0)),
        max_zoom=int(view_raw.get("max_zoom", 18)),
    )

    servers: List[ServerConfig] = []
    for server_raw in data["servers"]:
        layers_raw = server_raw.get("layers", [])
        if not layers_raw:
            raise ConfigError(f"Server '{server_raw.get('name', 'Unnamed')}' has no layers")
        layers = [
            LayerConfig(
                name=layer_raw["name"],
                title=layer_raw.get("title", layer_raw["name"]),
                styles=layer_raw.get("styles", ""),
                format=layer_raw.get("format", "image/png"),
            )
            for layer_raw in layers_raw
        ]
        servers.append(
            ServerConfig(
                name=server_raw["name"],
                url=server_raw["url"],
                version=server_raw.get("version", "1.1.1"),
                crs=server_raw.get("crs", "EPSG:3857"),
                attribution=server_raw.get("attribution", ""),
                layers=layers,
            )
        )

    return AppConfig(view=view, servers=servers)


class Projection:
    """Coordinate system helper built on top of pyproj."""

    def __init__(self, crs: str) -> None:
        self.crs = CRS.from_user_input(crs)
        self._to_xy = Transformer.from_crs("EPSG:4326", self.crs, always_xy=True)
        self._to_lonlat = Transformer.from_crs(self.crs, "EPSG:4326", always_xy=True)
        self.bounds = self._estimate_bounds()

    def _estimate_bounds(self) -> Optional[Tuple[float, float, float, float]]:
        if self.crs.to_epsg() == 3857:
            return WEB_MERCATOR_BOUNDS
        try:
            area = self.crs.area_of_use
            transformer = Transformer.from_crs("EPSG:4326", self.crs, always_xy=True)
            x_min, y_min = transformer.transform(area.west, area.south)
            x_max, y_max = transformer.transform(area.east, area.north)
            return (
                min(x_min, x_max),
                min(y_min, y_max),
                max(x_min, x_max),
                max(y_min, y_max),
            )
        except Exception:
            return None

    def lonlat_to_xy(self, lon: float, lat: float) -> Tuple[float, float]:
        return self._to_xy.transform(lon, lat)

    def xy_to_lonlat(self, x: float, y: float) -> Tuple[float, float]:
        return self._to_lonlat.transform(x, y)


class MapViewState:
    """Tracks pan/zoom state for a projected map view."""

    def __init__(
        self,
        width: int,
        height: int,
        projection: Projection,
        zoom: int = 2,
        min_zoom: int = 0,
        max_zoom: int = 18,
        center_lon: float = 0.0,
        center_lat: float = 0.0,
    ) -> None:
        self.width = width
        self.height = height
        self.projection = projection
        self.zoom = zoom
        self.min_zoom = min_zoom
        self.max_zoom = max_zoom
        self._set_center_xy(*self.projection.lonlat_to_xy(center_lon, center_lat))

    def _set_center_xy(self, x: float, y: float) -> None:
        if self.projection.bounds is None:
            self.center_x = x
            self.center_y = y
            return
        minx, miny, maxx, maxy = self.projection.bounds
        self.center_x = min(max(x, minx), maxx)
        self.center_y = min(max(y, miny), maxy)

    @property
    def resolution(self) -> float:
        return BASE_RESOLUTION / (2 ** self.zoom)

    def pan_pixels(self, dx: float, dy: float) -> None:
        res = self.resolution
        self._set_center_xy(self.center_x - dx * res, self.center_y - dy * res)

    def set_center_lonlat(self, lon: float, lat: float) -> None:
        self._set_center_xy(*self.projection.lonlat_to_xy(lon, lat))

    def zoom_to(self, zoom: int) -> None:
        self.zoom = max(self.min_zoom, min(self.max_zoom, zoom))

    def zoom_by(self, delta: int) -> None:
        self.zoom_to(self.zoom + delta)

    def resize(self, width: int, height: int) -> None:
        self.width = width
        self.height = height

    def bounding_box(self) -> Tuple[float, float, float, float]:
        res = self.resolution
        half_width = (self.width * res) / 2
        half_height = (self.height * res) / 2
        minx = self.center_x - half_width
        maxx = self.center_x + half_width
        miny = self.center_y - half_height
        maxy = self.center_y + half_height
        if self.projection.bounds is not None:
            bounds = self.projection.bounds
            minx = max(minx, bounds[0])
            miny = max(miny, bounds[1])
            maxx = min(maxx, bounds[2])
            maxy = min(maxy, bounds[3])
        return (minx, miny, maxx, maxy)

    def center_lonlat(self) -> Tuple[float, float]:
        return self.projection.xy_to_lonlat(self.center_x, self.center_y)


class WMSCache:
    """Simple disk cache for WMS image responses."""

    def __init__(self, cache_dir: Path) -> None:
        self.cache_dir = Path(cache_dir)
        self.cache_dir.mkdir(parents=True, exist_ok=True)

    def _key_to_path(self, key: Dict[str, object]) -> Path:
        digest = hashlib.sha1(json.dumps(key, sort_keys=True).encode("utf-8")).hexdigest()
        extension = str(key.get("format", "image/png")).split("/")[-1]
        subdir = self.cache_dir / digest[:2] / digest[2:4]
        subdir.mkdir(parents=True, exist_ok=True)
        return subdir / f"{digest}.{extension}"

    def get(self, key: Dict[str, object]) -> Optional[bytes]:
        path = self._key_to_path(key)
        if path.exists():
            return path.read_bytes()
        return None

    def set(self, key: Dict[str, object], data: bytes) -> bytes:
        path = self._key_to_path(key)
        path.write_bytes(data)
        return data


class WMSClient:
    """Fetches map imagery from a WMS server with caching."""

    def __init__(self, cache: WMSCache, timeout: int = 15) -> None:
        self.cache = cache
        self.timeout = timeout

    def build_params(
        self,
        server: ServerConfig,
        layer: LayerConfig,
        bbox: Sequence[float],
        size: Tuple[int, int],
    ) -> Dict[str, str]:
        width, height = size
        params = {
            "service": "WMS",
            "request": "GetMap",
            "version": server.version,
            "layers": layer.name,
            "styles": layer.styles,
            "format": layer.format,
            "srs": server.crs,
            "bbox": ",".join(f"{coord:.6f}" for coord in bbox),
            "width": str(width),
            "height": str(height),
            "transparent": "TRUE",
        }
        if server.version.startswith("1.3"):
            # Axis order swapped for some CRSs; users can override via config.
            params.pop("srs")
            params["crs"] = server.crs
        return params

    def fetch_map(
        self,
        server: ServerConfig,
        layer: LayerConfig,
        bbox: Sequence[float],
        size: Tuple[int, int],
    ) -> bytes:
        cache_key = {
            "url": server.url,
            "version": server.version,
            "layer": layer.name,
            "styles": layer.styles,
            "format": layer.format,
            "bbox": [round(c, 6) for c in bbox],
            "size": list(size),
            "crs": server.crs,
        }
        cached = self.cache.get(cache_key)
        if cached is not None:
            return cached

        params = self.build_params(server, layer, bbox, size)
        response = requests.get(server.url, params=params, timeout=self.timeout)
        response.raise_for_status()
        return self.cache.set(cache_key, response.content)


def ensure_cache_dir(base_path: Path) -> Path:
    """Utility to create a cache directory beside the executable."""
    cache_path = base_path / "cache"
    cache_path.mkdir(parents=True, exist_ok=True)
    return cache_path


def iter_layers(server: ServerConfig) -> Iterable[LayerConfig]:
    """Helper for flattening layer configs in the UI layer."""
    return server.layers
