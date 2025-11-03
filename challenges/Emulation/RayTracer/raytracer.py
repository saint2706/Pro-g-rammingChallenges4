"""Simple CPU-based ray tracer with JSON/CLI configurable scenes."""

from __future__ import annotations

import argparse
import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional, Sequence, Tuple

import numpy as np
from PIL import Image

Vector = np.ndarray

_EPSILON = 1e-5


def _normalize(vec: Vector) -> Vector:
    norm = np.linalg.norm(vec)
    if norm < _EPSILON:
        return vec
    return vec / norm


def _reflect(vector: Vector, normal: Vector) -> Vector:
    return vector - 2 * np.dot(vector, normal) * normal


def _to_array(values: Sequence[float]) -> Vector:
    return np.asarray(values, dtype=np.float64)


@dataclass(slots=True)
class Material:
    """Surface material properties."""

    color: Vector
    ambient: float = 0.1
    diffuse: float = 0.7
    specular: float = 0.5
    shininess: float = 32.0
    reflection: float = 0.0


@dataclass(slots=True)
class Light:
    position: Vector
    color: Vector = field(default_factory=lambda: _to_array((1.0, 1.0, 1.0)))
    intensity: float = 1.0


class SceneObject:
    material: Material

    def intersect(
        self, origin: Vector, direction: Vector
    ) -> Optional[Tuple[float, Vector]]:
        raise NotImplementedError


@dataclass(slots=True)
class Sphere(SceneObject):
    center: Vector
    radius: float
    material: Material

    def intersect(
        self, origin: Vector, direction: Vector
    ) -> Optional[Tuple[float, Vector]]:
        oc = origin - self.center
        a = np.dot(direction, direction)
        b = 2.0 * np.dot(oc, direction)
        c = np.dot(oc, oc) - self.radius * self.radius
        discriminant = b * b - 4 * a * c
        if discriminant < 0:
            return None
        sqrt_disc = math.sqrt(discriminant)
        denom = 2 * a
        t1 = (-b - sqrt_disc) / denom
        t2 = (-b + sqrt_disc) / denom
        t_hit = None
        if t1 > _EPSILON:
            t_hit = t1
        elif t2 > _EPSILON:
            t_hit = t2
        if t_hit is None:
            return None
        hit_point = origin + t_hit * direction
        normal = _normalize(hit_point - self.center)
        return t_hit, normal


@dataclass(slots=True)
class Plane(SceneObject):
    point: Vector
    normal: Vector
    material: Material

    def __post_init__(self) -> None:
        self.normal = _normalize(self.normal)

    def intersect(
        self, origin: Vector, direction: Vector
    ) -> Optional[Tuple[float, Vector]]:
        denom = np.dot(self.normal, direction)
        if abs(denom) < _EPSILON:
            return None
        t = np.dot(self.point - origin, self.normal) / denom
        if t < _EPSILON:
            return None
        return t, self.normal


@dataclass(slots=True)
class BoundingBox:
    minimum: Vector
    maximum: Vector

    def intersects(
        self, origin: Vector, direction: Vector, max_distance: float = float("inf")
    ) -> bool:
        inv_dir = np.where(np.abs(direction) > _EPSILON, 1.0 / direction, np.inf)
        t1 = (self.minimum - origin) * inv_dir
        t2 = (self.maximum - origin) * inv_dir
        lower = np.maximum.reduce(np.minimum(t1, t2))
        upper = np.minimum.reduce(np.maximum(t1, t2))
        if np.isnan(lower) or np.isnan(upper):
            return False
        if upper < 0.0 or lower > upper:
            return False
        return lower <= max_distance


@dataclass(slots=True)
class _AcceleratedObject:
    obj: SceneObject
    bbox: Optional[BoundingBox]


@dataclass(slots=True)
class Camera:
    position: Vector
    look_at: Vector
    up: Vector
    fov: float


@dataclass(slots=True)
class SceneConfig:
    camera: Camera
    objects: List[SceneObject]
    lights: List[Light]
    background_color: Vector
    max_depth: int = 2


class RayTracer:
    """CPU ray tracer with Phong shading and reflections."""

    def __init__(self, width: int, height: int, scene: SceneConfig):
        self.width = width
        self.height = height
        self.scene = scene
        self._accelerated_objects = self._build_accelerated_objects()
        self._setup_camera()

    def _setup_camera(self) -> None:
        camera = self.scene.camera
        self._forward = _normalize(camera.look_at - camera.position)
        self._right = _normalize(np.cross(self._forward, camera.up))
        self._up = _normalize(np.cross(self._right, self._forward))
        self._scale = math.tan(math.radians(camera.fov) / 2.0)
        self._aspect = self.width / self.height

    def render(self) -> Image.Image:
        image = np.zeros((self.height, self.width, 3), dtype=np.float64)
        for j in range(self.height):
            v = 1 - 2 * ((j + 0.5) / self.height)
            for i in range(self.width):
                u = 2 * ((i + 0.5) / self.width) - 1
                x = u * self._aspect * self._scale
                y = v * self._scale
                direction = _normalize(self._forward + x * self._right + y * self._up)
                color = self._trace_ray(self.scene.camera.position, direction, 0)
                image[j, i] = color
        image = np.clip(image * 255.0, 0, 255).astype(np.uint8)
        return Image.fromarray(image, mode="RGB")

    def _trace_ray(self, origin: Vector, direction: Vector, depth: int) -> Vector:
        hit = self._find_nearest(origin, direction)
        if hit is None:
            return self.scene.background_color
        distance, obj, normal = hit
        hit_point = origin + distance * direction
        view_dir = -direction
        color = self._shade(hit_point, normal, view_dir, obj.material)
        reflection = obj.material.reflection
        if reflection > 0 and depth < self.scene.max_depth:
            reflect_dir = _normalize(_reflect(direction, normal))
            offset_point = hit_point + normal * _EPSILON
            reflected = self._trace_ray(offset_point, reflect_dir, depth + 1)
            color = (1 - reflection) * color + reflection * reflected
        return color

    def _find_nearest(
        self, origin: Vector, direction: Vector
    ) -> Optional[Tuple[float, SceneObject, Vector]]:
        closest_t = float("inf")
        closest_obj: Optional[SceneObject] = None
        closest_normal: Optional[Vector] = None
        for entry in self._accelerated_objects:
            bbox = entry.bbox
            if bbox is not None and not bbox.intersects(origin, direction, closest_t):
                continue
            result = entry.obj.intersect(origin, direction)
            if result is None:
                continue
            t, normal = result
            if _EPSILON < t < closest_t:
                closest_t = t
                closest_obj = entry.obj
                closest_normal = normal
        if closest_obj is None or closest_normal is None:
            return None
        return closest_t, closest_obj, closest_normal

    def _shade(
        self, point: Vector, normal: Vector, view_dir: Vector, material: Material
    ) -> Vector:
        color = material.ambient * material.color
        for light in self.scene.lights:
            light_dir = _normalize(light.position - point)
            light_distance = np.linalg.norm(light.position - point)
            if self._is_shadowed(point, light_dir, light_distance):
                continue
            diff = max(np.dot(normal, light_dir), 0.0)
            diffuse = material.diffuse * diff
            reflect_dir = _normalize(_reflect(-light_dir, normal))
            spec = max(np.dot(reflect_dir, view_dir), 0.0) ** material.shininess
            specular = material.specular * spec
            diffuse_color = diffuse * material.color * light.color
            specular_color = specular * light.color
            color += light.intensity * (diffuse_color + specular_color)
        return np.clip(color, 0.0, 1.0)

    def _is_shadowed(
        self, point: Vector, light_dir: Vector, light_distance: float
    ) -> bool:
        origin = point + light_dir * _EPSILON
        for entry in self._accelerated_objects:
            bbox = entry.bbox
            if bbox is not None and not bbox.intersects(origin, light_dir, light_distance):
                continue
            result = entry.obj.intersect(origin, light_dir)
            if result is None:
                continue
            t, _ = result
            if _EPSILON < t < light_distance:
                return True
        return False

    def _build_accelerated_objects(self) -> List[_AcceleratedObject]:
        return [_AcceleratedObject(obj, self._bounding_box(obj)) for obj in self.scene.objects]

    @staticmethod
    def _bounding_box(obj: SceneObject) -> Optional[BoundingBox]:
        if isinstance(obj, Sphere):
            radius_vec = np.full(3, obj.radius, dtype=np.float64)
            return BoundingBox(obj.center - radius_vec, obj.center + radius_vec)
        if isinstance(obj, Plane):
            return None
        return None


def _parse_color(value: Sequence[float] | str) -> Vector:
    if isinstance(value, str):
        value = value.strip()
        if value.startswith("#"):
            value = value.lstrip("#")
            if len(value) != 6:
                raise ValueError("Hex colors must have 6 characters")
            r = int(value[0:2], 16)
            g = int(value[2:4], 16)
            b = int(value[4:6], 16)
            return _to_array((r, g, b)) / 255.0
        parts = [float(p) for p in value.split(",")]
        if len(parts) != 3:
            raise ValueError("Color must have three components")
        return _to_array(parts) / 255.0
    arr = _to_array(value)
    if arr.max() > 1.0:
        arr = arr / 255.0
    return arr


def _parse_vector(text: str) -> Vector:
    parts = [float(p) for p in text.split(",")]
    if len(parts) != 3:
        raise ValueError("Vector must have three components")
    return _to_array(parts)


def _build_material(tokens: Sequence[str]) -> Material:
    defaults = [0.1, 0.7, 0.5, 32.0, 0.0]
    ambient = float(tokens[1]) if len(tokens) > 1 else defaults[0]
    diffuse = float(tokens[2]) if len(tokens) > 2 else defaults[1]
    specular = float(tokens[3]) if len(tokens) > 3 else defaults[2]
    shininess = float(tokens[4]) if len(tokens) > 4 else defaults[3]
    reflection = float(tokens[5]) if len(tokens) > 5 else defaults[4]
    return Material(
        color=_parse_color(tokens[0]),
        ambient=ambient,
        diffuse=diffuse,
        specular=specular,
        shininess=shininess,
        reflection=reflection,
    )


def _parse_sphere(text: str) -> Sphere:
    parts = [p.strip() for p in text.split(";") if p.strip()]
    if len(parts) < 3:
        raise ValueError("Sphere definition requires center;radius;color")
    center = _parse_vector(parts[0])
    radius = float(parts[1])
    material = _build_material(parts[2:])
    return Sphere(center=center, radius=radius, material=material)


def _parse_plane(text: str) -> Plane:
    parts = [p.strip() for p in text.split(";") if p.strip()]
    if len(parts) < 3:
        raise ValueError("Plane definition requires point;normal;color")
    point = _parse_vector(parts[0])
    normal = _parse_vector(parts[1])
    material = _build_material(parts[2:])
    return Plane(point=point, normal=normal, material=material)


def _parse_light(text: str) -> Light:
    parts = [p.strip() for p in text.split(";") if p.strip()]
    if not parts:
        raise ValueError("Light definition cannot be empty")
    position = _parse_vector(parts[0])
    color = _parse_color(parts[1]) if len(parts) > 1 else _to_array((1.0, 1.0, 1.0))
    intensity = float(parts[2]) if len(parts) > 2 else 1.0
    return Light(position=position, color=color, intensity=intensity)


def load_scene_dict(data: dict) -> SceneConfig:
    camera_data = data.get("camera") or {}
    camera = Camera(
        position=_to_array(camera_data.get("position", (0.0, 0.0, -5.0))),
        look_at=_to_array(camera_data.get("look_at", (0.0, 0.0, 0.0))),
        up=_to_array(camera_data.get("up", (0.0, 1.0, 0.0))),
        fov=float(camera_data.get("fov", 45.0)),
    )
    objects: List[SceneObject] = []
    for obj in data.get("objects", []):
        obj_type = obj.get("type", "").lower()
        material = _build_material_from_dict(obj.get("material") or {})
        if obj_type == "sphere":
            objects.append(
                Sphere(
                    center=_to_array(obj.get("center", (0.0, 0.0, 0.0))),
                    radius=float(obj.get("radius", 1.0)),
                    material=material,
                )
            )
        elif obj_type == "plane":
            objects.append(
                Plane(
                    point=_to_array(obj.get("point", (0.0, -1.0, 0.0))),
                    normal=_to_array(obj.get("normal", (0.0, 1.0, 0.0))),
                    material=material,
                )
            )
        else:
            raise ValueError(f"Unsupported object type: {obj_type}")
    lights: List[Light] = []
    for light in data.get("lights", []):
        lights.append(
            Light(
                position=_to_array(light.get("position", (0.0, 5.0, -5.0))),
                color=_parse_color(light.get("color", (255, 255, 255))),
                intensity=float(light.get("intensity", 1.0)),
            )
        )
    background = _parse_color(data.get("background", (30, 30, 40)))
    max_depth = int(data.get("max_depth", 2))
    if not objects:
        raise ValueError("Scene must define at least one object")
    if not lights:
        raise ValueError("Scene must define at least one light")
    return SceneConfig(
        camera=camera,
        objects=objects,
        lights=lights,
        background_color=background,
        max_depth=max_depth,
    )


def _build_material_from_dict(data: dict) -> Material:
    color = _parse_color(data.get("color", (255, 255, 255)))
    return Material(
        color=color,
        ambient=float(data.get("ambient", 0.1)),
        diffuse=float(data.get("diffuse", 0.7)),
        specular=float(data.get("specular", 0.5)),
        shininess=float(data.get("shininess", 32.0)),
        reflection=float(data.get("reflection", 0.0)),
    )


def load_scene_file(path: Path | str) -> SceneConfig:
    with Path(path).open("r", encoding="utf-8") as handle:
        data = json.load(handle)
    return load_scene_dict(data)


def _build_scene_from_cli(args: argparse.Namespace) -> SceneConfig:
    fov = float(args.fov) if args.fov is not None else 45.0
    camera = Camera(
        position=(
            _parse_vector(args.camera_position)
            if args.camera_position
            else _to_array((0.0, 1.0, -5.0))
        ),
        look_at=(
            _parse_vector(args.camera_look_at)
            if args.camera_look_at
            else _to_array((0.0, 0.5, 0.0))
        ),
        up=(
            _parse_vector(args.camera_up)
            if args.camera_up
            else _to_array((0.0, 1.0, 0.0))
        ),
        fov=fov,
    )
    objects: List[SceneObject] = []
    for text in args.sphere or []:
        objects.append(_parse_sphere(text))
    for text in args.plane or []:
        objects.append(_parse_plane(text))
    if not objects:
        # default: two spheres and ground plane
        objects.extend(
            [
                Sphere(
                    center=_to_array((0.0, 0.5, 0.0)),
                    radius=0.5,
                    material=Material(color=_parse_color("#ff5555"), reflection=0.2),
                ),
                Sphere(
                    center=_to_array((-1.0, 0.35, 1.0)),
                    radius=0.35,
                    material=Material(
                        color=_parse_color("#4f83ff"), reflection=0.1, shininess=40.0
                    ),
                ),
                Plane(
                    point=_to_array((0.0, 0.0, 0.0)),
                    normal=_to_array((0.0, 1.0, 0.0)),
                    material=Material(
                        color=_parse_color((210, 210, 210)),
                        ambient=0.2,
                        diffuse=0.7,
                        specular=0.1,
                    ),
                ),
            ]
        )
    lights: List[Light] = []
    for text in args.light or []:
        lights.append(_parse_light(text))
    if not lights:
        lights.append(
            Light(
                position=_to_array((5.0, 5.0, -10.0)),
                color=_to_array((1.0, 1.0, 1.0)),
                intensity=1.2,
            )
        )
        lights.append(
            Light(
                position=_to_array((-3.0, 4.0, -2.0)),
                color=_to_array((1.0, 0.9, 0.8)),
                intensity=0.6,
            )
        )
    background = (
        _parse_color(args.background) if args.background else _parse_color((25, 30, 40))
    )
    max_depth = int(args.max_depth)
    return SceneConfig(
        camera=camera,
        objects=objects,
        lights=lights,
        background_color=background,
        max_depth=max_depth,
    )


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="CPU-based ray tracer (spheres + planes)."
    )
    parser.add_argument("--scene", type=Path, help="Path to JSON scene description.")
    parser.add_argument(
        "--output", type=Path, default=Path("render.png"), help="Output image path."
    )
    parser.add_argument(
        "--width", type=int, default=400, help="Render width in pixels."
    )
    parser.add_argument(
        "--height", type=int, default=300, help="Render height in pixels."
    )
    parser.add_argument(
        "--max-depth", type=int, default=2, help="Maximum reflection depth."
    )
    parser.add_argument(
        "--background", type=str, help="Background color (hex or r,g,b)."
    )
    parser.add_argument("--fov", type=float, help="Field of view in degrees.")
    parser.add_argument(
        "--camera-position", type=str, help="Camera position vector (x,y,z)."
    )
    parser.add_argument(
        "--camera-look-at", type=str, help="Camera look-at vector (x,y,z)."
    )
    parser.add_argument("--camera-up", type=str, help="Camera up vector (x,y,z).")
    parser.add_argument(
        "--sphere",
        action="append",
        help="Sphere definition center;radius;color;ambient;diffuse;specular;shininess;reflection",
    )
    parser.add_argument(
        "--plane",
        action="append",
        help="Plane definition point;normal;color;ambient;diffuse;specular;shininess;reflection",
    )
    parser.add_argument(
        "--light", action="append", help="Light definition position;color;intensity"
    )
    parser.add_argument(
        "--show", action="store_true", help="Open the rendered image after saving."
    )
    return parser


def main(argv: Optional[Sequence[str]] = None) -> None:
    parser = build_arg_parser()
    args = parser.parse_args(argv)
    if args.scene is not None:
        try:
            scene = load_scene_file(args.scene)
        except FileNotFoundError as exc:
            parser.error(f"Scene file not found: {exc}")
        except json.JSONDecodeError as exc:
            parser.error(
                f"Failed to parse scene file '{args.scene}': {exc.msg} (line {exc.lineno} column {exc.colno})"
            )
        # override with CLI additions
        if args.sphere:
            for text in args.sphere:
                scene.objects.append(_parse_sphere(text))
        if args.plane:
            for text in args.plane:
                scene.objects.append(_parse_plane(text))
        if args.light:
            for text in args.light:
                scene.lights.append(_parse_light(text))
        if args.background:
            scene.background_color = _parse_color(args.background)
        scene.max_depth = int(args.max_depth)
        if (
            args.camera_position
            or args.camera_look_at
            or args.camera_up
            or args.fov is not None
        ):
            scene.camera = Camera(
                position=(
                    _parse_vector(args.camera_position)
                    if args.camera_position
                    else scene.camera.position
                ),
                look_at=(
                    _parse_vector(args.camera_look_at)
                    if args.camera_look_at
                    else scene.camera.look_at
                ),
                up=_parse_vector(args.camera_up) if args.camera_up else scene.camera.up,
                fov=float(args.fov) if args.fov is not None else scene.camera.fov,
            )
        width = args.width
        height = args.height
    else:
        scene = _build_scene_from_cli(args)
        width = args.width
        height = args.height
    tracer = RayTracer(width=width, height=height, scene=scene)
    image = tracer.render()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    image.save(args.output)
    if args.show:
        image.show()


if __name__ == "__main__":  # pragma: no cover
    main()
