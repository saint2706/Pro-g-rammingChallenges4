"""Retro multi-effect demo with synchronized audio timeline."""

from __future__ import annotations

import argparse
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Iterable, List, Optional

import numpy as np
import pygame

try:  # Optional dependency used when exporting animated GIFs
    import imageio.v2 as imageio
except Exception:  # pragma: no cover - imageio is optional at runtime
    imageio = None


# ---------------------------------------------------------------------------
# Configuration data classes
# ---------------------------------------------------------------------------


@dataclass(slots=True)
class DemoConfig:
    """Runtime configuration for the demo."""

    width: int = 960
    height: int = 540
    fps: int = 60
    bpm: int = 120
    duration: float = 32.0
    sample_rate: int = 44_100
    capture_frames: Optional[Path] = None
    capture_stride: int = 2
    capture_gif: Optional[Path] = None
    disable_audio: bool = False
    fullscreen: bool = False
    window_title: str = "Old School Multi-Effect Demo"
    overlay: bool = True
    headless: bool = False
    stop_after: Optional[float] = None

    def resolution(self) -> tuple[int, int]:
        return (self.width, self.height)


@dataclass(slots=True)
class Segment:
    """Definition of a timeline segment."""

    name: str
    factory: Callable[[DemoConfig], "Effect"]
    duration: float
    fade_in: float = 0.6
    fade_out: float = 0.8
    metadata: dict[str, object] = field(default_factory=dict)


@dataclass(slots=True)
class SegmentState:
    """Mutable runtime state for a segment."""

    spec: Segment
    start: float
    effect: Optional["Effect"] = None
    started: bool = False
    finished: bool = False

    @property
    def end(self) -> float:
        return self.start + self.spec.duration

    def fade_window(self) -> tuple[float, float]:
        """Return (fade_in_start, fade_out_end)."""
        return (
            self.start - self.spec.fade_in,
            self.end + self.spec.fade_out,
        )


class Timeline:
    """Sequence of demo segments with deterministic start times."""

    def __init__(self, cfg: DemoConfig, segments: Iterable[Segment]):
        self.cfg = cfg
        self.segments: List[SegmentState] = []
        current_start = 0.0
        for seg in segments:
            state = SegmentState(spec=seg, start=current_start)
            self.segments.append(state)
            current_start += seg.duration
        self.total_duration = current_start

    def instantiate_effects(self) -> None:
        for state in self.segments:
            if state.effect is None:
                state.effect = state.spec.factory(self.cfg)

    def active_states(self, t: float) -> List[SegmentState]:
        """Return segments that should be rendered at time ``t`` including fades."""
        active: List[SegmentState] = []
        for state in self.segments:
            fade_in_start, fade_out_end = state.fade_window()
            if fade_in_start <= t <= fade_out_end:
                active.append(state)
        return active

    def seek(self, index: int) -> float:
        """Return the start time of a given segment index."""
        index = max(0, min(index, len(self.segments) - 1))
        return self.segments[index].start


# ---------------------------------------------------------------------------
# Beat clock & audio synthesis
# ---------------------------------------------------------------------------


class BeatClock:
    """Compute beat events from elapsed time."""

    def __init__(self, bpm: int):
        self.bpm = bpm
        self.seconds_per_beat = 60.0 / bpm
        self.current_index = -1

    def update(self, elapsed: float) -> List[int]:
        """Return beat indices that elapsed since the previous update."""
        beat_idx = int(math.floor(elapsed / self.seconds_per_beat))
        if beat_idx <= self.current_index:
            return []
        beats = list(range(self.current_index + 1, beat_idx + 1))
        self.current_index = beat_idx
        return beats


def generate_audio_track(cfg: DemoConfig, length: float) -> np.ndarray:
    """Create a simple synthwave-inspired stereo track for the given duration."""

    sample_count = int(length * cfg.sample_rate)
    t = np.linspace(0.0, length, sample_count, endpoint=False)
    beat_period = 60.0 / cfg.bpm

    # Bassline: simple saw wave on quarter notes
    bass_freqs = np.array([55, 65, 73, 82, 65, 73, 82, 98], dtype=float)
    bass = np.zeros_like(t)
    for i, freq in enumerate(bass_freqs):
        start = i * beat_period
        mask = (t >= start) & (t < start + beat_period)
        local_t = t[mask] - start
        env = np.exp(-local_t * 3.0)
        bass[mask] = 0.45 * env * np.sin(2 * math.pi * freq * local_t)

    # Lead arpeggio sweeps across the timeline using a triangle wave
    lead_freq = 440 + 80 * np.sin(2 * math.pi * t / (beat_period * 4))
    lead = 0.25 * np.sign(np.sin(2 * math.pi * lead_freq * t))

    # Hi-hat ticks on every 8th beat
    hat = np.zeros_like(t)
    hat_period = beat_period / 2
    phases = t % hat_period
    hat += (
        0.2
        * np.exp(-phases * 40)
        * np.random.default_rng(42).uniform(-1, 1, size=t.shape)
    )

    mix = bass + lead + hat
    mix = np.clip(mix, -1.0, 1.0)
    stereo = np.repeat((mix * 0.8)[:, None], 2, axis=1)
    return (stereo * np.iinfo(np.int16).max).astype(np.int16)


# ---------------------------------------------------------------------------
# Effect base class
# ---------------------------------------------------------------------------


class Effect:
    """Base class for visual effects."""

    def __init__(self, cfg: DemoConfig):
        self.cfg = cfg
        self.surface = pygame.Surface(cfg.resolution()).convert()

    def on_enter(self, metadata: dict[str, object]) -> None:
        pass

    def on_exit(self) -> None:
        pass

    def on_beat(self, beat_index: int) -> None:
        pass

    def render(self, timeline_t: float, dt: float) -> pygame.Surface:
        raise NotImplementedError


# ---------------------------------------------------------------------------
# Individual demo effects
# ---------------------------------------------------------------------------


class PlasmaTunnelEffect(Effect):
    """Color-cycling plasma tunnel."""

    def __init__(self, cfg: DemoConfig):
        super().__init__(cfg)
        w, h = cfg.resolution()
        xs = np.linspace(-1.6, 1.6, w, dtype=np.float32)
        ys = np.linspace(-1.0, 1.0, h, dtype=np.float32)
        self.yy, self.xx = np.meshgrid(ys, xs, indexing="ij")
        self.radius = np.sqrt(self.xx**2 + self.yy**2) + 1e-6
        self.angle = np.arctan2(self.yy, self.xx)
        self.lfo = 0.0

    def on_beat(self, beat_index: int) -> None:
        # Subtle zoom pulse on every beat
        self.lfo = min(self.lfo + 0.12, 0.4)

    def render(self, timeline_t: float, dt: float) -> pygame.Surface:
        self.lfo = max(self.lfo - dt * 0.15, 0.0)
        t = timeline_t * 1.7
        depth = 1.0 / (self.radius + 0.3 + self.lfo)
        swirl = np.sin(4.0 * self.angle + t * 1.5)
        rings = np.sin(12.0 * self.radius - t * 2.2)
        r = 0.5 + 0.5 * np.sin(5 * depth + swirl)
        g = 0.5 + 0.5 * np.sin(6 * depth + rings)
        b = 0.5 + 0.5 * np.sin(7 * depth + swirl * rings)
        colors = np.stack([r, g, b], axis=-1)
        colors = (colors * 255).astype(np.uint8)
        pixels = pygame.surfarray.pixels3d(self.surface)
        np.copyto(pixels, np.transpose(colors, (1, 0, 2)))
        del pixels
        return self.surface


class TextScrollerEffect(Effect):
    """Horizontally scrolling marquee text with a parallax star field."""

    def __init__(
        self,
        cfg: DemoConfig,
        lines: Optional[List[str]] = None,
        *,
        speed: float = 120.0,
    ):
        super().__init__(cfg)
        self.speed = speed
        self.font_large = pygame.font.SysFont("Share Tech Mono", 40)
        self.font_small = pygame.font.SysFont("Share Tech Mono", 24)
        self.lines = lines or [
            "WELCOME TO THE OLD SCHOOL MULTI-EFFECT DEMO",
            "SYNCHRONISED TIMELINE, AUDIO, AND RETRO GFX",
            "BUILT WITH PYGAME + NUMPY -- ENJOY THE VIBES!",
        ]
        self.text_surface = self._create_text_surface()
        self.bg_gradient = self._create_gradient()
        rng = np.random.default_rng(1337)
        stars = rng.random((200, 3))
        self.star_x = (stars[:, 0] * cfg.width).astype(np.float32)
        self.star_y = (stars[:, 1] * cfg.height).astype(np.float32)
        self.star_depth = stars[:, 2].astype(np.float32)
        self.flash = 0.0

    def _create_text_surface(self) -> pygame.Surface:
        padding = 80
        width = padding + sum(
            self.font_large.size(line + "   ")[0] for line in self.lines
        )
        surface = pygame.Surface((width, self.cfg.height), pygame.SRCALPHA)
        x = padding // 2
        for idx, line in enumerate(self.lines):
            color = pygame.Color(240, 240, 255)
            text = self.font_large.render(line, True, color)
            surface.blit(text, (x, self.cfg.height // 2 - text.get_height() // 2))
            sub = self.font_small.render(
                "::" + line.lower() + "::", True, (120, 200, 255)
            )
            surface.blit(sub, (x, self.cfg.height // 2 + text.get_height() // 2))
            x += text.get_width() + padding
        return surface.convert_alpha()

    def _create_gradient(self) -> pygame.Surface:
        gradient = pygame.Surface(self.cfg.resolution()).convert()
        pixels = pygame.surfarray.pixels3d(gradient)
        y = np.linspace(0.0, 1.0, self.cfg.height, dtype=np.float32)
        top = np.array([5, 15, 35], dtype=np.float32)
        bottom = np.array([40, 50, 80], dtype=np.float32)
        colors = top + (bottom - top) * y[:, None]
        row = np.repeat(colors[:, None, :], self.cfg.width, axis=1).astype(np.uint8)
        np.copyto(pixels, np.transpose(row, (1, 0, 2)))
        del pixels
        return gradient

    def on_beat(self, beat_index: int) -> None:
        self.flash = 1.0

    def render(self, timeline_t: float, dt: float) -> pygame.Surface:
        self.flash = max(self.flash - dt * 1.8, 0.0)
        surface = self.surface
        surface.blit(self.bg_gradient, (0, 0))

        # Parallax stars
        star_speed = (self.star_depth * 40 + 20) * dt
        self.star_x -= star_speed
        wrap = self.star_x < 0
        self.star_x[wrap] += self.cfg.width
        surface.lock()
        for x, y, depth in zip(self.star_x, self.star_y, self.star_depth):
            brightness = int(120 + 135 * depth + self.flash * 100)
            brightness = max(0, min(255, brightness))
            color = (brightness, brightness, 255)
            surface.fill(
                color, (int(x) % self.cfg.width, int(y) % self.cfg.height, 2, 2)
            )
        surface.unlock()

        offset = (timeline_t * self.speed) % self.text_surface.get_width()
        x = int(self.cfg.width - offset)
        surface.blit(self.text_surface, (x, 0))
        surface.blit(self.text_surface, (x - self.text_surface.get_width(), 0))

        if self.flash > 0.1:
            overlay = pygame.Surface(self.cfg.resolution(), pygame.SRCALPHA)
            overlay.fill((255, 255, 255, int(90 * self.flash)))
            surface.blit(overlay, (0, 0))

        return surface


class ZoomerEffect(Effect):
    """Z-axis zoomer grid with chromatic aberration."""

    def __init__(self, cfg: DemoConfig, *, layers: int = 160):
        super().__init__(cfg)
        rng = np.random.default_rng(9001)
        self.points = rng.random((layers, 3)).astype(np.float32)
        self.chroma_shift = 0.0
        self.scanline = pygame.Surface((cfg.width, 2), pygame.SRCALPHA)
        self.scanline.fill((0, 0, 0, 70))

    def on_beat(self, beat_index: int) -> None:
        self.chroma_shift = 0.5

    def render(self, timeline_t: float, dt: float) -> pygame.Surface:
        w, h = self.cfg.resolution()
        cx, cy = w / 2, h / 2
        speed = 0.35
        depth = (timeline_t * speed + self.points[:, 2]) % 1.0
        scale = 1.0 / (depth * 1.5 + 0.2)
        px = cx + (self.points[:, 0] - 0.5) * w * scale * 0.35
        py = cy + (self.points[:, 1] - 0.5) * h * scale * 0.35

        surface = self.surface
        surface.fill((2, 4, 8))
        size = np.clip((1.5 / (depth + 0.05)).astype(int), 1, 6)

        chroma = self.chroma_shift
        self.chroma_shift = max(0.0, self.chroma_shift - dt * 0.6)

        for idx, (x, y, s) in enumerate(zip(px, py, size)):
            alpha = max(0, min(255, int((1.0 - depth[idx]) * 255)))
            tint = 60 + int(160 * depth[idx])
            base_color = (
                min(255, tint + alpha // 3),
                min(255, 100 + alpha // 2),
                255,
            )
            rect = (int(x), int(y), s, s)
            surface.fill(base_color, rect)
            if chroma > 0.01:
                shift = int(chroma * 4)
                surface.fill(
                    (base_color[0], 30, 30),
                    (rect[0] + shift, rect[1], rect[2], rect[3]),
                )
                surface.fill(
                    (30, base_color[1], 30),
                    (rect[0], rect[1] + shift, rect[2], rect[3]),
                )

        # Subtle scanlines
        for y in range(0, h, 4):
            surface.blit(self.scanline, (0, y))

        return surface


# ---------------------------------------------------------------------------
# Main runtime loop
# ---------------------------------------------------------------------------


class DemoRuntime:
    def __init__(self, cfg: DemoConfig, timeline: Timeline):
        self.cfg = cfg
        self.timeline = timeline
        self.clock = pygame.time.Clock()
        self.elapsed = 0.0
        self.paused = False
        self.overlay_font = pygame.font.SysFont("Share Tech Mono", 20)
        self.beat_clock = BeatClock(cfg.bpm)
        self.gif_frames: List[np.ndarray] = []
        self.frame_index = 0

        self.screen = self._create_screen()
        self.timeline.instantiate_effects()
        self.info_surface = pygame.Surface((cfg.width, 80), pygame.SRCALPHA)

        self._audio_sound: Optional[pygame.mixer.Sound] = None
        if not cfg.disable_audio:
            self._audio_sound = self._prepare_audio()
            if self._audio_sound is not None:
                self._audio_sound.play()

    def _create_screen(self) -> pygame.Surface:
        flags = 0
        if self.cfg.fullscreen:
            flags |= pygame.FULLSCREEN
        if self.cfg.headless or self.cfg.capture_gif or self.cfg.capture_frames:
            flags |= pygame.HIDDEN
        screen = pygame.display.set_mode(self.cfg.resolution(), flags)
        pygame.display.set_caption(self.cfg.window_title)
        return screen

    def _prepare_audio(self) -> Optional[pygame.mixer.Sound]:
        target = self.timeline.total_duration + 1.0
        if self.cfg.stop_after is not None:
            target = max(self.cfg.stop_after, 1.0)
        length = max(self.cfg.duration, target)
        try:
            audio = generate_audio_track(self.cfg, length)
        except Exception as exc:  # pragma: no cover - audio generation failure is rare
            print(f"Audio generation failed: {exc}")
            return None
        return pygame.mixer.Sound(buffer=audio.tobytes())

    def run(self) -> None:
        running = True
        target = self.timeline.total_duration + 1.0
        if self.cfg.stop_after is not None:
            target = min(target, self.cfg.stop_after)
        while running:
            dt = self._tick()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                elif event.type == pygame.KEYDOWN:
                    if event.key in (pygame.K_ESCAPE, pygame.K_q):
                        running = False
                    elif event.key == pygame.K_SPACE:
                        self._toggle_pause()
                    elif event.key == pygame.K_o:
                        self.cfg.overlay = not self.cfg.overlay

            if not self.paused:
                self.elapsed += dt

            active = self.timeline.active_states(self.elapsed)
            self._update_segment_states(active)

            beats = self.beat_clock.update(self.elapsed)
            if beats:
                for state in active:
                    if state.started and not state.finished:
                        for beat in beats:
                            state.effect.on_beat(beat)

            frame_surface = self._compose_frame(active, dt)
            self._blit_frame(frame_surface)
            self._capture(frame_surface)

            if self.elapsed >= target:
                running = False

        if self._audio_sound is not None:
            pygame.mixer.fadeout(400)
        self._finalize_capture()

    def _tick(self) -> float:
        return self.clock.tick(self.cfg.fps) / 1000.0

    def _toggle_pause(self) -> None:
        self.paused = not self.paused
        if self.paused:
            pygame.mixer.pause()
        else:
            pygame.mixer.unpause()

    def _update_segment_states(self, active: List[SegmentState]) -> None:
        for state in active:
            if not state.started and self.elapsed >= state.start:
                state.effect.on_enter(state.spec.metadata)
                state.started = True
            if not state.finished and self.elapsed >= state.end:
                state.effect.on_exit()
                state.finished = True

    def _compose_frame(self, active: List[SegmentState], dt: float) -> pygame.Surface:
        frame = pygame.Surface(self.cfg.resolution(), pygame.SRCALPHA)
        active.sort(key=lambda s: s.start)
        for state in active:
            local_t = self.elapsed - state.start
            seg = state.spec
            alpha = 1.0
            if self.elapsed < state.start:
                if seg.fade_in > 0:
                    alpha = max(
                        0.0, min(1.0, 1.0 - (state.start - self.elapsed) / seg.fade_in)
                    )
                else:
                    alpha = 1.0
            elif self.elapsed > state.end:
                if seg.fade_out > 0:
                    alpha = max(
                        0.0, min(1.0, 1.0 - (self.elapsed - state.end) / seg.fade_out)
                    )
                else:
                    alpha = 0.0
            surface = state.effect.render(max(local_t, 0.0), dt)
            if alpha < 1.0:
                temp = surface.copy()
                temp.set_alpha(int(alpha * 255))
                frame.blit(temp, (0, 0))
            else:
                frame.blit(surface, (0, 0))
        if self.cfg.overlay:
            self._draw_overlay(frame)
        return frame

    def _draw_overlay(self, frame: pygame.Surface) -> None:
        self.info_surface.fill((0, 0, 0, 0))
        active_name = " / ".join(
            state.spec.name
            for state in self.timeline.active_states(self.elapsed)
            if state.started
        )
        text = f"t={self.elapsed:05.2f}s | fps~{self.clock.get_fps():05.1f} | {active_name or 'waiting'}"
        label = self.overlay_font.render(text, True, (220, 240, 250))
        self.info_surface.blit(label, (20, 20))
        frame.blit(self.info_surface, (0, 0))

    def _blit_frame(self, frame_surface: pygame.Surface) -> None:
        self.screen.blit(frame_surface, (0, 0))
        pygame.display.flip()

    def _capture(self, frame_surface: pygame.Surface) -> None:
        self.frame_index += 1
        if self.cfg.capture_frames and (
            self.frame_index % self.cfg.capture_stride == 0
        ):
            self.cfg.capture_frames.mkdir(parents=True, exist_ok=True)
            out = self.cfg.capture_frames / f"frame_{self.frame_index:05d}.png"
            pygame.image.save(frame_surface, out)
        if self.cfg.capture_gif is not None and (
            self.frame_index % self.cfg.capture_stride == 0
        ):
            array = pygame.surfarray.array3d(frame_surface)
            self.gif_frames.append(np.transpose(array, (1, 0, 2)))

    def _finalize_capture(self) -> None:
        if self.cfg.capture_gif is not None and self.gif_frames:
            if imageio is None:
                raise SystemExit(
                    "imageio is required for GIF export; install with `pip install imageio`."
                )
            self.cfg.capture_gif.parent.mkdir(parents=True, exist_ok=True)
            fps = max(1, self.cfg.fps // self.cfg.capture_stride)
            imageio.mimsave(self.cfg.capture_gif, self.gif_frames, fps=fps)


# ---------------------------------------------------------------------------
# Command-line interface
# ---------------------------------------------------------------------------


def build_timeline(cfg: DemoConfig) -> Timeline:
    lines = [
        "PYGAME TIMELINE INTRO",
        "PLASMA TUNNEL, SCROLLER, STAR ZOOMER",
        "SYNTH AUDIO + VISUALS IN SYNC",
        "PRESS SPACE TO PAUSE, O TO TOGGLE HUD",
    ]

    segments = [
        Segment(
            "Plasma Tunnel",
            lambda c: PlasmaTunnelEffect(c),
            duration=12.0,
            fade_in=1.2,
            fade_out=1.0,
        ),
        Segment(
            "Scroller",
            lambda c: TextScrollerEffect(c, lines=lines, speed=140.0),
            duration=9.0,
            fade_in=1.0,
            fade_out=1.0,
        ),
        Segment(
            "Zoomer",
            lambda c: ZoomerEffect(c),
            duration=10.0,
            fade_in=1.0,
            fade_out=1.2,
        ),
    ]
    timeline = Timeline(cfg, segments)
    if cfg.stop_after is None:
        cfg.duration = max(cfg.duration, timeline.total_duration + 1.0)
    else:
        cfg.duration = max(cfg.duration, cfg.stop_after)
    return timeline


def parse_args() -> DemoConfig:
    parser = argparse.ArgumentParser(
        description="Retro multi-effect demo (plasma tunnel, scroller, zoomer)"
    )
    parser.add_argument("--width", type=int, default=960, help="Viewport width")
    parser.add_argument("--height", type=int, default=540, help="Viewport height")
    parser.add_argument("--fps", type=int, default=60, help="Frame rate cap")
    parser.add_argument("--bpm", type=int, default=120, help="Audio timeline BPM")
    parser.add_argument(
        "--no-audio", action="store_true", help="Disable audio playback"
    )
    parser.add_argument(
        "--fullscreen", action="store_true", help="Launch in fullscreen"
    )
    parser.add_argument(
        "--capture-frames", type=Path, help="Capture frames to directory"
    )
    parser.add_argument(
        "--capture-gif", type=Path, help="Capture an animated GIF to path"
    )
    parser.add_argument(
        "--capture-stride", type=int, default=2, help="Capture every Nth frame"
    )
    parser.add_argument(
        "--headless", action="store_true", help="Hide the window (useful for capture)"
    )
    parser.add_argument(
        "--overlay", action="store_true", default=False, help="Force overlay display"
    )
    parser.add_argument(
        "--stop-after",
        type=float,
        help="Stop after N seconds (useful when capturing snippets)",
    )
    args = parser.parse_args()

    cfg = DemoConfig(
        width=args.width,
        height=args.height,
        fps=args.fps,
        bpm=args.bpm,
        disable_audio=args.no_audio,
        fullscreen=args.fullscreen,
        capture_frames=args.capture_frames,
        capture_stride=max(1, args.capture_stride),
        capture_gif=args.capture_gif,
        headless=args.headless,
        overlay=args.overlay or not args.headless,
        stop_after=args.stop_after,
    )
    return cfg


def main() -> None:
    cfg = parse_args()
    pygame.mixer.pre_init(cfg.sample_rate, size=-16, channels=2)
    pygame.init()
    if cfg.disable_audio:
        pygame.mixer.quit()
    timeline = build_timeline(cfg)
    runtime = DemoRuntime(cfg, timeline)
    runtime.run()
    pygame.quit()


if __name__ == "__main__":
    main()
