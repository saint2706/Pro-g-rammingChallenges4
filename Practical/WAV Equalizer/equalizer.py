"""Real-time multi-band equalizer for WAV files."""

from __future__ import annotations

import argparse
import queue
import threading
from dataclasses import dataclass
from typing import Dict, Iterable, List, Sequence, Tuple

import matplotlib

# Use TkAgg backend for Tkinter embedding before importing pyplot.
matplotlib.use("TkAgg")

import numpy as np
import sounddevice as sd
from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from scipy.io import wavfile
import tkinter as tk
from tkinter import ttk, messagebox

# Frequency bands (Hz) approximating ISO standard 10-band EQ.
BANDS: Sequence[Tuple[str, float, float]] = (
    ("31 Hz", 20.0, 45.0),
    ("63 Hz", 45.0, 90.0),
    ("125 Hz", 90.0, 180.0),
    ("250 Hz", 180.0, 355.0),
    ("500 Hz", 355.0, 710.0),
    ("1 kHz", 710.0, 1400.0),
    ("2 kHz", 1400.0, 2800.0),
    ("4 kHz", 2800.0, 5600.0),
    ("8 kHz", 5600.0, 11200.0),
    ("16 kHz", 11200.0, 20000.0),
)

PRESETS: Dict[str, Sequence[float]] = {
    "Flat": (0,) * len(BANDS),
    "Bass Boost": (6, 4, 3, 1, 0, -1, -2, -3, -4, -5),
    "Treble Boost": (-4, -3, -2, -1, 0, 1, 3, 4, 5, 6),
    "Podcast": (2, 3, 4, 4, 3, 2, 0, -2, -3, -4),
    "Vocal Cut": (-6, -4, -2, 0, 1, 2, 4, 6, 6, 6),
}


@dataclass
class LatencyStatus:
    seconds: float
    threshold: float

    @property
    def exceeds(self) -> bool:
        return self.seconds > self.threshold

    def warning_text(self) -> str:
        if not self.exceeds:
            return f"Estimated latency: {self.seconds * 1000:.1f} ms"
        return (
            f"Warning: block size ≈ {self.seconds * 1000:.1f} ms latency. "
            f"Reduce the block size (threshold {self.threshold * 1000:.0f} ms)."
        )


class RealTimeEqualizer:
    """Applies FFT-based equalization to audio blocks in real time."""

    def __init__(
        self,
        wav_path: str,
        blocksize: int = 2048,
        max_latency: float = 0.08,
        preset: Sequence[float] | None = None,
    ) -> None:
        self.samplerate, data = wavfile.read(wav_path)
        if data.ndim == 1:
            data = data[:, None]
        data = data.astype(np.float32)
        max_val = np.max(np.abs(data))
        if max_val > 0:
            data /= max_val
        self.data = data
        self.blocksize = blocksize
        self.max_latency = max_latency
        self._position = 0
        self._lock = threading.Lock()
        self._gains_db = np.zeros(len(BANDS), dtype=np.float32)
        if preset is not None:
            self.set_all_gains(preset)
        self.visual_queue: queue.Queue[Tuple[np.ndarray, np.ndarray]] = queue.Queue(
            maxsize=4
        )
        self._stream: sd.OutputStream | None = None
        self._finished = threading.Event()

    @property
    def channels(self) -> int:
        return self.data.shape[1]

    def reset(self) -> None:
        self._position = 0
        self._finished.clear()

    def set_gain(self, index: int, value: float) -> None:
        with self._lock:
            self._gains_db[index] = float(value)

    def set_all_gains(self, values: Sequence[float]) -> None:
        if len(values) != len(BANDS):
            raise ValueError("Preset length mismatch with band count")
        for idx, value in enumerate(values):
            self.set_gain(idx, value)

    def gains(self) -> np.ndarray:
        with self._lock:
            return self._gains_db.copy()

    def latency_status(self) -> LatencyStatus:
        seconds = self.blocksize / float(self.samplerate)
        return LatencyStatus(seconds=seconds, threshold=self.max_latency)

    def start(self) -> None:
        if self._stream is not None:
            return
        self.reset()
        latency = self.latency_status()
        if latency.exceeds:
            print(latency.warning_text())
        self._stream = sd.OutputStream(
            samplerate=self.samplerate,
            blocksize=self.blocksize,
            channels=self.channels,
            dtype="float32",
            callback=self._callback,
        )
        self._stream.start()

    def stop(self) -> None:
        if self._stream is None:
            return
        try:
            self._stream.stop()
        finally:
            self._stream.close()
            self._stream = None
            self._finished.set()

    def is_playing(self) -> bool:
        return self._stream is not None and not self._finished.is_set()

    def has_stream(self) -> bool:
        return self._stream is not None

    def _callback(self, outdata: np.ndarray, frames: int, _time, status) -> None:
        if status:
            print(status)
        chunk, finished = self._next_chunk(frames)
        processed, freqs, magnitude = self._apply_equalizer(chunk)
        outdata[:] = processed
        if magnitude is not None:
            try:
                self.visual_queue.put_nowait((freqs, magnitude))
            except queue.Full:
                pass
        if finished:
            self._finished.set()
            raise sd.CallbackStop()

    def _next_chunk(self, frames: int) -> Tuple[np.ndarray, bool]:
        start = self._position
        end = start + frames
        finished = False
        if start >= len(self.data):
            finished = True
            out = np.zeros((frames, self.channels), dtype=np.float32)
        else:
            out = self.data[start:end]
            if len(out) < frames:
                pad = np.zeros((frames - len(out), self.channels), dtype=np.float32)
                out = np.vstack((out, pad))
                finished = True
        self._position = end
        return out, finished

    def _apply_equalizer(
        self, chunk: np.ndarray
    ) -> Tuple[np.ndarray, np.ndarray, np.ndarray | None]:
        if not len(chunk):
            return chunk, np.array([]), None
        chunk = chunk.copy()
        freqs = np.fft.rfftfreq(len(chunk), d=1.0 / self.samplerate)
        spectrum = np.fft.rfft(chunk, axis=0)
        with self._lock:
            gains_db = self._gains_db.copy()
        for idx, (_, low, high) in enumerate(BANDS):
            mask = (freqs >= low) & (freqs < high)
            if not np.any(mask):
                continue
            gain = 10 ** (gains_db[idx] / 20.0)
            spectrum[mask, :] *= gain
        processed = np.fft.irfft(spectrum, n=len(chunk), axis=0)
        processed = np.clip(processed, -1.0, 1.0)
        # Prepare a magnitude spectrum (averaged across channels)
        magnitude = np.mean(np.abs(spectrum), axis=1)
        magnitude_db = 20 * np.log10(np.maximum(magnitude, 1e-6))
        return processed.astype(np.float32), freqs, magnitude_db


class EqualizerApp:
    def __init__(self, pipeline: RealTimeEqualizer) -> None:
        self.pipeline = pipeline
        self.root = tk.Tk()
        self.root.title("WAV Equalizer")
        self._build_ui()
        self._schedule_visual_update()

    def _build_ui(self) -> None:
        top = ttk.Frame(self.root, padding=10)
        top.pack(fill=tk.BOTH, expand=True)

        controls = ttk.Frame(top)
        controls.pack(side=tk.LEFT, fill=tk.Y)

        self.sliders: List[tk.Scale] = []
        ttk.Label(controls, text="Band gains (dB)").pack(pady=(0, 8))
        slider_frame = ttk.Frame(controls)
        slider_frame.pack()
        for idx, (label, *_rest) in enumerate(BANDS):
            col = ttk.Frame(slider_frame)
            col.grid(row=0, column=idx, padx=4)
            scale = tk.Scale(
                col,
                from_=12,
                to=-12,
                resolution=0.5,
                orient=tk.VERTICAL,
                length=200,
                command=lambda value, i=idx: self.pipeline.set_gain(i, float(value)),
            )
            scale.set(0)
            scale.pack()
            ttk.Label(col, text=label).pack(pady=(4, 0))
            self.sliders.append(scale)

        preset_frame = ttk.Frame(controls)
        preset_frame.pack(pady=10, fill=tk.X)
        ttk.Label(preset_frame, text="Preset:").pack(side=tk.LEFT)
        self.preset_var = tk.StringVar(value="Flat")
        preset_menu = ttk.OptionMenu(
            preset_frame,
            self.preset_var,
            "Flat",
            *PRESETS.keys(),
            command=self._apply_preset,
        )
        preset_menu.pack(side=tk.LEFT, padx=(8, 0))

        button_frame = ttk.Frame(controls)
        button_frame.pack(pady=10)
        self.start_button = ttk.Button(
            button_frame, text="Start", command=self._start_playback
        )
        self.start_button.grid(row=0, column=0, padx=5)
        ttk.Button(button_frame, text="Stop", command=self._stop_playback).grid(
            row=0, column=1, padx=5
        )

        latency = self.pipeline.latency_status()
        self.latency_label = ttk.Label(controls, text=latency.warning_text())
        self.latency_label.pack(pady=(10, 0))
        if latency.exceeds:
            self.latency_label.configure(foreground="red")

        # Plot area
        figure = plt.Figure(figsize=(6, 3), dpi=100)
        self.ax = figure.add_subplot(111)
        self.ax.set_title("Live magnitude spectrum")
        self.ax.set_xlabel("Frequency (Hz)")
        self.ax.set_ylabel("Magnitude (dB)")
        self.ax.set_xscale("log")
        self.ax.set_xlim(20, self.pipeline.samplerate / 2)
        self.ax.set_ylim(-80, 10)
        (self.line,) = self.ax.plot([], [], color="tab:blue")
        canvas = FigureCanvasTkAgg(figure, master=top)
        canvas.draw()
        canvas.get_tk_widget().pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)
        self.canvas = canvas

    def _apply_preset(self, preset_name: str) -> None:
        if preset_name not in PRESETS:
            return
        gains = PRESETS[preset_name]
        self.pipeline.set_all_gains(gains)
        for slider, value in zip(self.sliders, gains):
            slider.set(value)

    def _start_playback(self) -> None:
        while not self.pipeline.visual_queue.empty():
            try:
                self.pipeline.visual_queue.get_nowait()
            except queue.Empty:
                break
        try:
            self.pipeline.start()
        except Exception as exc:  # pragma: no cover - UI path
            messagebox.showerror("Audio error", str(exc))
            return
        self.start_button.state(["disabled"])

    def _stop_playback(self) -> None:
        self.pipeline.stop()
        self.start_button.state(["!disabled"])

    def _schedule_visual_update(self) -> None:
        self.root.after(66, self._update_visual)

    def _update_visual(self) -> None:
        try:
            freqs, magnitude_db = self.pipeline.visual_queue.get_nowait()
        except queue.Empty:
            pass
        else:
            if len(freqs) > 1:
                self.line.set_data(freqs, magnitude_db)
                self.ax.set_xlim(max(20, freqs[1]), self.pipeline.samplerate / 2)
                self.canvas.draw_idle()
        if not self.pipeline.is_playing():
            if self.pipeline.has_stream():
                self.pipeline.stop()
            self.start_button.state(["!disabled"])
        self._schedule_visual_update()

    def run(self) -> None:
        self.root.protocol("WM_DELETE_WINDOW", self._on_close)
        self.root.mainloop()

    def _on_close(self) -> None:
        self._stop_playback()
        self.root.destroy()


def parse_args(argv: Iterable[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Real-time WAV equalizer")
    parser.add_argument("wav_path", help="Path to the WAV file to play")
    parser.add_argument(
        "--blocksize",
        type=int,
        default=2048,
        help="Audio block size (samples). Larger values increase latency but reduce CPU usage.",
    )
    parser.add_argument(
        "--max-latency",
        type=float,
        default=0.08,
        help="Warn if block latency (seconds) exceeds this threshold.",
    )
    parser.add_argument(
        "--preset",
        choices=sorted(PRESETS.keys()),
        default="Flat",
        help="Preset gains to load on startup.",
    )
    return parser.parse_args(list(argv) if argv is not None else None)


def main(argv: Iterable[str] | None = None) -> None:
    args = parse_args(argv)
    preset = PRESETS.get(args.preset, PRESETS["Flat"])
    pipeline = RealTimeEqualizer(
        wav_path=args.wav_path,
        blocksize=args.blocksize,
        max_latency=args.max_latency,
        preset=preset,
    )
    app = EqualizerApp(pipeline)
    app.run()


if __name__ == "__main__":
    main()
