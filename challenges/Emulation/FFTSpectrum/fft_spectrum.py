"""Realtime FFT spectrum visualizer for microphone or WAV input."""

from __future__ import annotations

import argparse
import queue
import sys
import wave
from dataclasses import dataclass
from pathlib import Path
from typing import Generator, Iterable, Optional, Tuple

import numpy as np


@dataclass
class SpectrumConfig:
    """Configuration values for FFT processing."""

    sample_rate: int = 44100
    window_size: int = 2048
    window_type: str = "hann"

    def window(self, length: int) -> np.ndarray:
        """Return a windowing function array for the given length."""
        if self.window_type.lower() in {"hann", "hanning"}:
            return np.hanning(length)
        if self.window_type.lower() == "hamming":
            return np.hamming(length)
        return np.ones(length)


def _ensure_mono(samples: np.ndarray) -> np.ndarray:
    if samples.ndim == 1:
        return samples
    return samples.mean(axis=1)


def _to_float32(samples: np.ndarray) -> np.ndarray:
    if samples.dtype == np.float32:
        return samples
    if np.issubdtype(samples.dtype, np.floating):
        return samples.astype(np.float32, copy=False)
    if np.issubdtype(samples.dtype, np.integer):
        info = np.iinfo(samples.dtype)
        if np.issubdtype(samples.dtype, np.unsignedinteger):
            midpoint = (info.max + 1) / 2.0
            return (samples.astype(np.float32) - midpoint) / midpoint
        return samples.astype(np.float32) / max(abs(info.min), info.max)
    return samples.astype(np.float32)


def chunk_to_fft(
    samples: np.ndarray, config: SpectrumConfig
) -> Tuple[np.ndarray, np.ndarray]:
    """Compute FFT magnitude spectrum for the provided audio chunk."""

    array = np.asarray(samples)
    if array.size == 0:
        raise ValueError("Cannot compute FFT of empty sample array")
    if array.ndim > 1:
        array = _ensure_mono(array)
    array = _to_float32(array)
    window = config.window(len(array))
    windowed = array * window
    spectrum = np.fft.rfft(windowed, n=len(array))
    freqs = np.fft.rfftfreq(len(array), d=1.0 / config.sample_rate)
    magnitude = np.abs(spectrum) / max(len(array), 1)
    return freqs, magnitude


class WavStream:
    """Generator over WAV frames."""

    def __init__(self, path: Path, config: SpectrumConfig, loop: bool = False):
        self.path = Path(path)
        self.config = config
        self.loop = loop

    def __iter__(self) -> Iterable[np.ndarray]:
        while True:
            with wave.open(self.path, "rb") as wav_file:
                channels = wav_file.getnchannels()
                width = wav_file.getsampwidth()
                dtype = {1: np.uint8, 2: np.int16, 4: np.int32}.get(width)
                if dtype is None:
                    raise ValueError(f"Unsupported sample width: {width} bytes")
                framerate = wav_file.getframerate()
                if self.config.sample_rate != framerate:
                    self.config.sample_rate = framerate
                blocksize = self.config.window_size
                while True:
                    frames = wav_file.readframes(blocksize)
                    if not frames:
                        break
                    data = np.frombuffer(frames, dtype=dtype)
                    if channels > 1:
                        data = data.reshape(-1, channels)
                    yield data
            if not self.loop:
                break


class MicrophoneStream:
    """Yield audio blocks from the active input device using sounddevice."""

    def __init__(
        self,
        config: SpectrumConfig,
        device: Optional[str] = None,
    ) -> None:
        self.config = config
        self.device = device
        self._queue: "queue.Queue[np.ndarray]" = queue.Queue()
        self._stream = None

    def __enter__(self) -> "MicrophoneStream":
        try:
            import sounddevice as sd  # type: ignore
        except ImportError as exc:  # pragma: no cover - optional dependency
            raise RuntimeError(
                "sounddevice is required for microphone capture"
            ) from exc

        def callback(
            indata, frames, time, status
        ):  # pragma: no cover - requires hardware
            if status:
                print(status, file=sys.stderr)
            self._queue.put(indata.copy())

        self._stream = sd.InputStream(
            samplerate=self.config.sample_rate,
            blocksize=self.config.window_size,
            channels=1,
            device=self.device,
            callback=callback,
        )
        self._stream.start()
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        if self._stream is not None:  # pragma: no cover - requires hardware
            self._stream.stop()
            self._stream.close()
            self._stream = None

    def __iter__(
        self,
    ) -> Generator[np.ndarray, None, None]:  # pragma: no cover - requires hardware
        while True:
            yield self._queue.get()


def _run_visualizer(
    source: Iterable[np.ndarray],
    config: SpectrumConfig,
    scale: str,
    refresh_rate: float,
) -> None:
    import matplotlib.pyplot as plt
    from matplotlib.animation import FuncAnimation

    source_iter = iter(source)
    fig, ax = plt.subplots()
    base_freqs = np.fft.rfftfreq(config.window_size, d=1.0 / config.sample_rate)
    (line,) = ax.plot(base_freqs, np.zeros_like(base_freqs))
    ax.set_xlabel("Frequency (Hz)")
    ax.set_ylabel("Magnitude" if scale == "linear" else "dBFS")
    ax.set_xlim(0, config.sample_rate / 2)
    ax.set_ylim(0, 1)
    ax.set_title("Real-time FFT Spectrum")

    def update(_):
        try:
            chunk = next(source_iter)
        except StopIteration:
            plt.close(fig)
            return line
        freqs, magnitude = chunk_to_fft(chunk, config)
        if scale == "log":
            magnitude = 20 * np.log10(np.maximum(magnitude, 1e-12))
            ax.set_ylim(-120, 0)
        else:
            peak = float(np.max(magnitude))
            ax.set_ylim(0, max(1e-6, peak * 1.2))
        line.set_data(freqs, magnitude)
        ax.set_xlim(0, config.sample_rate / 2)
        return line

    interval_ms = 1000.0 / refresh_rate if refresh_rate > 0 else 0
    animation = FuncAnimation(fig, update, interval=interval_ms, blit=False)
    plt.show()
    del animation


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source", choices=["mic", "wav"], default="mic", help="Audio input source"
    )
    parser.add_argument(
        "--wav-path", type=Path, help="Path to WAV file when using --source=wav"
    )
    parser.add_argument(
        "--samplerate", type=int, default=None, help="Sample rate to request"
    )
    parser.add_argument(
        "--window-size", type=int, default=2048, help="FFT window size (samples)"
    )
    parser.add_argument(
        "--window",
        choices=["hann", "hamming", "rect"],
        default="hann",
        help="Window function",
    )
    parser.add_argument("--device", help="Optional sounddevice input identifier")
    parser.add_argument(
        "--refresh",
        type=float,
        default=30.0,
        help="Plot refresh rate (frames per second)",
    )
    parser.add_argument(
        "--scale",
        choices=["linear", "log"],
        default="linear",
        help="Display magnitude scale",
    )
    parser.add_argument("--loop", action="store_true", help="Loop WAV file playback")
    args = parser.parse_args(list(argv) if argv is not None else None)

    config = SpectrumConfig(
        sample_rate=args.samplerate or 44100,
        window_size=args.window_size,
        window_type="rect" if args.window == "rect" else args.window,
    )

    if args.source == "wav":
        if not args.wav_path:
            parser.error("--wav-path is required when --source=wav")
        stream = WavStream(args.wav_path, config, loop=args.loop)
        try:
            _run_visualizer(stream, config, args.scale, args.refresh)
        except ValueError as exc:
            parser.error(str(exc))
        return 0

    try:
        with MicrophoneStream(config, device=args.device) as mic_stream:
            _run_visualizer(mic_stream, config, args.scale, args.refresh)
    except RuntimeError as exc:
        parser.error(str(exc))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
