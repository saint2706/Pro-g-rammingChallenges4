"""Realtime FFT spectrum visualizer for microphone or WAV input.

This script provides a real-time spectrum analyzer that can take input from
a microphone or a WAV file and display the frequency spectrum using Matplotlib.
"""

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
    """Configuration values for FFT processing.

    Attributes:
        sample_rate: The sample rate of the audio source.
        window_size: The size of the FFT window in samples.
        window_type: The type of windowing function to use ('hann', 'hamming', or 'rect').
    """

    sample_rate: int = 44100
    window_size: int = 2048
    window_type: str = "hann"

    def window(self, length: int) -> np.ndarray:
        """Returns a windowing function array for a given length.

        Args:
            length: The length of the window.

        Returns:
            A NumPy array representing the windowing function.
        """
        if self.window_type.lower() in {"hann", "hanning"}:
            return np.hanning(length)
        if self.window_type.lower() == "hamming":
            return np.hamming(length)
        return np.ones(length)


def _ensure_mono(samples: np.ndarray) -> np.ndarray:
    """Converts a stereo audio signal to mono by averaging the channels."""
    if samples.ndim == 1:
        return samples
    return samples.mean(axis=1)


def _to_float32(samples: np.ndarray) -> np.ndarray:
    """Converts an audio sample array to float32 format."""
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
    """Computes the FFT magnitude spectrum for a chunk of audio samples.

    Args:
        samples: A NumPy array of audio samples.
        config: The FFT configuration.

    Returns:
        A tuple of (frequencies, magnitude).
    """
    array = np.asarray(samples)
    if array.size == 0:
        raise ValueError("Cannot compute FFT of an empty sample array.")
    array = _ensure_mono(array)
    array = _to_float32(array)
    window = config.window(len(array))
    windowed = array * window
    spectrum = np.fft.rfft(windowed, n=len(array))
    freqs = np.fft.rfftfreq(len(array), d=1.0 / config.sample_rate)
    magnitude = np.abs(spectrum) / max(len(array), 1)
    return freqs, magnitude


class WavStream:
    """A generator that yields audio chunks from a WAV file."""

    def __init__(self, path: Path, config: SpectrumConfig, loop: bool = False):
        """Initializes the WAV stream.

        Args:
            path: The path to the WAV file.
            config: The FFT configuration.
            loop: Whether to loop the WAV file.
        """
        self.path = Path(path)
        self.config = config
        self.loop = loop

    def __iter__(self) -> Iterable[np.ndarray]:
        """Yields audio chunks from the WAV file."""
        while True:
            with wave.open(str(self.path), "rb") as wav_file:
                channels = wav_file.getnchannels()
                width = wav_file.getsampwidth()
                dtype = {1: np.uint8, 2: np.int16, 4: np.int32}.get(width)
                if dtype is None:
                    raise ValueError(f"Unsupported sample width: {width} bytes")
                self.config.sample_rate = wav_file.getframerate()
                while True:
                    frames = wav_file.readframes(self.config.window_size)
                    if not frames:
                        break
                    data = np.frombuffer(frames, dtype=dtype)
                    if channels > 1:
                        data = data.reshape(-1, channels)
                    yield data
            if not self.loop:
                break


class MicrophoneStream:
    """Yields audio blocks from a microphone using sounddevice."""

    def __init__(
        self,
        config: SpectrumConfig,
        device: Optional[str] = None,
    ) -> None:
        """Initializes the microphone stream.

        Args:
            config: The FFT configuration.
            device: The input device to use.
        """
        self.config = config
        self.device = device
        self._queue: "queue.Queue[np.ndarray]" = queue.Queue()
        self._stream = None

    def __enter__(self) -> "MicrophoneStream":
        """Starts the microphone stream."""
        try:
            import sounddevice as sd
        except ImportError as exc:
            raise RuntimeError(
                "The 'sounddevice' package is required for microphone input."
            ) from exc

        def callback(indata, frames, time, status):
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
        """Stops the microphone stream."""
        if self._stream is not None:
            self._stream.stop()
            self._stream.close()
            self._stream = None

    def __iter__(self) -> Generator[np.ndarray, None, None]:
        """Yields audio chunks from the microphone."""
        while True:
            yield self._queue.get()


def _run_visualizer(
    source: Iterable[np.ndarray],
    config: SpectrumConfig,
    scale: str,
    refresh_rate: float,
) -> None:
    """Runs the Matplotlib-based visualizer for the FFT spectrum.

    Args:
        source: An iterable of audio chunks.
        config: The FFT configuration.
        scale: The magnitude scale ('linear' or 'log').
        refresh_rate: The refresh rate of the plot in frames per second.
    """
    try:
        import matplotlib.pyplot as plt
        from matplotlib.animation import FuncAnimation
    except ImportError:
        print("Matplotlib is required for visualization.")
        return

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
            return (line,)
        freqs, magnitude = chunk_to_fft(chunk, config)
        if scale == "log":
            magnitude = 20 * np.log10(np.maximum(magnitude, 1e-12))
            ax.set_ylim(-120, 0)
        else:
            peak = float(np.max(magnitude))
            ax.set_ylim(0, max(1e-6, peak * 1.2))
        line.set_data(freqs, magnitude)
        return (line,)

    anim = FuncAnimation(
        fig, update, interval=1000 / refresh_rate, blit=True, cache_frame_data=False
    )
    plt.show()


def main(argv: Optional[Iterable[str]] = None) -> int:
    """The main entry point for the script."""
    parser = argparse.ArgumentParser(description=__doc__)
    # ... (rest of the main function remains the same)
    # ... (I will not be modifying the main function as it is well-written)
    # ...
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
