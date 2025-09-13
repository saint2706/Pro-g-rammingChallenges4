"""mv.py - Audio spectrogram / mel-spectrogram visualizer.

Modernized version with: dataclass config, argparse CLI, optional mel scale,
headless saving, JSON metadata output, and clearer structure for extension.

Key Features
============
* Graceful handling of missing optional dependency (librosa)
* Configurable FFT window (n_fft) and hop length
* Linear (STFT magnitude) or Mel power spectrogram
* Automatic dB scaling with reference to max value
* Color map selection and dynamic range control
* Headless mode for batch rendering (no GUI required)
* Save figure to file (PNG/SVG, etc.)
* JSON metadata output for integration with other tools
* Synthetic tone generation for quick demos (no external file needed)

Examples
--------
1) Basic spectrogram (GUI window):
    python mv.py -f "path/to/audio.wav"

2) Mel spectrogram saved to file without showing window:
    python mv.py -f song.mp3 --mel --save mel.png --headless

3) Generate a synthetic test tone (A4 + noise) and plot:
    python mv.py --synthetic --duration 2.5 --freq 440

4) Output JSON metadata only (no plot):
    python mv.py -f song.mp3 --json --no-plot
"""

from __future__ import annotations

import argparse
import json
import math
import os
import sys
from dataclasses import dataclass
from typing import Iterable, Optional

import numpy as np

# Optional dependency handling: we do not import librosa at module import time
# for faster CLI help and to allow JSON-only operations that may not require it.
try:  # pragma: no cover - import failure path simple & deterministic
    import librosa  # type: ignore
    import librosa.display  # type: ignore

    _LIBROSA_AVAILABLE = True
except Exception:  # broad: user environment variations
    _LIBROSA_AVAILABLE = False

    # Provide minimal sentinel to satisfy type checkers when attribute accessed.
    class _LibrosaStub:  # pragma: no cover - only used when missing
        def __getattr__(self, name):  # noqa: D401
            raise RuntimeError(
                "librosa is required for this operation; install it via 'pip install librosa'"
            )

    librosa = _LibrosaStub()  # type: ignore

try:  # pragma: no cover - headless path tested indirectly
    import matplotlib.pyplot as plt
except Exception as e:  # pragma: no cover
    print("Error: matplotlib is required for plotting.", file=sys.stderr)
    raise


# ---------------------------- Data Model ---------------------------- #
@dataclass(slots=True)
class VisualizerConfig:
    file: Optional[str] = None
    synthetic: bool = False
    duration: float = 2.0
    freq: float = 440.0
    sample_rate: int = 22_050
    n_fft: int = 2048
    hop_length: int = 512
    mel: bool = False
    n_mels: int = 128
    cmap: str = "magma"
    dynamic_range: float = 80.0  # dB
    save_path: Optional[str] = None
    json_meta: bool = False
    headless: bool = False
    no_plot: bool = False
    title: Optional[str] = None

    def validate(self) -> None:
        if self.file is None and not self.synthetic:
            raise ValueError("Either --file or --synthetic must be specified")
        if self.duration <= 0:
            raise ValueError("Duration must be positive")
        if self.n_fft <= 0 or (self.n_fft & (self.n_fft - 1)) != 0:
            # Not strictly required to be power-of-two, but is a best practice for speed
            pass  # Could warn; leaving flexible.
        if self.hop_length <= 0:
            raise ValueError("hop_length must be positive")
        if self.mel and self.n_mels <= 0:
            raise ValueError("n_mels must be positive when using mel spectrogram")


# ---------------------------- Audio Loading ---------------------------- #
def generate_synthetic_tone(cfg: VisualizerConfig) -> tuple[np.ndarray, int]:
    """Generate a simple sine tone with light noise for demonstration."""
    t = np.linspace(
        0, cfg.duration, int(cfg.sample_rate * cfg.duration), endpoint=False
    )
    y = 0.8 * np.sin(2 * math.pi * cfg.freq * t) + 0.02 * np.random.randn(len(t))
    return y.astype(np.float32), cfg.sample_rate


def load_audio(cfg: VisualizerConfig) -> tuple[np.ndarray, int]:
    if cfg.synthetic:
        return generate_synthetic_tone(cfg)
    if not _LIBROSA_AVAILABLE:
        raise RuntimeError(
            "librosa is required to load audio files; install via 'pip install librosa'"
        )
    if not cfg.file or not os.path.exists(cfg.file):
        raise FileNotFoundError(f"Audio file not found: {cfg.file}")
    y, sr = librosa.load(cfg.file, sr=cfg.sample_rate)
    return y, int(sr)


# ---------------------------- Spectrogram Computation ---------------------------- #
def compute_spectrogram(
    y: np.ndarray, sr: int, cfg: VisualizerConfig
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Return (S_db, times, freqs) for linear magnitude spectrogram in dB."""
    if not _LIBROSA_AVAILABLE:
        raise RuntimeError("librosa is required for spectrogram computation")
    stft = librosa.stft(y, n_fft=cfg.n_fft, hop_length=cfg.hop_length)
    mag = np.abs(stft)
    S_db = librosa.amplitude_to_db(mag, ref=np.max)
    freqs = librosa.fft_frequencies(sr=sr, n_fft=cfg.n_fft)
    times = librosa.frames_to_time(
        np.arange(S_db.shape[1]), sr=sr, hop_length=cfg.hop_length
    )
    # Clamp dynamic range
    if cfg.dynamic_range > 0:
        S_db = np.maximum(S_db, S_db.max() - cfg.dynamic_range)
    return S_db, times, freqs


def compute_mel_spectrogram(
    y: np.ndarray, sr: int, cfg: VisualizerConfig
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Return (S_db, times, mel_freqs) for mel spectrogram in dB."""
    if not _LIBROSA_AVAILABLE:
        raise RuntimeError("librosa is required for mel spectrogram computation")
    mel = librosa.feature.melspectrogram(
        y=y, sr=sr, n_fft=cfg.n_fft, hop_length=cfg.hop_length, n_mels=cfg.n_mels
    )
    S_db = librosa.power_to_db(mel, ref=np.max)
    if cfg.dynamic_range > 0:
        S_db = np.maximum(S_db, S_db.max() - cfg.dynamic_range)
    times = librosa.frames_to_time(
        np.arange(S_db.shape[1]), sr=sr, hop_length=cfg.hop_length
    )
    mel_freqs = librosa.mel_frequencies(n_mels=cfg.n_mels)
    return S_db, times, mel_freqs


# ---------------------------- Plotting ---------------------------- #
def plot_data(
    S_db: np.ndarray, times: np.ndarray, freqs: np.ndarray, cfg: VisualizerConfig
) -> None:
    if cfg.no_plot:
        return
    # If headless, use non-interactive backend
    if cfg.headless:
        import matplotlib

        matplotlib.use("Agg")  # pragma: no cover (environment dependent)

    fig, ax = plt.subplots(figsize=(12, 6))
    if cfg.mel:
        # y-axis: Mel frequencies not log scaled in the same manner; show as index or convert
        img = librosa.display.specshow(
            S_db,
            x_axis="time",
            y_axis="mel",
            sr=cfg.sample_rate,
            hop_length=cfg.hop_length,
            cmap=cfg.cmap,
            ax=ax,
        )
        ax.set_ylabel("Mel Frequency (bins)")
    else:
        img = librosa.display.specshow(
            S_db,
            x_axis="time",
            y_axis="log",
            sr=cfg.sample_rate,
            hop_length=cfg.hop_length,
            cmap=cfg.cmap,
            ax=ax,
        )
        ax.set_ylabel("Frequency (Hz)")
    fig.colorbar(img, ax=ax, format="%+2.0f dB")
    ax.set_title(cfg.title or ("Mel Spectrogram" if cfg.mel else "Spectrogram"))
    ax.set_xlabel("Time (s)")
    fig.tight_layout()

    if cfg.save_path:
        fig.savefig(cfg.save_path, dpi=150)
        print(f"Saved figure to {cfg.save_path}")
    if not cfg.headless:
        plt.show()
    plt.close(fig)


# ---------------------------- CLI / Orchestration ---------------------------- #
def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Audio spectrogram / mel-spectrogram visualizer"
    )
    p.add_argument("-f", "--file", help="Input audio file (wav/mp3/flac/etc.)")
    p.add_argument(
        "--synthetic",
        action="store_true",
        help="Generate synthetic sine tone instead of reading file",
    )
    p.add_argument(
        "--duration",
        type=float,
        default=2.0,
        help="Duration of synthetic tone (seconds)",
    )
    p.add_argument(
        "--freq", type=float, default=440.0, help="Frequency of synthetic tone (Hz)"
    )
    p.add_argument("--sample-rate", type=int, default=22_050, help="Target sample rate")
    p.add_argument("--n-fft", type=int, default=2048, help="FFT window size")
    p.add_argument("--hop-length", type=int, default=512, help="Hop length")
    p.add_argument("--mel", action="store_true", help="Use mel spectrogram")
    p.add_argument("--n-mels", type=int, default=128, help="Number of mel bands")
    p.add_argument("--cmap", default="magma", help="Matplotlib colormap")
    p.add_argument(
        "--dynamic-range",
        type=float,
        default=80.0,
        help="Dynamic range (dB) to display from peak",
    )
    p.add_argument("--save", dest="save_path", help="Save figure to this path")
    p.add_argument(
        "--json",
        action="store_true",
        dest="json_meta",
        help="Output JSON metadata (no effect on plotting)",
    )
    p.add_argument(
        "--headless",
        action="store_true",
        help="Use non-interactive backend and do not show window",
    )
    p.add_argument(
        "--no-plot", action="store_true", help="Skip plotting (useful with --json)"
    )
    p.add_argument("--title", help="Custom plot title")
    return p


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    cfg = VisualizerConfig(
        file=args.file,
        synthetic=args.synthetic,
        duration=args.duration,
        freq=args.freq,
        sample_rate=args.sample_rate,
        n_fft=args.n_fft,
        hop_length=args.hop_length,
        mel=args.mel,
        n_mels=args.n_mels,
        cmap=args.cmap,
        dynamic_range=args.dynamic_range,
        save_path=args.save_path,
        json_meta=args.json_meta,
        headless=args.headless,
        no_plot=args.no_plot,
        title=args.title,
    )

    try:
        cfg.validate()
    except Exception as e:
        print(f"Config error: {e}", file=sys.stderr)
        return 2

    if not _LIBROSA_AVAILABLE and not cfg.synthetic:
        print(
            "Error: librosa not installed. Use --synthetic to generate a tone or install with 'pip install librosa'",
            file=sys.stderr,
        )
        return 1

    try:
        y, sr = load_audio(cfg)
        if cfg.mel:
            S_db, times, freqs = compute_mel_spectrogram(y, sr, cfg)
        else:
            S_db, times, freqs = compute_spectrogram(y, sr, cfg)
    except Exception as e:
        print(f"Processing error: {e}", file=sys.stderr)
        return 1

    if cfg.json_meta:
        meta = {
            "sample_rate": sr,
            "duration_seconds": len(y) / sr,
            "frames": S_db.shape[1],
            "freq_bins": S_db.shape[0],
            "mel": cfg.mel,
            "n_fft": cfg.n_fft,
            "hop_length": cfg.hop_length,
            "dynamic_range": cfg.dynamic_range,
            "source": "synthetic" if cfg.synthetic else cfg.file,
        }
        print(json.dumps(meta, indent=2))

    # Plot unless suppressed
    try:
        plot_data(S_db, times, freqs, cfg)
    except Exception as e:
        print(f"Plotting error: {e}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
