import numpy as np
import matplotlib.pyplot as plt
import sys
import os

# Check for librosa dependency and provide a helpful message if it's missing.
try:
    import librosa
    import librosa.display
except ImportError:
    print("Error: The 'librosa' library is required to run this script.", file=sys.stderr)
    print("Please install it using: pip install librosa", file=sys.stderr)
    print("You may also need an audio backend like 'ffmpeg' (recommended) or 'soundfile'.", file=sys.stderr)
    sys.exit(1)

def plot_spectrogram(audio_path: str) -> None:
    """
    Loads an audio file, computes its spectrogram, and displays it using
    matplotlib.

    Args:
        audio_path: The path to the audio file to be analyzed.

    Raises:
        FileNotFoundError: If the specified audio file does not exist.
        Exception: Can raise various exceptions from librosa if the file is
                   corrupt or in an unsupported format.
    """
    if not os.path.exists(audio_path):
        raise FileNotFoundError(f"Audio file not found at the specified path: {audio_path}")

    print(f"Loading audio file: '{os.path.basename(audio_path)}'...")
    try:
        # Load the audio file; y is the time series, sr is the sample rate
        y, sr = librosa.load(audio_path)
    except Exception as e:
        print(f"Error loading audio file with librosa: {e}", file=sys.stderr)
        print("The file might be corrupt or in an unsupported format.", file=sys.stderr)
        sys.exit(1)

    print("Calculating spectrogram...")
    # Calculate the Short-Time Fourier Transform (STFT)
    stft_result = librosa.stft(y)
    # Get the absolute values (magnitudes) of the STFT
    D = np.abs(stft_result)
    # Convert the amplitude spectrogram to a decibel (dB) scaled spectrogram
    DB = librosa.amplitude_to_db(D, ref=np.max)

    # Create the plot
    fig, ax = plt.subplots(figsize=(12, 8))
    # Display the spectrogram
    img = librosa.display.specshow(DB, sr=sr, x_axis="time", y_axis="log", ax=ax)
    # Add a color bar to show the dB scale
    fig.colorbar(img, ax=ax, format="%+2.0f dB")

    ax.set_title(f"Spectrogram: {os.path.basename(audio_path)}")
    ax.set_xlabel("Time (seconds)")
    ax.set_ylabel("Frequency (Hz)")

    plt.tight_layout()
    print("Displaying plot. Close the plot window to exit.")
    plt.show()

def main():
    """
    Main function to get an audio file path from command-line arguments
    and generate its spectrogram.
    """
    print("--- Audio Spectrogram Visualizer ---")

    # Construct the default audio file path in a cross-platform way
    default_audio_file = os.path.join("Algorithmic", "Medium", "Music Visualizer", "fur elise.mp3")

    # Use the file provided in command-line arguments, or the default
    if len(sys.argv) > 1:
        audio_file = sys.argv[1]
    else:
        audio_file = default_audio_file
        print(f"No audio file provided via command-line. Using default: '{audio_file}'")

    try:
        plot_spectrogram(audio_file)
    except FileNotFoundError as e:
        print(f"\nError: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"\nAn unexpected error occurred: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
