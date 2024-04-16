import librosa
import matplotlib.pyplot as plt
import numpy as np

# Load audio file
audio_file = r"Algorithmic\Medium\Music Visualizer\fur elise.mp3"
y, sr = librosa.load(audio_file)

# Calculate the short-time Fourier transform (STFT)
D = np.abs(librosa.stft(y))
DB = librosa.amplitude_to_db(D, ref=np.max)

# Display the spectrogram
plt.figure(figsize=(12, 8))
librosa.display.specshow(DB, sr=sr, x_axis="time", y_axis="log")
plt.colorbar(format="%+2.0f dB")
plt.title("Spectrogram")
plt.show()
