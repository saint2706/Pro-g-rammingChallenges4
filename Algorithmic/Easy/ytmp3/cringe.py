import sys

# Note: 'youtube_dl' is not as actively maintained as its successor, 'yt-dlp'.
# For better performance and compatibility, consider installing and using 'yt-dlp'.
# To install the required library: pip install youtube_dl
try:
    from youtube_dl import YoutubeDL
    from youtube_dl.utils import DownloadError
except ImportError:
    print("Error: The 'youtube_dl' library is required to run this script.", file=sys.stderr)
    print("Please install it using: pip install youtube_dl", file=sys.stderr)
    print("For a more up-to-date alternative, consider 'yt-dlp'.", file=sys.stderr)
    sys.exit(1)

def download_youtube_audio_as_mp3(url: str) -> None:
    """
    Downloads the best quality audio from a given YouTube URL and saves it as an MP3 file.

    This function requires `ffmpeg` to be installed and available in the system's PATH
    for the MP3 conversion to work.

    Args:
        url: The YouTube URL of the video to download.

    Raises:
        DownloadError: If youtube_dl encounters an error during download.
    """
    # Options for youtube-dl:
    # - 'format': 'bestaudio/best' selects the best quality audio stream.
    # - 'postprocessors': Specifies that the audio should be extracted and converted to mp3.
    # - 'outtmpl': Sets the output filename template.
    ydl_opts = {
        'format': 'bestaudio/best',
        'postprocessors': [{
            'key': 'FFmpegExtractAudio',
            'preferredcodec': 'mp3',
            'preferredquality': '192',  # Standard MP3 quality
        }],
        'outtmpl': '%(title)s.%(ext)s',
    }

    print(f"\nAttempting to download audio from: {url}")
    with YoutubeDL(ydl_opts) as ydl:
        ydl.download([url])
    print("\nDownload finished successfully!")

def main():
    """
    Main function to run the YouTube audio downloader in a loop.
    """
    print("--- YouTube to MP3 Audio Downloader ---")
    print("Note: This tool requires 'ffmpeg' to be installed on your system.")

    while True:
        try:
            video_url = input("Enter the YouTube URL: ")
            if not video_url.strip():
                print("No URL entered.")
                continue

            download_youtube_audio_as_mp3(video_url)

        except DownloadError as e:
            print(f"\nAn error occurred during download: {e}", file=sys.stderr)
            print("Please check the URL and your network connection.", file=sys.stderr)
        except Exception as e:
            print(f"\nAn unexpected error occurred: {e}", file=sys.stderr)

        # Ask the user if they want to download another video
        while True:
            try_again = input("\nDownload another video? (y/n): ").lower().strip()
            if try_again in ['y', 'yes', 'n', 'no']:
                break
            print("Invalid input. Please enter 'y' or 'n'.")

        if try_again in ['n', 'no']:
            break

    print("Exiting downloader.")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nOperation cancelled by user. Exiting.")
        sys.exit(0)
