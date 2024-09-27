import os
import requests
import urllib.parse
import concurrent.futures
import json


class BooruDownloader:
    def __init__(self, base_url, tags, limit=100, download_path="downloads"):
        self.base_url = base_url
        self.tags = tags
        self.limit = limit
        self.download_path = download_path
        self.session = requests.Session()

    def get_image_urls(self):
        url = f"{self.base_url}/index.php?page=dapi&s=post&q=index&json=1&tags={urllib.parse.quote(self.tags)}&limit={self.limit}"
        response = self.session.get(url)
        if response.status_code == 200:
            try:
                data = json.loads(response.text)
                if isinstance(data, list):
                    return [
                        post.get("file_url") for post in data if post.get("file_url")
                    ]
                elif isinstance(data, dict) and "post" in data:
                    return [
                        post.get("file_url")
                        for post in data["post"]
                        if post.get("file_url")
                    ]
                else:
                    print("Unexpected data structure in the API response")
                    return []
            except json.JSONDecodeError:
                print("Error decoding JSON response")
                return []
        else:
            print(f"Error fetching data: HTTP {response.status_code}")
            return []

    def download_image(self, url):
        if url:
            filename = os.path.join(self.download_path, url.split("/")[-1])
            if not os.path.exists(filename):
                response = self.session.get(url)
                if response.status_code == 200:
                    with open(filename, "wb") as f:
                        f.write(response.content)
                    print(f"Downloaded: {filename}")
                else:
                    print(f"Failed to download: {url}")
            else:
                print(f"File already exists: {filename}")
        else:
            print("Invalid URL")

    def download_images(self):
        if not os.path.exists(self.download_path):
            os.makedirs(self.download_path)

        image_urls = self.get_image_urls()

        with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
            executor.map(self.download_image, image_urls)


def main():
    base_url = "https://gelbooru.com"

    tags = input("Enter tags (space-separated): ")
    limit = int(input("Enter number of images to download: "))
    download_path = (
        input("Enter download path (default is 'downloads'): ") or "downloads"
    )

    downloader = BooruDownloader(base_url, tags, limit, download_path)
    downloader.download_images()


if __name__ == "__main__":
    main()
