import requests
from bs4 import BeautifulSoup


def crawl_web_page(url):
    # Send a GET request to the URL
    response = requests.get(url)

    if response.status_code == 200:
        # Parse the HTML content of the web page
        soup = BeautifulSoup(response.content, "html.parser")

        # Extract relevant information from the web page
        title = soup.title.string
        links = [link.get("href") for link in soup.find_all("a", href=True)]

        return title, links
    else:
        print("Failed to fetch the web page")
        return None, None


# URL of the web page to crawl
url = "https://www.example.com"

# Crawl the web page
title, links = crawl_web_page(url)

if title and links:
    print("Title:", title)
    print("Links:")
    for link in links:
        print(link)
