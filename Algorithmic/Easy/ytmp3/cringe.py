from youtube_dl import YoutubeDL

audio = YoutubeDL({"format": "bestaudio"})
while 1:
    try:
        url = input("Enter youtube url:\n")
        audio.extract_info(url)
    except:
        print("Couldn't download")
    finally:
        again = int(input("try again? (1/0):\n"))
        if not again:
            break
