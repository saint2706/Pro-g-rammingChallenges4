from youtube_dl import YoutubeDL

audio = YoutubeDL({"format": "bestaudio"})
while 1:
    try:
        url = input("Enter youtube url:\n")
        audio.extract_info(url)
    except KeyboardInterrupt:
        print("Couldn't download")
    except Exception as e:
        print(e)
    finally:
        again = int(input("try again? (1/0):\n"))
        if not again:
            break
