from PIL import Image
import os

CHARS = ["#", "?", "%", ".", "S", "+", "*", ":", ",", "@"]


def resize(img, new_width=25):
    """
    Resize the image to a new width.
    """
    (old_width, old_height) = img.size
    aspect_ratio = float(old_height) / float(old_width)
    new_height = int(aspect_ratio * new_width)
    img = img.resize((new_width, new_height))
    return img


def pixelsToAscii(image):
    """
    Converts an image to ASCII art.
    """
    pixels = list(map(int, list(image.getdata())))
    pChars = [CHARS[int(pixel / (256 / len(CHARS)))] for pixel in pixels]
    return "".join(pChars)


def imageToAscii(image, new_width=25):
    """
    Converts an image to ASCII art.
    """
    img = resize(image, new_width)
    img = img.convert("L")
    pChars = pixelsToAscii(img)
    finImg = [
        pChars[i : i + new_width] for i in range(0, len(pChars), new_width)
    ]
    return "\n".join(finImg)


def convert(path):
    """
    Converts an image to ASCII art.
    """
    img = Image.open(path)
    f = open(os.path.splitext(path)[0] + ".txt", "w")
    finImg = imageToAscii(img)
    f.write(finImg)
    f.close()


convert(r"Practical\Easy\ImgToASCII\cat.png")
