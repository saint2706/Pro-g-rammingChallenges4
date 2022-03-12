import sys
from PIL import Image
import numpy as np


def hilo(r, g, b):
    """Function to return the highest and lowest value of a given color

    Args:
        r (int_): red color component
        g (int): green color component
        b (int): blue color component
    """
    if b < g:
        g, b = b, g
    if g < r:
        r, g = g, r
    if b < g:
        g, b = b, g
    return r + b


def complement(r, g, b):
    """Function to return the complimentary color of a given color

    Args:
        r (int_): red color component
        g (int): green color component
        b (int): blue color component

    Returns:
        tuple: tuple of complimentary color
    """
    temp = hilo(r, g, b)
    return tuple(temp - u for u in (r, g, b))


def complementImage(inpName, outName):
    """Function to generate complimentary image

    Args:
        inpName (image path): path to input image
        outName (image path): path to output image
    """
    img = Image.open(inpName)
    in_data = np.asarray(img)

    lo = np.amin(in_data, axis=2, keepdims=True)
    hi = np.amax(in_data, axis=2, keepdims=True)

    out_data = (lo + hi) - in_data
    out_img = Image.fromarray(out_data)
    out_img.save(outName)


def main():
    """Function to run the program"""
    if len(sys.argv) == 3:
        complementImage(sys.argv[1], sys.argv[2])
    else:
        print("Usage: python comp.py <input file> <output file>")
        print("Example: python comp.py in.png out.png")


if __name__ == "__main__":
    main()
