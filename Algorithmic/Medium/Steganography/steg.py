from PIL import Image, ImageSequence


def hide_data(image_path, data_to_hide, output_path):
    image = Image.open(image_path)
    frames = [frame.copy() for frame in ImageSequence.Iterator(image)]
    data_index = 0
    data_length = len(data_to_hide)
    data_to_hide += "|"

    for frame in frames:
        for x in range(frame.width):
            for y in range(frame.height):
                pixel = list(frame.getpixel((x, y)))
                for i in range(3):  # Modify each color channel (RGB)
                    if data_index < data_length:
                        pixel[i] = pixel[i] & 0xFE | (
                            ord(data_to_hide[data_index]) >> 7
                        )
                        data_to_hide[data_index] = chr(
                            ((ord(data_to_hide[data_index]) << 1) & 0xFF)
                        )
                        data_index += 1
                frame.putpixel((x, y), tuple(pixel))

    frames[0].save(output_path, save_all=True, append_images=frames[1:])


def extract_data(image_path):
    image = Image.open(image_path)
    extracted_data = ""

    for frame in ImageSequence.Iterator(image):
        for x in range(frame.width):
            for y in range(frame.height):
                pixel = frame.getpixel((x, y))
                char = 0

                for i in range(3):
                    char = (char << 1) | (pixel[i] & 1)

                if chr(char) == "|":
                    return extracted_data

                extracted_data += chr(char)

    return extracted_data


# Usage example
image_path = "image.gif"
output_path = "output.gif"
data_to_hide = "momenta de bruh"
hide_data(image_path, data_to_hide, output_path)

extracted_data = extract_data(output_path)
print("Extracted data:", extracted_data)
