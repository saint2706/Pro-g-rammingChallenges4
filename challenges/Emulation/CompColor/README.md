# Complementary Color Image Transformer

This solution provides a command-line tool, `comp.py`, for transforming images into their complementary color variants using a luminance-preserving inversion technique.

## Description

The script implements a method for finding the complementary color of each pixel that, unlike a naive inversion, preserves the luminance. This is achieved by mirroring each color channel around the midpoint of the pixel's minimum and maximum RGB values. The formula used is:

`new_channel = min(R,G,B) + max(R,G,B) - original_channel`

This technique can produce more visually harmonious results than a simple color inversion.

## Features

- **Single and Batch Processing**: You can process a single image or an entire directory of images.
- **Alpha Channel Preservation**: The script can preserve the alpha channel of PNG images with transparency.
- **JSON Summary**: It can generate a JSON file with detailed statistics about the color transformation for each image.
- **Safe Overwriting**: By default, the script will not overwrite existing files.

## Dependencies

- Python 3
- NumPy (`numpy`)
- Pillow (`Pillow`)

You can install the required dependencies using pip:
```bash
pip install numpy Pillow
```

## Usage

You can run the script from the command line with various options.

### Single Image Processing

To transform a single image and save it to a new file:
```bash
python "challenges/Emulation/CompColor/comp.py" --single input.jpg output.jpg
```

### Batch Processing

To process all images in a directory and save the results with a `_comp` suffix:
```bash
python "challenges/Emulation/CompColor/comp.py" --batch /path/to/images --suffix _comp
```

### Advanced Example

To recursively process a directory, preserve the alpha channel, and generate a JSON summary:
```bash
python "challenges/Emulation/CompColor/comp.py" --batch /path/to/images --recursive --keep-alpha --json summary.json
```

### Command-Line Arguments
- `--single IN OUT`: Process a single input file and save it to the specified output file.
- `--batch PATH`: Process a directory or a single file.
- `--suffix`: The suffix to add to output filenames in batch mode.
- `--dest DIR`: The directory to save the output files to in batch mode.
- `--recursive`: Recursively process subdirectories in batch mode.
- `--keep-alpha`: Preserve the alpha channel if it's present.
- `--json FILE`: Write a JSON summary of the processing statistics to a file.
- `--force`: Overwrite existing output files.
