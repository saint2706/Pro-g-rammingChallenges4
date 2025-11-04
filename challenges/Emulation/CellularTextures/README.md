# Cellular Textures

This solution is a C++ program that generates and benchmarks grayscale cellular (Worley-like) textures. It implements and compares several algorithms for finding the two nearest feature points, which are used to determine the intensity of each pixel.

## Description

The `cell.cpp` program generates a texture by distributing a set of random points on a 2D plane and then, for each pixel, calculating the distance to the two nearest points. The intensity of the pixel is derived from these distances, creating a distinct cellular pattern.

The main purpose of this program is to benchmark the performance of different algorithms for accelerating this process. The implemented algorithms include:
- Brute force
- Incremental Y-sorted pruning
- Y-sorted with SSE vectorization
- Tiled pruning
- Tiled with SSE vectorization
- Recursive spatial subdivision
- A custom binary tree structure

The program outputs the generated texture as a TGA image file (`test.tga`).

## Dependencies

- A C++ compiler that supports C++17 (for `std::filesystem`).
- No external libraries are required.

## Compilation

You can compile the program using a standard C++ compiler like g++ or Clang.

**Using g++:**
```bash
g++ -std=c++17 -O2 challenges/Emulation/CellularTextures/cell.cpp -o cellular_texture
```

**Using Clang:**
```bash
clang++ -std=c++17 -O2 challenges/Emulation/CellularTextures/cell.cpp -o cellular_texture
```

## Usage

After compiling, you can run the executable to generate the texture and see the benchmark results printed to the console.
```bash
./cellular_texture
```
The program will run through the different algorithms, print the time taken by each, and save the resulting texture to `challenges/Emulation/CellularTextures/test.tga`.

## TODO

This solution is a work in progress. Future improvements could include:
- **Command-Line Arguments**: Add command-line arguments to control parameters like image size, number of points, and which algorithms to run.
- **Modern C++**: Replace legacy C-style code (e.g., manual memory management, `printf`) with modern C++ alternatives (`std::vector`, `std::chrono`, iostreams).
- **Multi-threading**: Add support for multi-threading to further accelerate the texture generation process.
- **Cross-Platform Compatibility**: Remove MSVC-specific code (`__declspec(align(16))`) to improve portability.
