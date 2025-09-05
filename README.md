# Pro-g-rammingChallenges4
[![](https://wakatime.com/badge/github/saintwithataint/Pro-g-rammingChallenges4.svg)](https://wakatime.com/badge/github/saintwithataint/Pro-g-rammingChallenges4)

Going to be attempting this everytime i wanna learn a language.

![](https://github.com/saintwithataint/Pro-g-rammingChallenges4/blob/8a7441045645b20a2ae862624a495c1802056044/programming%20challenges.png?raw=true)

---

## GUI Applications

This repository now includes several scripts with graphical user interfaces (GUIs) built with Tkinter, providing a user-friendly way to interact with the underlying tools.

### PDF Metadata Tagger

A tool to view and edit the metadata (Title, Author, Subject, etc.) of a PDF file.

**To Run:**
```bash
python "Practical/Easy/PDF Tagger/pdftag_gui.py"
```

### Image to ASCII Art Converter

A tool to convert image files into ASCII art. You can specify the output width and the character set to use for the conversion.

**To Run:**
```bash
python "Practical/Easy/ImgToASCII/convert_gui.py"
```

### Multi-threaded Port Scanner

A tool to scan for open ports on a target host. The GUI allows you to specify the target, port range, and number of concurrent threads, and it displays results in real-time.

**To Run:**
```bash
python "Practical/Easy/Port Scanner/scanner_gui.py"
```

### Radix Base Converter

A simple utility to convert numbers between different bases (Binary, Octal, Decimal, and Hexadecimal).

**To Run:**
```bash
python "Practical/Medium/Radix Base Converter/radix_gui.py"
```

### Markov Chain Sentence Generator

A tool that builds a Markov model from a source text file and generates new, random sentences based on it.

**To Run:**
```bash
python "Practical/Easy/Markov Chain Sentence Generator/mcsg_gui.py"
```

### Seam Carving Image Resizer

A content-aware image resizing tool. It allows you to load an image and intelligently reduce its width or height by removing the lowest-energy "seams".

**To Run:**
```bash
python "Practical/Medium/Seam Carving/resize_gui.py"
```

## Algorithm Visualizations

A selection of scripts that use `matplotlib` to create animated or static visualizations of classic algorithms.

### Dijkstra's Shortest Path Visualizer

Animates the step-by-step process of Dijkstra's algorithm finding the shortest path in a graph. It highlights the current node, visited nodes, and the updated shortest path tree in each frame.

**To Run:**
```bash
python "Algorithmic/Medium/Djikstra/dijkstra_visualizer.py" --start A
```

### Towers of Hanoi Visualizer

Animates the classic recursive solution to the Towers of Hanoi puzzle, showing the disks moving from one peg to another.

**To Run:**
```bash
python "Algorithmic/Easy/Towers of Hanoi/ToH_visualizer.py" 4
```

### Character Frequency Visualizer

Generates a bar chart to visualize the frequency of each character in a given string or text file.

**To Run:**
```bash
python "Algorithmic/Easy/Character Counter/charcount_visualizer.py" -t "hello world"
```
