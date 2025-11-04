# Eulerian Path Solvers

This directory contains implementations of different algorithms for finding an Eulerian path in a graph. An Eulerian path is a trail in a finite graph that visits every edge exactly once.

## Algorithms

This solution provides three different implementations of Eulerian path solvers:

- **`hierholzer.py` (Python)**: An implementation of Hierholzer's algorithm, which is an efficient method for finding Eulerian paths.
- **`fleury.java` (Java)**: An implementation of Fleury's algorithm, another classic algorithm for finding Eulerian paths.
- **`naive.cpp` (C++)**: A naive, brute-force approach to finding an Eulerian path. This is less efficient and is primarily for educational purposes.

## Usage

Each implementation is a command-line tool that takes a graph file as input and prints the Eulerian path to the console.

### `hierholzer.py` (Python)

This script can be run directly from the command line.

**Example:**
```bash
python "challenges/Emulation/EulerianPath/hierholzer.py" graph.txt
```
*(Note: You will need to create a `graph.txt` file in the correct format.)*

**Input Format:**
The script supports different input formats for the graph, which can be specified with the `--format` argument. See the script's help message for more details:
```bash
python "challenges/Emulation/EulerianPath/hierholzer.py" --help
```

### `fleury.java` (Java)

To run the Java implementation, you will need to compile it first.

**Compilation:**
```bash
javac challenges/Emulation/EulerianPath/fleury.java
```

**Execution:**
```bash
java -cp challenges/Emulation/EulerianPath fleury graph.txt
```

### `naive.cpp` (C++)

To run the C++ implementation, you will need to compile it first.

**Compilation (using g++):**
```bash
g++ -std=c++17 -O2 challenges/Emulation/EulerianPath/naive.cpp -o naive_eulerian_path
```

**Execution:**
```bash
./naive_eulerian_path graph.txt
```
