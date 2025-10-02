# Producer/Consumer Native Implementations

This folder contains reference implementations of the bounded-buffer producer/consumer
problem in C, C++, and Java. The table below lists the source files that participate
in the shared native build, along with the compilers/VMs that are expected.

| Project ID   | Source file | Language | Required toolchain | Notes |
| ------------ | ----------- | -------- | ------------------ | ----- |
| `pc-c`       | `pc.c`      | C (POSIX threads) | GCC or any C11 compiler with pthreads support | CLI supports `-h/--help` for usage. |
| `pc-cpp`     | `pc.cpp`    | C++17     | G++/Clang with C++17 and pthreads | Recommended flags: `-std=c++17 -pthread`. |
| `pcLL-java`  | `pcLL.java` | Java 11+  | OpenJDK (javac/java) | Uses intrinsic locks with optional verbose logging. |
| `pcSem-java` | `pcSem.java`| Java 11+  | OpenJDK (javac/java) | Semaphore-based variant with similar CLI. |

All implementations only depend on the standard libraries provided by the selected
compiler or JVM.

## Shared build script

For repeatable local builds the repository provides a shared driver at
`tools/build_native.py`. The script discovers the native projects in this folder,
compiles them into a temporary build directory, and performs a lightweight
functional smoke-test for each artifact.

Typical usage from the repository root:

```bash
python tools/build_native.py --base "Practical/Producer Consumer"
```

Useful options:

- `--list` — Show the discovered projects and exit.
- `--only pc-cpp` — Limit the build to a specific project (repeatable flag).
- `--keep-build` — Preserve the temporary build directory at `build/native/` for
  debugging the compiled artifacts.

When running the script locally ensure that `gcc`, `g++`, and an OpenJDK runtime
(javac/java) are available on your `PATH`. The CI workflow installs these
prerequisites automatically and executes the same smoke tests for every project
via the `native-build` GitHub Actions job.
