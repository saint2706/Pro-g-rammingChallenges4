# C++ IDE Plugin for Sublime Text

This project implements challenge 23 from the Practical list: a C++ IDE plugin powered by Clang tooling. The plugin targets **Sublime Text 3/4** and integrates `libclang`/`clangd` features to deliver:

- Intelligent auto-completion.
- Go-to declaration/definition navigation.
- Project-wide symbol indexing with a persistent cache.
- A configuration UI exposed through Sublime's settings interface.
- Optional clangd-driven diagnostics via `clangd --check`.

## Repository Layout

```
challenges/Practical/C++ IDE Plugin/
├── C++ IDE Plugin.sublime-commands   # Command palette entries
├── C++IDEPlugin.sublime-settings     # User-editable settings
├── INSTALL.md                        # Installation and packaging guide
├── Main.sublime-menu                 # Menu integration for settings
├── README.md                         # You are here
├── c_cpp_ide_plugin.py               # Core plugin implementation
├── scripts/
│   └── package.py                    # Helper to build .sublime-package archives
└── tests/
    └── manual_validation.md          # Checklist for manual QA
```

## Features

### Auto-Completion
The plugin hooks into Sublime's completion system and proxies requests to `clang.cindex`'s `codeComplete` API. Unsaved buffers are provided to libclang to ensure completions remain context-aware even before a file is saved.

### Navigation Commands
Two commands—`cpp_go_to_definition` and `cpp_go_to_declaration`—inspect the clang AST at the caret location and open the resolved file/line in Sublime. Both commands are available via the context menu, the command palette, or keyboard shortcuts you define.

### Symbol Indexing and Cache
The `cpp_index_symbols` command walks clang's AST and persists symbol information to a JSON cache (stored under Sublime's cache directory by default). The cache keeps track of symbol name, kind, location, and a generation timestamp. `cpp_show_cached_symbols` surfaces the cache via a quick panel to jump across your project.

### clangd Diagnostics
`cpp_clangd_check` shells out to `clangd --check` (respecting `compile_commands_dir` and any additional flags) to surface diagnostics inside Sublime's output panel. This augments libclang-powered features with clangd's semantic analysis.

### Configurable via Settings
`CppOpenSettingsCommand` opens a split-view settings editor pre-populated with sane defaults. You can specify:

- `libclang_path` or a list of `libclang_search_paths`.
- Additional `clang_arguments` (include paths, `-std` flags, etc.).
- A `compile_commands_dir` to automatically pick up `compile_commands.json`.
- Cache size/locations to tune memory usage.

## Getting Started

1. Install LLVM/Clang on your system (`brew install llvm`, `apt install clang`, etc.).
2. Ensure the Python bindings for clang are available (often packaged as `python3-clang`).
3. Copy the plugin folder into Sublime's `Packages/User` directory (see [INSTALL.md](./INSTALL.md) for a scripted approach).
4. Open the command palette and run **“C++ IDE Plugin: Open Settings”** to configure libclang search paths and compilation arguments that match your project.
5. Open a C++ file and start typing—completions should appear automatically. Use **Go to Definition/Declaration** via the command palette or assign key bindings.
6. Run **“C++ IDE Plugin: Index Symbols”** to build the navigation cache, then **“Show Cached Symbols”** to verify results.
7. (Optional) Run **“C++ IDE Plugin: Run clangd --check”** to surface clangd diagnostics in the output panel.

## Development Notes

- Translation units are cached in memory using an LRU strategy to minimize repeated parsing cost. The cache is keyed by the SHA-1 of the current buffer contents, so unsaved edits invalidate the cache automatically.
- `clangd` support is provided by `_run_clangd`, which can be extended to perform background diagnostics or formatting in future iterations.
- Threading is used for indexing work to keep Sublime responsive.

## Validation

Manual validation steps live under [`tests/manual_validation.md`](./tests/manual_validation.md). Follow the checklist after installation to confirm completions, navigation, and indexing behave as expected.

## Packaging

Packaging instructions are documented in [`INSTALL.md`](./INSTALL.md), covering manual zipping as well as a helper script for Sublime's `.sublime-package` format.

## License

The plugin code inherits the root repository license (MIT). Refer to the top-level [`LICENSE`](../../LICENSE) file.
