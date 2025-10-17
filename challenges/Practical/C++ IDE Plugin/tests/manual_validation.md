# Manual Validation Checklist

Use this checklist to verify the Sublime Text C++ IDE Plugin functions correctly after installation.

## Environment Setup

- [ ] Sublime Text restarted after copying the plugin into `Packages/User`.
- [ ] `libclang_path` or `libclang_search_paths` configured to resolve the correct `libclang` shared library.
- [ ] `clang_arguments` updated with include paths for the sample project.

## Auto-Completion

1. [ ] Open the sample project (or any C++ workspace) in Sublime Text.
2. [ ] Open a `.cpp` file and start typing an identifier—confirm the completion popup contains clang-powered suggestions.
3. [ ] Modify the file without saving and request completions again—confirm completions reflect the unsaved changes.

## Navigation Commands

1. [ ] Place the caret on a symbol declaration and run **C++ IDE Plugin: Go to Definition**. Verify the definition opens in the correct file/line.
2. [ ] Place the caret on a symbol usage and run **C++ IDE Plugin: Go to Declaration**. Verify the declaration is located.
3. [ ] Attempt navigation on an unknown symbol and confirm a status message indicates no match (ensuring graceful failure).

## Symbol Indexing

1. [ ] Run **C++ IDE Plugin: Index Symbols** from the command palette.
2. [ ] Wait for the status message indicating the number of indexed symbols.
3. [ ] Run **C++ IDE Plugin: Show Cached Symbols** and confirm the quick panel lists symbols.
4. [ ] Select a symbol from the list and verify Sublime jumps to the stored location.

## clangd Diagnostics

1. [ ] Run **C++ IDE Plugin: Run clangd --check** on an open file.
2. [ ] Confirm diagnostics (or success message) appear in the output panel.
3. [ ] Introduce a deliberate syntax error and rerun the command to ensure clangd reports the issue.

## Caching Behaviour

- [ ] Open multiple large C++ files sequentially and confirm navigation remains responsive (translation-unit cache).
- [ ] Clear the cache directory and rebuild the index—confirm new cache files are created.

## Packaging Smoke Test

1. [ ] Run `python3 scripts/package.py` to build `C++IDEPlugin.sublime-package`.
2. [ ] Move the archive into `Installed Packages/` and remove the source folder from `Packages/User/`.
3. [ ] Restart Sublime and confirm the commands/settings remain available.

Record observations or anomalies below:

```
Notes:
- 
- 
```
