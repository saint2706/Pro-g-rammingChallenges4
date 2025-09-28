"""Sublime Text C++ IDE Plugin integrating libclang/clangd.

This plugin provides:
- Auto-completion via libclang's codeComplete API.
- Go-to definition/declaration commands.
- Project symbol indexing backed by a persistent cache.
- Settings UI hooks and background indexing helpers.

The implementation is written to be self-contained and
relies only on the standard Sublime Text plugin runtime
and clang's Python bindings (clang.cindex).
"""

import hashlib
import json
import os
import subprocess
import threading
import time
from collections import OrderedDict
from typing import Dict, Iterable, List, Optional, Tuple

import sublime
import sublime_plugin

try:
    from clang import cindex
except Exception:  # pragma: no cover - handled gracefully
    cindex = None  # type: ignore

SETTINGS_FILE = "C++IDEPlugin.sublime-settings"
CACHE_FILE = "clang_symbol_cache.json"


def _hash_content(content: str) -> str:
    return hashlib.sha1(content.encode("utf-8")).hexdigest()


class TranslationUnitCache:
    """An in-memory and on-disk cache for clang translation units."""

    def __init__(self) -> None:
        self._items: "OrderedDict[str, Tuple[cindex.TranslationUnit, str]]" = (
            OrderedDict()
        )
        self._lock = threading.Lock()

    def get(self, key: str, content_hash: str) -> Optional[cindex.TranslationUnit]:
        with self._lock:
            if key in self._items:
                tu, cached_hash = self._items[key]
                if cached_hash == content_hash:
                    # mark as recently used
                    self._items.move_to_end(key)
                    return tu
                # content changed -> drop cache entry
                del self._items[key]
        return None

    def put(
        self, key: str, tu: "cindex.TranslationUnit", content_hash: str, max_items: int
    ) -> None:
        with self._lock:
            self._items[key] = (tu, content_hash)
            self._items.move_to_end(key)
            while len(self._items) > max_items:
                self._items.popitem(last=False)


TRANSLATION_UNIT_CACHE = TranslationUnitCache()
PROJECT_INDEX_LOCK = threading.Lock()


def plugin_loaded() -> None:
    settings = sublime.load_settings(SETTINGS_FILE)
    settings.add_on_change("reload", lambda: _configure_clang(settings))
    _configure_clang(settings)


def plugin_unloaded() -> None:
    settings = sublime.load_settings(SETTINGS_FILE)
    settings.clear_on_change("reload")


def _configure_clang(settings: sublime.Settings) -> None:
    global cindex
    libclang_path = settings.get("libclang_path")
    if libclang_path:
        try:
            cindex.Config.set_library_file(libclang_path)
        except Exception as exc:
            sublime.error_message("Failed to load libclang: {}".format(exc))
            return
    elif settings.get("libclang_search_paths"):
        for candidate in settings.get("libclang_search_paths"):
            lib_file = os.path.join(candidate, "libclang.so")
            if os.path.exists(lib_file):
                try:
                    cindex.Config.set_library_file(lib_file)
                    break
                except Exception:
                    continue
    if cindex is None:
        try:
            from clang import cindex as loaded_index  # type: ignore

            cindex = loaded_index
        except Exception as exc:  # pragma: no cover - surfaces via popup
            sublime.error_message(
                "Unable to import clang.cindex. Install llvm's python bindings.\n{}".format(
                    exc
                )
            )
            return


def _make_index() -> Optional["cindex.Index"]:
    if cindex is None:
        return None
    try:
        return cindex.Index.create()
    except Exception as exc:  # pragma: no cover
        sublime.error_message("Failed to create clang index: {}".format(exc))
        return None


def _get_compile_args(settings: sublime.Settings) -> List[str]:
    args = list(settings.get("clang_arguments") or [])
    project_settings = sublime.active_window().project_data() or {}
    cpp_settings = (
        project_settings.get("cpp") if isinstance(project_settings, dict) else None
    )
    if isinstance(cpp_settings, dict):
        args.extend(cpp_settings.get("extra_args", []))
    return args


def _get_translation_unit(
    view: sublime.View, index: "cindex.Index"
) -> Optional["cindex.TranslationUnit"]:
    file_name = view.file_name() or "<untitled>"
    content = view.substr(sublime.Region(0, view.size()))
    content_hash = _hash_content(content)
    cache_settings = sublime.load_settings(SETTINGS_FILE)
    cache_size = int(cache_settings.get("max_cached_translation_units", 4))
    tu = TRANSLATION_UNIT_CACHE.get(file_name, content_hash)
    if tu:
        return tu

    unsaved = [(file_name, content)]
    args = _get_compile_args(cache_settings)
    options = (
        cindex.TranslationUnit.PARSE_DETAILED_PROCESSING_RECORD
        | cindex.TranslationUnit.PARSE_INCOMPLETE
    )

    try:
        tu = index.parse(file_name, args=args, unsaved_files=unsaved, options=options)
    except Exception:
        compile_commands_dir = cache_settings.get("compile_commands_dir")
        if compile_commands_dir and os.path.isdir(compile_commands_dir):
            cmake_args = list(args)
            cmake_args.append("-I" + compile_commands_dir)
            tu = index.parse(
                file_name, args=cmake_args, unsaved_files=unsaved, options=options
            )
        else:
            raise

    TRANSLATION_UNIT_CACHE.put(file_name, tu, content_hash, cache_size)
    return tu


def _cursor_from_location(
    tu: "cindex.TranslationUnit", file_path: str, row: int, col: int
) -> "cindex.Cursor":
    file_obj = tu.get_file(file_path)
    location = cindex.SourceLocation.from_position(tu, file_obj, row, col)
    return cindex.Cursor.from_location(tu, location)


def _run_clangd(command: List[str], input_payload: str) -> str:
    """Minimal helper to send a single LSP request to clangd."""
    proc = subprocess.Popen(
        command,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        stdout, stderr = proc.communicate(input_payload, timeout=10)
    except subprocess.TimeoutExpired:
        proc.kill()
        raise
    if stderr:
        sublime.status_message("clangd: {}".format(stderr.strip()))
    return stdout


class CppAutoComplete(sublime_plugin.EventListener):
    def on_query_completions(
        self, view: sublime.View, prefix: str, locations: List[int]
    ):
        if not view.match_selector(locations[0], "source.c++"):
            return None

        index = _make_index()
        if index is None:
            return None
        try:
            tu = _get_translation_unit(view, index)
        except Exception as exc:
            sublime.status_message("clang parse failed: {}".format(exc))
            return None
        if tu is None:
            return None

        row, col = view.rowcol(locations[0])
        # clang is 1-indexed
        row += 1
        col += 1
        file_name = view.file_name() or "<untitled>"
        unsaved = [(file_name, view.substr(sublime.Region(0, view.size())))]
        try:
            results = tu.codeComplete(file_name, row, col, unsaved_files=unsaved)
        except Exception as exc:
            sublime.status_message("codeComplete failed: {}".format(exc))
            return None

        completions = []
        if results is None:
            return None
        for candidate in results.results[:50]:
            completion = ""
            insertion = ""
            for chunk in candidate.string:
                completion += chunk.spelling
                if chunk.isKindTypedText():
                    insertion = chunk.spelling
            completion = completion or insertion
            completions.append((completion, insertion or completion))
        flags = sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS
        return (completions, flags)


class CppGoToDefinitionCommand(sublime_plugin.TextCommand):
    def run(self, edit, **kwargs):
        view = self.view
        if not view.match_selector(view.sel()[0].begin(), "source.c++"):
            return

        index = _make_index()
        if index is None:
            return
        try:
            tu = _get_translation_unit(view, index)
        except Exception as exc:
            sublime.status_message("clang parse failed: {}".format(exc))
            return
        if tu is None:
            return

        point = view.sel()[0].begin()
        row, col = view.rowcol(point)
        row += 1
        col += 1
        file_name = view.file_name() or "<untitled>"
        cursor = _cursor_from_location(tu, file_name, row, col)
        target = cursor.referenced or cursor
        location = target.location
        if location and location.file:
            window = view.window()
            if not window:
                return
            window.open_file(
                "{}:{}:{}".format(location.file.name, location.line, location.column),
                sublime.ENCODED_POSITION,
            )
        else:
            sublime.status_message("No definition found via clang")


class CppGoToDeclarationCommand(sublime_plugin.TextCommand):
    def run(self, edit, **kwargs):
        view = self.view
        if not view.match_selector(view.sel()[0].begin(), "source.c++"):
            return
        index = _make_index()
        if index is None:
            return
        try:
            tu = _get_translation_unit(view, index)
        except Exception as exc:
            sublime.status_message("clang parse failed: {}".format(exc))
            return
        if tu is None:
            return

        point = view.sel()[0].begin()
        row, col = view.rowcol(point)
        row += 1
        col += 1
        file_name = view.file_name() or "<untitled>"
        cursor = _cursor_from_location(tu, file_name, row, col)
        target = cursor.get_definition() or cursor
        location = target.location
        if location and location.file:
            window = view.window()
            if not window:
                return
            window.open_file(
                "{}:{}:{}".format(location.file.name, location.line, location.column),
                sublime.ENCODED_POSITION,
            )
        else:
            sublime.status_message("No declaration found via clang")


def _walk_symbols(cursor: "cindex.Cursor") -> Iterable[Dict[str, str]]:
    for child in cursor.get_children():
        if child.kind.is_declaration() and child.spelling:
            location = child.location
            if location and location.file:
                yield {
                    "name": child.spelling,
                    "kind": child.kind.name,
                    "file": location.file.name,
                    "line": location.line,
                    "column": location.column,
                }
        yield from _walk_symbols(child)


class CppIndexSymbolsCommand(sublime_plugin.WindowCommand):
    def run(self):
        window = self.window
        view = window.active_view()
        if not view or not view.match_selector(0, "source.c++"):
            sublime.status_message("Open a C++ file to index symbols")
            return

        index = _make_index()
        if index is None:
            return

        try:
            tu = _get_translation_unit(view, index)
        except Exception as exc:
            sublime.status_message("clang parse failed: {}".format(exc))
            return
        if tu is None:
            return

        settings = sublime.load_settings(SETTINGS_FILE)
        cache_dir = settings.get("cache_directory") or sublime.cache_path()
        os.makedirs(cache_dir, exist_ok=True)
        cache_path = os.path.join(cache_dir, CACHE_FILE)

        def worker():
            symbols = list(_walk_symbols(tu.cursor))
            with PROJECT_INDEX_LOCK:
                with open(cache_path, "w", encoding="utf-8") as handle:
                    json.dump(
                        {"symbols": symbols, "generated": time.time()}, handle, indent=2
                    )
            sublime.status_message("Indexed {} symbols".format(len(symbols)))

        threading.Thread(target=worker, daemon=True).start()


class CppShowCachedSymbolsCommand(sublime_plugin.WindowCommand):
    def run(self):
        settings = sublime.load_settings(SETTINGS_FILE)
        cache_dir = settings.get("cache_directory") or sublime.cache_path()
        cache_path = os.path.join(cache_dir, CACHE_FILE)
        if not os.path.exists(cache_path):
            sublime.status_message("No cached symbols yet")
            return
        with open(cache_path, "r", encoding="utf-8") as handle:
            payload = json.load(handle)
        symbols = payload.get("symbols", [])
        items = ["{name} — {file}:{line}".format(**symbol) for symbol in symbols][:200]
        window = sublime.active_window()
        if not window:
            return

        def on_select(index: int):
            if index == -1:
                return
            symbol = symbols[index]
            window.open_file(
                "{}:{}:{}".format(symbol["file"], symbol["line"], symbol["column"]),
                sublime.ENCODED_POSITION,
            )

        window.show_quick_panel(items, on_select)


class CppClangdCheckCommand(sublime_plugin.WindowCommand):
    """Run clangd's --check mode and surface diagnostics in Sublime."""

    def run(self):
        window = self.window
        view = window.active_view()
        if not view or not view.match_selector(0, "source.c++"):
            sublime.status_message("Open a C++ file to run clangd --check")
            return

        file_name = view.file_name()
        if not file_name:
            sublime.status_message("Save the file before running clangd")
            return

        settings = sublime.load_settings(SETTINGS_FILE)
        clangd_binary = settings.get("clangd_binary") or "clangd"
        compile_commands_dir = settings.get("compile_commands_dir")
        command = [clangd_binary, f"--check={file_name}", "--pretty"]
        if compile_commands_dir:
            command.append(f"--compile-commands-dir={compile_commands_dir}")
        command.extend(settings.get("clangd_additional_flags") or [])

        try:
            result = subprocess.run(
                command,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                check=False,
            )
        except FileNotFoundError:
            sublime.error_message(
                "clangd binary not found. Update 'clangd_binary' in settings."
            )
            return

        output = result.stdout.strip() or "clangd completed with no output."
        panel = window.create_output_panel("clangd_check")
        panel.run_command("erase_view")
        panel.run_command("append", {"characters": output + "\n"})
        window.run_command("show_panel", {"panel": "output.clangd_check"})
        if result.returncode == 0:
            sublime.status_message("clangd check passed")
        else:
            sublime.status_message("clangd reported issues (see output panel)")


class CppOpenSettingsCommand(sublime_plugin.WindowCommand):
    def run(self):
        self.window.run_command(
            "edit_settings",
            {
                "base_file": "Packages/User/{file}".format(file=SETTINGS_FILE),
                "default": _default_settings_template(),
            },
        )


def _default_settings_template() -> str:
    return json.dumps(
        {
            "libclang_path": "",
            "libclang_search_paths": [
                "/usr/lib/llvm-16/lib",
                "/usr/local/opt/llvm/lib",
            ],
            "clang_arguments": [
                "-std=c++20",
                "-Iinclude",
            ],
            "compile_commands_dir": "",
            "clangd_binary": "clangd",
            "clangd_additional_flags": [],
            "max_cached_translation_units": 4,
            "cache_directory": "",
        },
        indent=4,
    )


__all__ = [
    "CppAutoComplete",
    "CppGoToDefinitionCommand",
    "CppGoToDeclarationCommand",
    "CppIndexSymbolsCommand",
    "CppShowCachedSymbolsCommand",
    "CppClangdCheckCommand",
    "CppOpenSettingsCommand",
]
