import importlib.util
import sys
from pathlib import Path

MODULE_PATH = Path(__file__).resolve().parents[1] / "buffer.py"
spec = importlib.util.spec_from_file_location("ctedit_buffer", MODULE_PATH)
module = importlib.util.module_from_spec(spec)
assert spec.loader is not None
sys.modules[spec.name] = module
spec.loader.exec_module(module)
TextBuffer = module.TextBuffer


def test_insert_and_newline():
    buf = TextBuffer()
    buf.insert("hello")
    buf.newline()
    buf.insert("world")
    assert buf.lines == ["hello", "world"]
    assert buf.position() == (1, 5)


def test_backspace_merges_lines():
    buf = TextBuffer("hello\nworld\n")
    buf.move_to_end()
    buf.backspace()
    assert buf.lines == ["hello", "worl"]
    buf.backspace()
    buf.backspace()
    assert buf.lines == ["hello", "wo"]


def test_search_and_replace():
    buf = TextBuffer("foo bar baz foo")
    result = buf.search("bar")
    assert result == (0, 4)
    count = buf.replace("foo", "qux", count=1)
    assert count == 1
    assert buf.lines[0].startswith("qux")


def test_save_and_load(tmp_path: Path):
    buf = TextBuffer("hello")
    target = tmp_path / "sample.txt"
    buf.save_to_file(target)
    assert target.read_text() == "hello"
    buf.insert(" world")
    buf.load_from_file(target)
    assert buf.as_text() == "hello"


def test_word_movement():
    buf = TextBuffer("alpha beta\ngamma")
    buf.move_to_start()
    buf.move_word_forward()
    assert buf.position() == (0, 5)
    buf.move_word_forward()
    assert buf.position()[0] == 1
