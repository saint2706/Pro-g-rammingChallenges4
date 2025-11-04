import json

import pytest

from triangle import generate_sierpinski_lines, main as tri_main


def test_basic_generation():
    lines, drawn = generate_sierpinski_lines(4, "*")
    assert len(lines) == 4
    # Top line should start with no leading spaces
    assert lines[0].startswith("")
    # Last line should contain exactly one symbol (apex row widens upward in our orientation)
    assert "*" in lines[0]
    assert drawn == sum(line.count("*") for line in lines)


def test_invalid_size():
    # size 0 should trigger error via CLI
    rc = tri_main(["--size", "0"])
    assert rc == 1


def test_json_output(capsys):
    rc = tri_main(["--size", "8", "--json"])
    assert rc == 0
    captured = capsys.readouterr()
    payload = json.loads(captured.out)
    assert payload["non_space_chars"] == pytest.approx(
        payload["density"] * (8 * 8)
    )


def test_custom_char():
    lines, drawn = generate_sierpinski_lines(4, "#")
    assert any("#" in ln for ln in lines)
    assert drawn == sum(line.count("#") for line in lines)


def test_space_character_counts_drawn_cells(capsys):
    rc = tri_main(["--size", "4", "--json", "--char", " "])
    assert rc == 0
    payload = json.loads(capsys.readouterr().out)
    assert payload["non_space_chars"] == 5
    assert payload["density"] == payload["non_space_chars"] / (4 * 4)
