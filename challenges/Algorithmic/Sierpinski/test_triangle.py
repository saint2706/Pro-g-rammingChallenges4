from triangle import generate_sierpinski_lines, main as tri_main


def test_basic_generation():
    lines = generate_sierpinski_lines(4, "*")
    assert len(lines) == 4
    # Top line should start with no leading spaces
    assert lines[0].startswith("")
    # Last line should contain exactly one symbol (apex row widens upward in our orientation)
    assert "*" in lines[0]


def test_invalid_size():
    # size 0 should trigger error via CLI
    rc = tri_main(["--size", "0"])
    assert rc == 1


def test_json_output():
    rc = tri_main(["--size", "8", "--json"])
    assert rc == 0


def test_custom_char():
    lines = generate_sierpinski_lines(4, "#")
    assert any("#" in ln for ln in lines)
