import subprocess, sys, json, os, pathlib

import pytest

# Import module directly
from atbash import atbash_cipher, main as atbash_main


def test_basic_round_trip():
    text = "Hello, World!"
    enc = atbash_cipher(text)
    dec = atbash_cipher(enc)
    assert dec == text
    assert enc != text  # ensure transformation occurred for letters


def test_non_alpha_preserved():
    sample = "1234!? _-"
    assert atbash_cipher(sample) == sample


def test_cli_json(tmp_path):
    # Run via module function to avoid spawning new python process
    input_text = "ABC xyz"
    rc = atbash_main(["--text", input_text, "--json"])
    assert rc == 0


def test_cli_save(tmp_path):
    infile = tmp_path / "in.txt"
    outfile = tmp_path / "out.txt"
    infile.write_text("Secret", encoding="utf-8")
    rc = atbash_main(["--file", str(infile), "--save", str(outfile)])
    assert rc == 0
    assert outfile.read_text(encoding="utf-8") == atbash_cipher("Secret")


def test_conflicting_sources_error():
    # Should raise ValueError inside main when multiple sources given
    # We call resolve via main expecting non-zero return.
    rc = atbash_main(["--text", "A", "--stdin"])  # stdin flag plus text
    assert rc == 1
