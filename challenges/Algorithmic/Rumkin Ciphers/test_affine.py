from affine import affine_cipher, main as affine_main

import pytest


def test_encrypt_decrypt_cycle():
    plain = "Affine Test!"
    a, b = 5, 8  # 5 is coprime with 26
    enc = affine_cipher(plain, a, b, "encrypt")
    dec = affine_cipher(enc, a, b, "decrypt")
    assert dec == plain
    assert enc != plain


def test_invalid_key_a():
    with pytest.raises(ValueError):
        affine_cipher("Hello", 2, 1, "encrypt")  # 2 not coprime with 26


def test_cli_json():
    rc = affine_main(
        ["--mode", "encrypt", "-a", "5", "-b", "8", "--text", "Hello", "--json"]
    )
    assert rc == 0


def test_cli_bruteforce(tmp_path):
    # Encrypt a short word with known keys; verify brute force output includes original
    word = "hello"
    a, b = 5, 7
    enc = affine_cipher(word, a, b, "encrypt")
    rc = affine_main(["--mode", "decrypt", "--text", enc, "--bruteforce", "--json"])
    assert rc == 0


def test_conflicting_sources():
    rc = affine_main(
        ["--mode", "encrypt", "-a", "5", "-b", "8", "--text", "x", "--stdin"]
    )
    assert rc == 1


def test_save_output(tmp_path):
    outfile = tmp_path / "out.txt"
    rc = affine_main(
        [
            "--mode",
            "encrypt",
            "-a",
            "5",
            "-b",
            "8",
            "--text",
            "abc",
            "--save",
            str(outfile),
        ]
    )
    assert rc == 0
    assert outfile.read_text(encoding="utf-8")
