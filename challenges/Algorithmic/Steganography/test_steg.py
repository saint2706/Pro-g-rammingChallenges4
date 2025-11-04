from steg import (
    create_dummy_image,
    hide_message_in_image,
    extract_message_from_image,
    open_image,
    main as steg_main,
)


def test_round_trip(tmp_path):
    img_path = tmp_path / "base.png"
    out_path = tmp_path / "out.png"
    create_dummy_image(str(img_path), (40, 40))
    img = open_image(str(img_path))
    hidden = hide_message_in_image(img, "Hello World!")
    hidden.save(out_path)
    extracted = extract_message_from_image(open_image(str(out_path)))
    assert extracted == "Hello World!"


def test_capacity_cli(tmp_path):
    img_path = tmp_path / "c.png"
    create_dummy_image(str(img_path), (10, 10))
    rc = steg_main(["capacity", str(img_path), "--json"])
    assert rc == 0


def test_hide_cli(tmp_path):
    img_path = tmp_path / "h.png"
    out_path = tmp_path / "o.png"
    create_dummy_image(str(img_path), (30, 30))
    rc = steg_main(["hide", str(img_path), str(out_path), "--message", "abc", "--json"])
    assert rc == 0
    rc2 = steg_main(["extract", str(out_path)])
    assert rc2 == 0


def test_oversize_error(tmp_path):
    img_path = tmp_path / "small.png"
    create_dummy_image(str(img_path), (5, 5))
    # 5*5*3 bits = 75 bits = 9 bytes -> message longer should fail
    rc = steg_main(
        ["hide", str(img_path), str(img_path) + ".out", "--message", "X" * 50]
    )
    assert rc == 1
