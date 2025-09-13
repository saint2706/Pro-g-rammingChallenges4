from ToH import (
    towers_of_hanoi,
    towers_of_hanoi_iterative,
    hanoi_state_generator,
    main as toh_main,
)


def collect(gen):
    return list(gen)


def test_recursive_vs_iterative_moves():
    rec = collect(towers_of_hanoi(4, "A", "C", "B"))
    it = collect(towers_of_hanoi_iterative(4, "A", "C", "B"))
    assert rec == it
    assert len(rec) == 2**4 - 1


def test_state_generator():
    states = list(hanoi_state_generator(3, "A", "C", "B"))
    # First state has all disks on A
    assert states[0]["A"] == [3, 2, 1]
    # Last state has all disks on C
    assert states[-1]["C"] == [3, 2, 1]


def test_cli_json_count_only():
    rc = toh_main(["--disks", "5", "--count-only", "--json"])
    assert rc == 0


def test_cli_max_steps():
    rc = toh_main(["--disks", "5", "--max-steps", "3", "--json"])
    assert rc == 0
