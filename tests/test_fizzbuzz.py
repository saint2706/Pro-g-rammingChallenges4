from challenges.Algorithmic.FizzBuzz.fizzbuzz import run


def test_run_with_empty_rules():
    result = run(5, rules=())
    assert result == "1\n2\n3\n4\n5"
