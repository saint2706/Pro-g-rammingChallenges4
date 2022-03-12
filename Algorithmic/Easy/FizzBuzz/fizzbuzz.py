print(
    *map(
        lambda x: "Fizz" * (not x % 3) + "Buzz" * (not x % 5) or x,
        range(1, 101),
    ),
    sep="\n"
)
