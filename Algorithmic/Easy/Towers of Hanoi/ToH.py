def toh(disks, source, middle, target):
    if disks == 1:
        yield "Move disk {} from {} to {}".format(disks, source, target)
        return
    toh(disks - 1, source, target, middle)
    yield "Move disk {} from {} to {}.".format(disks, source, target)
    toh(disks - 1, middle, source, target)


instructions = toh(int(input("Enter number of disks:")), "A", "B", "C")
for instruct in instructions:
    print(instruct)
