import os
import time


zero = [
    "  ___    ",
    " / _ \\   ",
    "| | | |  ",
    "| | | |  ",
    "| |_| |  ",
    " \___/   ",
]

one = [" __   ", "/_ |  ", " | |  ", " | |  ", " | |  ", " |_|  "]

two = [
    " ___     ",
    "|__ \\    ",
    "   ) |   ",
    "  / /    ",
    " / /_    ",
    "|____|   ",
]

three = [
    " ____    ",
    "|___ \\   ",
    "  __) |  ",
    " |__ <   ",
    " ___) |  ",
    "|____/   ",
]

four = [
    " _  _    ",
    "| || |   ",
    "| || |_  ",
    "|__   _| ",
    "   | |   ",
    "   |_|   ",
]
five = [
    " _____   ",
    "| ____|  ",
    "| |__    ",
    "|___ \\   ",
    " ___) |  ",
    "|____/   ",
]
six = [
    "   __    ",
    "  / /    ",
    " / /_    ",
    "|  _ \\   ",
    "| (_) |  ",
    " \___/   ",
]

seven = [
    " ______  ",
    "|____  | ",
    "    / /  ",
    "   / /   ",
    "  / /    ",
    " /_/     ",
]

eight = [
    "  ___    ",
    " / _ \\   ",
    "| (_) |  ",
    " > _ <   ",
    "| (_) |  ",
    " \___/   ",
]

nine = [
    "  ___    ",
    " / _ \\   ",
    "| (_) |  ",
    " \__, |  ",
    "   / /   ",
    "  /_/    ",
]

colon = ["  _ ", " (_)", "    ", "  _ ", " (_)", "    "]

space = [
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
]

clockNumbers = [zero, one, two, three, four, five, six, seven, eight, nine]

while True:
    time.sleep(1)
    os.system("cls" if os.name == "nt" else "clear; date; cal")
    h = time.strftime("%H")
    m = time.strftime("%M")
    s = time.strftime("%S")
    clock = str(h + ":" + m + ":" + s)

    print(clock)

    for i in range(6):
        print(
            clockNumbers[int(h[0])][i]
            + space[i]
            + clockNumbers[int(h[1])][i]
            + colon[i]
            + clockNumbers[int(m[0])][i]
            + space[i]
            + clockNumbers[int(m[1])][i]
            + colon[i]
            + clockNumbers[int(s[0])][i]
            + space[i]
            + clockNumbers[int(s[1])][i]
        )
