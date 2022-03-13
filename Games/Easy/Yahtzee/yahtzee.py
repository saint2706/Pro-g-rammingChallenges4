from random import randint


def singleGame():
    roll1 = roll()
    print(format("You rolled: ", "64s"), roll1)
    print("")
    holdPrompt = input("Swap any dice? (y for HITME, n for HOLD): ")
    print("")
    holdPrompt.replace(" ", "")
    holdPrompt.lower()
    while holdPrompt != "y" and holdPrompt != "n":
        print("INCORRECT INPUT\n")
        holdPrompt = input("Swap any dice? (y for HITME, n for HOLD): ")
        print("")
        holdPrompt.replace(" ", "")
        holdPrompt.lower()

    if holdPrompt == "y":
        roll2 = swap(roll1)
        print("")
        print("Swapping:", roll2[1])
        print("")
        print(format("You rolled: ", "64s"), roll2[0])
        print("")
        holdPrompt2 = input("Swap any dice? (y for HITME, n for HOLD): ")
        print("")
        while holdPrompt2 != "y" and holdPrompt2 != "n":
            print("INCORRECT INPUT\n")
            holdPrompt2 = input("Swap any dice? (y for HITME, n for HOLD): ")
            print("")
            holdPrompt2.replace(" ", "")
            holdPrompt2.lower()
        if holdPrompt2 == "y":
            roll3 = swap(roll2[0])
            print("")
            print("Swapping:", roll3[1])
            print("")
            print(format("You rolled: ", "64s"), roll3[0])
            print("")
            stat = rollType(roll3[0])
        else:
            stat = rollType(roll2[0])
    elif holdPrompt == "n":
        stat = rollType(roll1)
    else:
        pass
    return stat


def roll():
    dice = []
    for _ in range(5):
        dice.append(randint(1, 6))
    return dice


def swap(diceList):
    valid = True
    swapDice = input("Type position of each die you'd like to swap (1-5): ")
    swapDice = swapDice.replace(",", "")
    swapDice = swapDice.replace(" ", "")
    swapDiceList = []
    for x in swapDice:
        swapDiceList.append(x)
    for x in swapDiceList:
        x = str(x)
        if x.isdigit():
            x = int(x)
            if x in range(1, 6):
                valid = True
            else:
                valid = False
        else:
            valid = False

    while valid == False:
        print("")
        print("INCORRECT INPUT")
        print("")
        swapDice = input("Type position of each die you'd like to swap (1-5): ")
        swapDice = swapDice.replace(",", "")
        swapDice = swapDice.replace(" ", "")
        swapDiceList = []
        for x in swapDice:
            swapDiceList.append(x)
        for x in swapDiceList:
            x = str(x)
            if x.isdigit():
                x = int(x)
                if x in range(1, 6):
                    valid = True
                else:
                    valid = False
            else:
                valid = False

    swapIndex = []

    for x in swapDiceList:
        swapIndex.append(int(x) - 1)

    for x in swapIndex:
        x = int(x)
        x -= 1

    for x in swapIndex:
        diceList.pop(x)
        diceList.insert(x, randint(1, 6))

    return diceList, swapDiceList


def rollType(diceList):
    counts = []
    for x in diceList:
        counts.append(diceList.count(x))
    diceListNew = sorted(diceList)
    diceListNew = list(set(diceListNew))
    yahtzee = False
    fullHouse = False
    smallStraight = False
    largeStraight = False
    fourOfKind = False
    threeOfKind = False

    if 5 in counts:
        yahtzee = True
        print(format("YAHTZEE", ">80s"))
    elif 3 in counts and 2 in counts:
        fullHouse = True
        print(format("FULL HOUSE", ">80s"))
    elif 3 in counts and 2 not in counts:
        threeOfKind = True
        print(format("THREE OF A KIND", ">80s"))
    elif 4 in counts:
        fourOfKind = True
        print(format("FOUR OF A KIND", ">80s"))
    elif len(diceListNew) == 3:
        print(format("NOTHING SPECIAL", ">80s"))
    elif len(diceListNew) == 4:
        if (
            diceListNew[-2] == diceListNew[-1] - 1
            and diceListNew[-3] == diceListNew[-2] - 1
            and diceListNew[-4] == diceListNew[-3] - 1
        ):
            smallStraight = True
            print(format("SMALL STRAIGHT", ">80s"))
        else:
            print(format("NOTHING SPECIAL", ">80s"))
    elif len(diceListNew) == 5:
        if (
            diceListNew[-2] == diceListNew[-1] - 1
            and diceListNew[-3] == diceListNew[-2] - 1
            and diceListNew[-4] == diceListNew[-3] - 1
            and diceListNew[-5] == diceListNew[-4] - 1
        ):
            largeStraight = True
            print(format("LARGE STRAIGHT", ">80s"))
        elif (
            diceListNew[-2] == diceListNew[-1] - 1
            and diceListNew[-3] == diceListNew[-2] - 1
            and diceListNew[-4] == diceListNew[-3] - 1
        ):
            smallStraight = True
            print(format("SMALL STRAIGHT", ">80s"))
        elif (
            diceListNew[-3] == diceListNew[-2] - 1
            and diceListNew[-4] == diceListNew[-3] - 1
            and diceListNew[-5] == diceListNew[-4] - 1
        ):
            smallStraight = True
            print(format("SMALL STRAIGHT", ">80s"))
        else:
            print(format("NOTHING SPECIAL", ">80s"))
    else:
        pass
    result = (
        yahtzee,
        fullHouse,
        smallStraight,
        largeStraight,
        fourOfKind,
        threeOfKind,
    )
    return result


def main():
    yahtzee = 0
    fullHouse = 0
    smallStraight = 0
    largeStraight = 0
    fourOfKind = 0
    threeOfKind = 0
    statIndex = 0
    yahtzeePer = 0
    fullHousePer = 0
    smallStraightPer = 0
    largeStraightPer = 0
    fourOfKindPer = 0
    threeOfKindPer = 0

    print("")
    print("-" * 80)
    print("")
    print(format("Welcome to Fake Yahtzee!", "61s"), "Written by Coleo94")
    print("")
    print("-" * 80)
    print("")
    print(format("Game 1", ">43s"))
    print("")
    game = singleGame()
    game = list(game)
    for x in game:
        if x == True:
            statIndex = game.index(x)
    if statIndex == 0:
        yahtzee += 1
    elif statIndex == 1:
        fullHouse += 1
    elif statIndex == 2:
        smallStraight += 1
    elif statIndex == 3:
        largeStraight += 1
    elif statIndex == 4:
        fourOfKind += 1
    elif statIndex == 5:
        threeOfKind += 1
    else:
        pass
    print("")
    gameCount = 1
    print("END OF GAME", gameCount)
    print("-" * 80)
    print("")
    gamesPrompt = input("Play another game? (y for yes, n for no): ")
    gamesPrompt.strip()
    gamesPrompt.lower()
    while gamesPrompt != "y" and gamesPrompt != "n":
        print("")
        print("INCORRECT INPUT")
        print("")
        gamesPrompt = input("Play another game? (y for yes, n for no): ")
        gamesPrompt.strip()
        gamesPrompt.lower()
    print("")

    while gamesPrompt == "y":
        gameCount += 1
        print("-" * 80)
        print("")
        print(format("Game", ">43s"), gameCount)
        print("")
        game = singleGame()
        game = list(game)
        for x in game:
            if x == True:
                statIndex = game.index(x)
        if statIndex == 0:
            yahtzee += 1
        elif statIndex == 1:
            fullHouse += 1
        elif statIndex == 2:
            smallStraight += 1
        elif statIndex == 3:
            largeStraight += 1
        elif statIndex == 4:
            fourOfKind += 1
        elif statIndex == 5:
            threeOfKind += 1
        else:
            pass
        print("")
        print(format("END OF GAME"), gameCount)
        print("-" * 80)
        print("")
        gamesPrompt = input("Play another game? (y for yes, n for no): ")
        gamesPrompt.strip()
        gamesPrompt.lower()
        while gamesPrompt != "y" and gamesPrompt != "n":
            print("")
            print("INCORRECT INPUT")
            print("")
            gamesPrompt = input("Play another game? (y for yes, n for no): ")
            gamesPrompt.strip()
            gamesPrompt.lower()
        print("")
    if gamesPrompt == "n":
        yahtzeePer = (yahtzee / gameCount) * 100
        fullHousePer = (fullHouse / gameCount) * 100
        smallStraightPer = (smallStraight / gameCount) * 100
        largeStraightPer = (largeStraight / gameCount) * 100
        fourOfKindPer = (fourOfKind / gameCount) * 100
        threeOfKindPer = (threeOfKind / gameCount) * 100
        print("-" * 80)
        print("")
        print(format("STATS", ">43s"))
        print("")
        print("In", gameCount, "games, you rolled:\n")
        if yahtzee > 1 or yahtzee == 0:
            print("Yahtzees:", yahtzee, int(yahtzeePer), "%")
            print("")
        else:
            print("Yahtzee:", yahtzee, int(yahtzeePer), "%")
            print("")
        if fullHouse > 1 or fullHouse == 0:
            print("Full houses:", fullHouse, int(fullHousePer), "%")
            print("")
        else:
            print("Full house:", fullHouse, int(fullHousePer), "%")
            print("")
        if smallStraight > 1 or smallStraight == 0:
            print("Small straights:", smallStraight, int(smallStraightPer), "%")
            print("")
        else:
            print("Small straight:", smallStraight, int(smallStraightPer), "%")
            print("")
        if largeStraight > 1 or largeStraight == 0:
            print("Large straights:", largeStraight, int(largeStraightPer), "%")
            print("")
        else:
            print("Large straight:", largeStraight, int(largeStraightPer), "%")
            print("")
        if fourOfKind > 1 or fourOfKind == 0:
            print("Fours of a kind:", fourOfKind, int(fourOfKindPer), "%")
            print("")
        else:
            print("Four of a kind:", fourOfKind, int(fourOfKindPer), "%")
            print("")
        if threeOfKind > 1 or threeOfKind == 0:
            print("Threes of a kind:", threeOfKind, int(threeOfKindPer), "%")
            print("")
        else:
            print(
                "Three of a kind:", threeOfKind, round(threeOfKindPer, 2), "%"
            )
            print("")


main()
