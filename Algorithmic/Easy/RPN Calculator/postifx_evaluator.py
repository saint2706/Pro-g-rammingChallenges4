def evaluatePostfix(givenExp):

    stack = []

    for character in givenExp:

        if character.isdigit():
            stack.append(int(character))

        else:
            first = stack.pop()
            second = stack.pop()
            if character == "+":
                stack.append(second + first)
            elif character == "-":
                stack.append(second - first)
            elif character == "x" or character == "*":
                stack.append(second * first)
            elif character == "/":
                stack.append(second // first)

    return stack.pop()


print(
    "The value of the given postfix expression =",
    evaluatePostfix(input("Enter some random postfix Expression = ")),
)
