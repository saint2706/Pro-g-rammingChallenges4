def evaluate_postfix(given_exp):

    stack = []

    for character in given_exp:

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
    evaluate_postfix(input("Enter some random postfix Expression = ")),
)
