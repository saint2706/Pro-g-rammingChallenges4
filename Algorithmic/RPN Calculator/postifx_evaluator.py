import operator
import sys

def evaluate_rpn(expression: str) -> float:
    """
    Evaluates a Reverse Polish Notation (RPN) expression.

    Args:
        expression: A string containing the RPN expression with tokens
                    separated by spaces.

    Returns:
        The result of the evaluation as a float.

    Raises:
        ValueError: If the expression is malformed, contains invalid tokens,
                    or attempts division by zero.
    """
    tokens = expression.split()
    stack = []

    # Map operator symbols to their corresponding functions
    operators = {
        '+': operator.add,
        '-': operator.sub,
        '*': operator.mul,
        'x': operator.mul,  # Allow 'x' as a multiplication operator
        '/': operator.truediv,
        '//': operator.floordiv,
        '^': operator.pow,
    }

    for token in tokens:
        # Check if the token is a number (integer or float)
        try:
            stack.append(float(token))
        except ValueError:
            if token in operators:
                if len(stack) < 2:
                    raise ValueError(f"Not enough operands for operator '{token}'.")

                operand2 = stack.pop()
                operand1 = stack.pop()

                if token in ('/', '//') and operand2 == 0:
                    raise ValueError("Division by zero is not allowed.")

                result = operators[token](operand1, operand2)
                stack.append(result)
            else:
                raise ValueError(f"Invalid token found in expression: '{token}'.")

    if len(stack) != 1:
        raise ValueError("The expression has too many operands.")

    return stack.pop()

def main():
    """
    Main function to get an RPN expression from the user and evaluate it.
    """
    print("--- Reverse Polish Notation (RPN) Calculator ---")
    print("Enter an expression with spaces between tokens (e.g., '10 5 / 3 *').")
    print("Supported operators: +, -, *, x, /, //, ^")

    if len(sys.argv) > 1:
        expression = " ".join(sys.argv[1:])
        print(f"\nEvaluating expression from command-line: '{expression}'")
    else:
        try:
            expression = input("Expression: ")
        except (EOFError, KeyboardInterrupt):
            print("\nNo input provided. Exiting.")
            return

    if not expression.strip():
        print("No expression entered.")
        return

    try:
        result = evaluate_rpn(expression)
        print(f"Result: {result}")
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
