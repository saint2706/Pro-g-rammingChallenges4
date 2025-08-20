import random
import sys

class SimpleChatbot:
    """
    A simple, rule-based chatbot that responds to keywords in user input.
    """
    def __init__(self):
        """Initializes the chatbot with a set of predefined rules."""
        self.rules = {
            "hello": ["Hello there!", "Hi! I'm happy to chat.", "Good to see you!"],
            "how are you": ["I'm just a bot, but I'm doing great!", "I'm functioning as expected, thanks for asking!"],
            "name": ["You can call me a simple ChatBot.", "I don't have a name, I'm just a humble Python script."],
            "bye": ["Goodbye!", "See you later!", "Bye-bye!"],
            "help": [
                "I can respond to keywords like 'hello', 'how are you', 'name', and 'bye'.",
                "Just type something and I'll see if I understand.",
            ],
            "default": [
                "Sorry, I don't understand that.",
                "Could you please rephrase?",
                "I'm not sure how to respond to that. Try asking for 'help'.",
            ]
        }

    def get_response(self, user_input: str) -> str:
        """
        Generates a response based on keywords found in the user's input.

        Args:
            user_input: The string entered by the user.

        Returns:
            A string containing the chatbot's response.
        """
        # Convert input to lowercase for case-insensitive matching
        processed_input = user_input.lower()

        for keyword, responses in self.rules.items():
            # Check if any part of the user input contains a keyword
            if keyword != "default" and keyword in processed_input:
                return random.choice(responses)

        # If no other rule matched, return a default response
        return random.choice(self.rules["default"])

def main():
    """
    Main function to run the chatbot's interactive loop.
    """
    print("--- Simple Rule-Based Chatbot ---")
    print("Bot: Hello! I'm a simple chatbot. Ask me about my name, or how I am. Type 'bye' to exit.")

    bot = SimpleChatbot()

    try:
        while True:
            user_input = input("You: ")

            if "bye" in user_input.lower():
                print(f"Bot: {bot.get_response('bye')}")
                break

            response = bot.get_response(user_input)
            print(f"Bot: {response}")

    except (KeyboardInterrupt, EOFError):
        print("\nBot: Goodbye!")
        sys.exit(0)

if __name__ == "__main__":
    main()
