import spacy

# Load the English language model
nlp = spacy.load("en_core_web_sm")


def parse_sentence(sentence):
    # Process the input sentence using spaCy
    doc = nlp(sentence)

    # Extract the context of the sentence
    context = []
    for token in doc:
        context.append((token.text, token.pos_, token.dep_, token.head.text))

    return context


# Example sentence
sentence = "Sachin hit the ball with a bat."

# Parse the sentence and print the context
parsed_sentence = parse_sentence(sentence)
for token in parsed_sentence:
    print(token)
