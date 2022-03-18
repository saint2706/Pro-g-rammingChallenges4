import numpy as np
import random
import re
from collections import defaultdict

fpath = r"Practical\Easy\Markov Chain Sentence Generator\crime_and_punishment"
with open(fpath, encoding="utf8") as f:
    model_text = f.read()
    token_text = [word for word in re.split("\W+", model_text) if word != ""]

markov_dict = defaultdict(lambda: defaultdict(int))

last_word = token_text[0].lower()
for word in token_text[1:]:
    word = word.lower()
    markov_dict[last_word][word] += 1
    last_word = word


def gen_sentence(mark_dict, distance, start=None):
    if distance <= 0:
        return []
    if not start:
        start = random.choice(list(mark_dict.keys()))
    weights = np.array(list(markov_dict[start].values()), dtype=np.float64)
    weights = weights / weights.sum()
    choices = list(markov_dict[start].keys())
    chosen = np.random.choice(choices, None, p=weights)
    return [chosen] + gen_sentence(markov_dict, distance - 1, chosen)


distance = int(input("How many word long sentences to generate?\n"))
num = int(input("Number of sentences?\n"))
start = input("Any word to start with?\nPress Enter to skip.\n")
print()
for i in range(num):
    print(" ".join(gen_sentence(markov_dict, distance, start)), "\n")
