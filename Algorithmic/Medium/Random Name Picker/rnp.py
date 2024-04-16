import random

# List of names
names = ["Alice", "Bob", "Charlie", "David", "Eve", "Frank"]


def pick_random_name(names_list):
    shuffled_names = names_list[:]  # Create a copy of the original list
    random.shuffle(shuffled_names)  # Shuffle the names in place
    return random.choice(shuffled_names)  # Pick a random name from the shuffled list


# Pick a random name
random_name = pick_random_name(names)
print("Randomly picked name:", random_name)
