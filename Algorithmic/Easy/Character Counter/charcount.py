from pandas import Series

print(Series(list(input("Enter sentence:"))).value_counts().to_string())
