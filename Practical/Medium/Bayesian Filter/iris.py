import pandas as pd
import argparse
import sys
from typing import Tuple, List

# Add dependency checks for a better user experience.
try:
    from sklearn.datasets import load_iris
    from sklearn.model_selection import train_test_split
    from sklearn.naive_bayes import GaussianNB
    from sklearn.metrics import accuracy_score, classification_report
except ImportError as e:
    library_name = str(e).split("'")[1]
    print(f"Error: The '{library_name}' library is required.", file=sys.stderr)
    print("Please install it using: pip install scikit-learn pandas", file=sys.stderr)
    sys.exit(1)

def load_data(file_path: str, target_column: str) -> Tuple[pd.DataFrame, pd.Series, List[str]]:
    """
    Loads data from a CSV file or the default Iris dataset.

    Args:
        file_path: Path to the CSV file. If None, loads Iris dataset.
        target_column: The name of the column to be used as the target variable.

    Returns:
        A tuple containing features (X), target (y), and target names.
    """
    if file_path:
        print(f"Loading data from '{file_path}'...")
        df = pd.read_csv(file_path)
        X = df.drop(target_column, axis=1)
        y = df[target_column]
        target_names = y.unique().astype(str)
    else:
        print("No file path provided, loading default Iris dataset...")
        iris = load_iris()
        X = pd.DataFrame(iris.data, columns=iris.feature_names)
        y = pd.Series(iris.target)
        target_names = iris.target_names

    return X, y, target_names

def train_and_evaluate(X: pd.DataFrame, y: pd.Series, target_names: List[str], test_size: float):
    """
    Splits data, trains a Gaussian Naive Bayes classifier, and evaluates it.

    Args:
        X: The feature data.
        y: The target data.
        target_names: The names of the target classes for the report.
        test_size: The proportion of the dataset to allocate to the test set.
    """
    # Split the data into training and testing sets
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, random_state=42)
    print(f"Data split into {len(X_train)} training samples and {len(X_test)} testing samples.")

    # Train the Naive Bayes classifier
    print("Training the Gaussian Naive Bayes classifier...")
    clf = GaussianNB()
    clf.fit(X_train, y_train)
    print("Training complete.")

    # Make predictions on the test data
    y_pred = clf.predict(X_test)

    # Evaluate the model
    accuracy = accuracy_score(y_test, y_pred)
    print("\n--- Model Evaluation ---")
    print(f"Accuracy: {accuracy:.2%}")
    print("\nClassification Report:")
    print(classification_report(y_test, y_pred, target_names=target_names))

def main():
    """Main function to parse arguments and run the classifier."""
    parser = argparse.ArgumentParser(description="Train and evaluate a Gaussian Naive Bayes classifier.")
    parser.add_argument("-f", "--file", help="Path to a CSV dataset file. If not provided, the Iris dataset will be used.")
    parser.add_argument("-t", "--target", default="target",
                        help="The name of the target column in the CSV file. Defaults to 'target'.")
    parser.add_argument("--test-size", type=float, default=0.2,
                        help="The proportion of the dataset to use for testing. Defaults to 0.2.")

    args = parser.parse_args()

    try:
        X, y, target_names = load_data(args.file, args.target)
        train_and_evaluate(X, y, target_names, args.test_size)
    except FileNotFoundError:
        print(f"Error: The file '{args.file}' was not found.", file=sys.stderr)
    except KeyError:
        print(f"Error: The target column '{args.target}' was not found in the CSV file.", file=sys.stderr)
    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
