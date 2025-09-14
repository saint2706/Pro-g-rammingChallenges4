
import java.util.Arrays;
import java.util.Random;
import java.util.Scanner;

/**
 * A simple command-line Yahtzee game for one player.
 * Demonstrates dice rolling, rerolling, and scoring logic for educational
 * purposes.
 *
 * Modernized, documented, and optimized for clarity and maintainability.
 */
public class Yahtzee {
    /**
     * Main entry point for the Yahtzee game.
     * Handles game loop, dice rolling, rerolling, and scoring.
     */
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int play = 1, totalScore = 0, filledCategories = 0;
        int[] usedCategories = new int[15]; // Tracks which categories have been scored

        System.out.println("Welcome to Yahtzee! Score in all 15 categories to finish the game.");
        while (play == 1 && filledCategories < 15) {
            filledCategories = 0;
            int[] dice = new int[5];
            int rerollCount = 0;
            Die die = new Die();

            // Initial roll
            for (int i = 0; i < 5; i++) {
                die.roll();
                dice[i] = die.get();
            }
            printDice(dice);

            // Up to two rerolls
            while (rerollCount < 2) {
                int rerollNum = inputInt(scanner, "How many dice do you want to reroll? (0-5): ", 0, 5);
                if (rerollNum == 0)
                    break;
                int[] rerollIndices = new int[rerollNum];
                for (int j = 0; j < rerollNum; j++) {
                    rerollIndices[j] = inputInt(scanner, "Which die to reroll (1-5)? ", 1, 5) - 1;
                }
                for (int idx : rerollIndices) {
                    die.roll();
                    dice[idx] = die.get();
                }
                rerollCount++;
                printDice(dice);
            }

            // Scoring
            Winnings prize = new Winnings(scanner);
            prize.checkWinnings(dice, usedCategories);
            usedCategories[prize.choice() - 1] = 1;
            for (int cat : usedCategories)
                filledCategories += cat;
            totalScore += prize.score();
            System.out.println("Your total score is: " + totalScore);
            if (filledCategories < 15) {
                play = inputInt(scanner, "Do you want to play again? (1=yes, 2=no): ", 1, 2);
            } else {
                System.out.println("GAME OVER! Final score: " + totalScore);
            }
        }
        scanner.close();
    }

    /**
     * Print the current dice values in a user-friendly format.
     */
    private static void printDice(int[] dice) {
        System.out.println("Current dice:");
        for (int i = 0; i < dice.length; i++) {
            System.out.println("Die " + (i + 1) + ": " + dice[i]);
        }
    }

    /**
     * Prompt the user for an integer input within a specified range.
     * Handles invalid input gracefully.
     */
    public static int inputInt(Scanner scanner, String prompt, int min, int max) {
        int result = min - 1;
        while (result < min || result > max) {
            System.out.print(prompt);
            try {
                result = Integer.parseInt(scanner.nextLine().trim());
            } catch (NumberFormatException e) {
                System.out.println("Invalid input. Please enter a number between " + min + " and " + max + ".");
                result = min - 1;
            }
            if (result < min || result > max) {
                System.out.println("Please enter a number between " + min + " and " + max + ".");
            }
        }
        return result;
    }
}

/**
 * Represents a single six-sided die.
 */
class Die {
    private int value;
    private final Random rand;

    public Die() {
        value = 0;
        rand = new Random();
    }

    /**
     * Roll the die and set its value to a random number between 1 and 6.
     */
    public void roll() {
        value = 1 + rand.nextInt(6);
    }

    /**
     * Get the current value of the die.
     */
    public int get() {
        return value;
    }
}

/**
 * Handles Yahtzee scoring logic and user category selection.
 */
class Winnings {
    private int score;
    private int choice;
    private final Scanner scanner;

    public Winnings(Scanner scanner) {
        this.score = 0;
        this.scanner = scanner;
    }

    /**
     * Prompts the user to select a scoring category, checks if the dice match, and
     * updates the score.
     * 
     * @param dice           The current dice values.
     * @param usedCategories Array tracking which categories have been used.
     */
    public void checkWinnings(int[] dice, int[] usedCategories) {
        System.out.println("Which category do you want to score?");
        String[] categories = {
                "Yahtzee", "Full House", "Large Straight", "Small Straight", "Four of a Kind", "Three of a Kind",
                "Pair", "Two Pair", "Number of 1's", "Number of 2's", "Number of 3's", "Number of 4's",
                "Number of 5's", "Number of 6's", "Chance"
        };
        for (int i = 0; i < categories.length; i++) {
            if (usedCategories[i] == 0) {
                System.out.printf("%2d - %s\n", i + 1, categories[i]);
            }
        }
        // Prompt for category
        choice = Yahtzee.inputInt(scanner, "Enter category number: ", 1, 15);

        // Count occurrences of each die face
        int[] counts = new int[6];
        for (int d : dice)
            counts[d - 1]++;
        Arrays.sort(dice);

        // Scoring logic
        switch (choice) {
            case 1: // Yahtzee
                if (hasOfAKind(counts, 5)) {
                    System.out.println("Yahtzee!");
                    score = 50;
                } else {
                    fail();
                }
                break;
            case 2: // Full House
                if (hasFullHouse(counts)) {
                    System.out.println("You have a full house.");
                    score = 25;
                } else {
                    fail();
                }
                break;
            case 3: // Large Straight
                if (isLargeStraight(dice)) {
                    System.out.println("You have a large straight.");
                    score = 40;
                } else {
                    fail();
                }
                break;
            case 4: // Small Straight
                if (isSmallStraight(dice)) {
                    System.out.println("You have a small straight.");
                    score = 30;
                } else {
                    fail();
                }
                break;
            case 5: // Four of a Kind
                if (hasOfAKind(counts, 4)) {
                    System.out.println("You have four of a kind.");
                    score = sum(dice);
                } else {
                    fail();
                }
                break;
            case 6: // Three of a Kind
                if (hasOfAKind(counts, 3)) {
                    System.out.println("You have three of a kind.");
                    score = sum(dice);
                } else {
                    fail();
                }
                break;
            case 7: // Pair
                if (countPairs(counts) >= 1) {
                    System.out.println("You have a pair.");
                    score = 5;
                } else {
                    fail();
                }
                break;
            case 8: // Two Pair
                if (countPairs(counts) >= 2) {
                    System.out.println("You have two pairs.");
                    score = 10;
                } else {
                    fail();
                }
                break;
            case 9: // Number of 1's
                System.out.println("You have " + counts[0] + " ones.");
                score = counts[0] * 1;
                break;
            case 10: // Number of 2's
                System.out.println("You have " + counts[1] + " twos.");
                score = counts[1] * 2;
                break;
            case 11: // Number of 3's
                System.out.println("You have " + counts[2] + " threes.");
                score = counts[2] * 3;
                break;
            case 12: // Number of 4's
                System.out.println("You have " + counts[3] + " fours.");
                score = counts[3] * 4;
                break;
            case 13: // Number of 5's
                System.out.println("You have " + counts[4] + " fives.");
                score = counts[4] * 5;
                break;
            case 14: // Number of 6's
                System.out.println("You have " + counts[5] + " sixes.");
                score = counts[5] * 6;
                break;
            case 15: // Chance
                score = sum(dice);
                System.out.println("You get " + score + " points.");
                break;
            default:
                fail();
        }
    }

    /**
     * Get the score for the last checked category.
     */
    public int score() {
        return score;
    }

    /**
     * Get the category number chosen by the user (1-based).
     */
    public int choice() {
        return choice;
    }

    // --- Helper methods for scoring logic ---

    private void fail() {
        System.out.println("You got nothin'.");
        score = 0;
    }

    private int sum(int[] arr) {
        int s = 0;
        for (int v : arr)
            s += v;
        return s;
    }

    private boolean hasOfAKind(int[] counts, int n) {
        for (int c : counts)
            if (c >= n)
                return true;
        return false;
    }

    private int countPairs(int[] counts) {
        int pairs = 0;
        for (int c : counts)
            if (c >= 2)
                pairs++;
        return pairs;
    }

    private boolean hasFullHouse(int[] counts) {
        boolean has3 = false, has2 = false;
        for (int c : counts) {
            if (c == 3)
                has3 = true;
            if (c == 2)
                has2 = true;
        }
        return has3 && has2;
    }

    private boolean isLargeStraight(int[] dice) {
        // Large straight: 1-2-3-4-5 or 2-3-4-5-6
        return (Arrays.equals(dice, new int[] { 1, 2, 3, 4, 5 }) || Arrays.equals(dice, new int[] { 2, 3, 4, 5, 6 }));
    }

    private boolean isSmallStraight(int[] dice) {
        // Small straight: any four consecutive numbers
        int[][] smallStraights = {
                { 1, 2, 3, 4 }, { 2, 3, 4, 5 }, { 3, 4, 5, 6 }
        };
        for (int[] straight : smallStraights) {
            int found = 0;
            for (int s : straight) {
                for (int d : dice) {
                    if (d == s) {
                        found++;
                        break;
                    }
                }
            }
            if (found == 4)
                return true;
        }
        return false;
    }
}