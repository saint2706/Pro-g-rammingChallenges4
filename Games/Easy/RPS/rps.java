package Games.Easy.RPS;

import java.util.*;

class RPS {
    enum Move {
        ROCK("rock"), PAPER("paper"), SCISSORS("scissors");

        private String val;

        Move(String val) {
            this.val = val;
        }

        public String getVal() {
            return val;
        }
    }

    public static void main(String[] args) {
        try (Scanner inp = new Scanner(System.in)) {
            int wins = 0;
            int losses = 0;

            System.out.println("Please enter \"rock\", \"paper\", \"scissors\", or \"quit\" to exit.");
            while (true) {
                System.out.println("Enter your move:");
                String userMove = inp.nextLine();

                if (userMove.equals("quit")) {
                    System.out.println("You won " + wins + " times.");
                    System.out.println("You lost " + losses + " times.");
                    break;
                }

                if (Arrays.stream(Move.values()).noneMatch(move -> move.getVal().equals(userMove))) {
                    System.out
                            .println(
                                    "Invalid move. Please enter \"rock\", \"paper\", \"scissors\", or \"quit\" to exit.");
                    continue;
                }

                String compMove = getCompMove();

                if (userMove.equals(compMove)) {
                    System.out.println("It's a tie!");
                } else if (userMove.equals("rock") && compMove.equals("scissors")
                        || userMove.equals("paper") && compMove.equals("rock")
                        || userMove.equals("scissors") && compMove.equals("paper")) {
                    System.out.println("You win!");
                    wins++;
                } else {
                    System.out.println("You lose!");
                    losses++;
                }

            }
        }
    }

    private static String getCompMove() {
        Random rand = new Random();
        int randInt = rand.nextInt(3);
        return Move.values()[randInt].getVal();
    }
}