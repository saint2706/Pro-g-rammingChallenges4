package Games.Easy.Yahtzee;

import java.util.*;

public class yahtzee {
    public static void main(String[] args) {
        int play = 1, score1 = 0, sum = 0;
        int[] wins = new int[15];
        while ((play == 1) && (sum < 15)) {
            sum = 0;
            int[] Dice = new int[] { 0, 0, 0, 0, 0 };
            int roll = 0;
            int x, y, w, z;
            int reroll1 = 0, reroll2 = 03;
            Die die = new Die();
            for (x = 0; x < 5; x++) {
                die.roll();
                Dice[x] = die.get();
            }

            System.out.println("Die 1: " + Dice[0]);
            System.out.println("Die 2: " + Dice[1]);
            System.out.println("Die 3: " + Dice[2]);
            System.out.println("Die 4: " + Dice[3]);
            System.out.println("Die 5: " + Dice[4]);

            do {
                reroll1 = inputInt("How many dice do you want to reroll? (0-5)");
                if (reroll1 > 0) {
                    int[] reroll = new int[reroll1];
                    for (y = 0; y < reroll1; y++) {
                        reroll2 = inputInt("Which ones?");
                        reroll[y] = reroll2;
                    }
                    for (w = 0; w < reroll1; w++) {
                        if (reroll[w] == 1) {
                            die.roll();
                            Dice[0] = die.get();
                        }
                        if (reroll[w] == 2) {
                            die.roll();
                            Dice[1] = die.get();
                        }
                        if (reroll[w] == 3) {
                            die.roll();
                            Dice[2] = die.get();
                        }
                        if (reroll[w] == 4) {
                            die.roll();
                            Dice[3] = die.get();
                        }
                        if (reroll[w] == 5) {
                            die.roll();
                            Dice[4] = die.get();
                        }
                    }
                    roll++;
                    System.out.println("Die 1: " + Dice[0]);
                    System.out.println("Die 2: " + Dice[1]);
                    System.out.println("Die 3: " + Dice[2]);
                    System.out.println("Die 4: " + Dice[3]);
                    System.out.println("Die 5: " + Dice[4]);

                }
            } while ((roll < 2) && (reroll1 > 0));
            Winnings prize = new Winnings();
            prize.checkWinnings(Dice, wins);
            wins[prize.choice() - 1] = 1;
            for (z = 0; z < 15; z++) {
                sum += wins[z];
            }
            score1 += prize.score();
            System.out.println("Your total score is: " + score1);
            if (sum < 15) {
                play = inputInt("do you want to play again?(1=yes, 2=no)");
            } else {
                System.out.println("GAME OVER!");
            }
        }
    }

    static int inputInt(String Prompt) {
        int result = 0;
        try {
            result = Integer.parseInt(input(Prompt).trim());
        } catch (Exception e) {
            result = 0;
        }
        return result;
    }

    static String input(String prompt) {
        String inputLine = "";
        System.out.print(prompt);
        try {
            java.io.InputStreamReader sys = new java.io.InputStreamReader(
                    System.in);
            java.io.BufferedReader inBuffer = new java.io.BufferedReader(sys);
            inputLine = inBuffer.readLine();
        } catch (Exception e) {
            String err = e.toString();
            System.out.println(err);
        }
        return inputLine;
    }
}

class Die {
    private int value;
    private Random rand;

    public Die() {
        value = 0;
        rand = new Random();
    }

    public void roll() {
        value = 1 + rand.nextInt(6);
    }

    public int get() {
        return (value);
    }
}

class Winnings {
    private int score;
    private int choice;

    public Winnings() {
        score = 0;
    }

    public void checkWinnings(int[] Dice, int[] wins) {
        System.out.println("Which do you want to see if you have?");
        if (wins[0] == 0) {
            System.out.println("1 - yahtzee");
        }
        if (wins[1] == 0) {
            System.out.println("2 - full house");
        }
        if (wins[2] == 0) {
            System.out.println("3 - large straigt");
        }
        if (wins[3] == 0) {
            System.out.println("4 - small straigt");
        }
        if (wins[4] == 0) {
            System.out.println("5 - four of a kind");
        }
        if (wins[5] == 0) {
            System.out.println("6 - three of a kind");
        }
        if (wins[6] == 0) {
            System.out.println("7 - pair");
        }
        if (wins[7] == 0) {
            System.out.println("8 - two pair");
        }
        if (wins[8] == 0) {
            System.out.println("9 - number of 1's");
        }
        if (wins[9] == 0) {
            System.out.println("10 - number of 2's");
        }
        if (wins[10] == 0) {
            System.out.println("11 - number of 3's");
        }
        if (wins[11] == 0) {
            System.out.println("12 - number of 4's");
        }
        if (wins[12] == 0) {
            System.out.println("13 - number of 5's");
        }
        if (wins[13] == 0) {
            System.out.println("14 - number of 6's");
        }
        if (wins[14] == 0) {
            System.out.println("15 - chance");
        }
        choice = yahtzee.inputInt("");

        int x = 0, y = 0, wins2 = 0, wins1 = 0;
        int ones = 0, twos = 0, threes = 0, fours = 0, fives = 0, sixes = 0;
        Arrays.sort(Dice);

        for (y = 0; y < 5; y++) {
            if (Dice[y] == 1) {
                ones++;
            }
            if (Dice[y] == 2) {
                twos++;
            }
            if (Dice[y] == 3) {
                threes++;
            }
            if (Dice[y] == 4) {
                fours++;
            }
            if (Dice[y] == 5) {
                fives++;
            }
            if (Dice[y] == 6) {
                sixes++;
            }
        }

        if ((Dice[0] == Dice[1] - 1) && (Dice[1] == Dice[2] - 1)
                && (Dice[2] == Dice[3] - 1) && (Dice[3] == Dice[4] - 1)
                && (choice == 3)) {
            wins1 = 1;
        } else if ((ones > 0) && (twos > 0) && (threes > 0) && (fours > 0)) {
            wins1 = 2;
        } else if ((threes > 0) && (fours > 0) && (fives > 0) && (sixes > 0)) {
            wins1 = 2;
        } else if ((twos > 0) && (threes > 0) && (fours > 0) && (fives > 0)) {
            wins1 = 2;
        }

        for (x = 0; x < 5; x++) {
            if (x != 0) {
                if ((Dice[0] == Dice[x])) {
                    wins2++;
                }
            }
            if ((x != 0) && (x != 1)) {
                if ((Dice[1] == Dice[x])) {
                    wins2++;
                }
            }
            if ((x != 0) && (x != 1) && (x != 2)) {
                if ((Dice[2] == Dice[x])) {
                    wins2++;
                }
            }
            if ((x != 0) && (x != 1) && (x != 2) && (x != 3)) {
                if ((Dice[3] == Dice[x])) {
                    wins2++;
                }
            }
        }

        if ((wins1 == 1) && (choice == 3)) {
            System.out.println("You have a straight.");
            score = 40;
        } else if ((wins1 == 2) && (choice == 4)) {
            System.out.println("You have a small straight.");
            score = 30;
        } else if ((wins2 == 10) && (choice == 1)) {
            System.out.println("Yatzee!");
            score = 50;
        } else if ((choice == 6) && (wins2 >= 3)) {
            System.out.println("You have three of a kind.");
            score = Dice[0] + Dice[1] + Dice[2] + Dice[3] + Dice[4];
        } else if ((choice == 7) && (wins2 > 0)) {
            System.out.println("You have a pair.");
            score = 5;
        } else if ((wins2 == 2) && (choice == 8)) {
            System.out.println("You have two pairs.");
            score = 10;
        } else if ((wins2 == 4) && (choice == 2)) {
            System.out.println("You have a full house.");
            score = 25;
        } else if ((wins2 >= 6) && (choice == 5)) {
            System.out.println("You have four of a kind.");
            score = Dice[0] + Dice[1] + Dice[2] + Dice[3] + Dice[4];
        } else if (choice == 9) {
            System.out.println("You have " + ones + " ones.");
            score = ones;
        } else if (choice == 10) {
            System.out.println("You have " + twos + " twos.");
            score = twos * 2;
        } else if (choice == 11) {
            System.out.println("You have " + threes + " threes.");
            score = threes * 3;
        } else if (choice == 12) {
            System.out.println("You have " + fours + " fours.");
            score = fours * 4;
        } else if (choice == 13) {
            System.out.println("You have " + fives + " fives.");
            score = fives * 5;
        } else if (choice == 14) {
            System.out.println("You have " + sixes + " sixes.");
            score = sixes * 6;
        } else if (choice == 15) {
            score = Dice[0] + Dice[1] + Dice[2] + Dice[3] + Dice[4];
            System.out.println("Your get " + score + " points.");
        } else {
            System.out.println("You got nothin'.");
            score = 0;
        }
    }

    public int score() {
        return (score);
    }

    public int choice() {
        return (choice);
    }
}