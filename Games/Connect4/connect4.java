package Games.Easy.Connect4;

import java.util.Scanner;

public class connect4 {
  public static void main(String[] args) {
    try (Scanner inp = new Scanner(System.in)) {
      char[][] board = new char[6][7];

      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 7; j++) {
          board[i][j] = '.';
        }
      }

      int move = 1;
      char player = 'A';
      boolean gameOver = false;

      while (!gameOver && move <= 42) {
        boolean validMove = false;
        int play;
        do {
          display(board);
          System.out.println("Player " + player +
              ": Enter a column number (1-7): ");
          play = inp.nextInt();

          if (play < 1 || play > 7) {
            System.out.println("Invalid column number. Try again.");
          } else {
            validMove = true;
          }
        } while (!validMove);

        for (int row = 5; row >= 0; row--) {
          if (board[row][play - 1] == '.') {
            board[row][play - 1] = player;
            break;
          }
        }
        gameOver = checkWinner(player, board);

        if (player == 'A') {
          player = 'B';
        } else {
          player = 'A';
        }
        move++;
      }
      display(board);

      if (gameOver) {
        if (player == 'A') {
          System.out.println("Player B wins!");
        } else {
          System.out.println("Player A wins!");
        }
      } else {
        System.out.println("It's a tie!");
      }
    }
  }

  private static boolean checkWinner(char player, char[][] board) {
    boolean winner = false;

    for (int row = 0; row < 6; row++) {
      for (int col = 0; col < 7; col++) {
        if (board[row][col] == player) {
          if (row + 3 < 6) {
            if (board[row][col] == board[row + 1][col] &&
                board[row][col] == board[row + 2][col] &&
                board[row][col] == board[row + 3][col]) {
              winner = true;
            }
          }
          if (col + 3 < 7) {
            if (board[row][col] == board[row][col + 1] &&
                board[row][col] == board[row][col + 2] &&
                board[row][col] == board[row][col + 3]) {
              winner = true;
            }
          }
          if (row + 3 < 6 && col + 3 < 7) {
            if (board[row][col] == board[row + 1][col + 1] &&
                board[row][col] == board[row + 2][col + 2] &&
                board[row][col] == board[row + 3][col + 3]) {
              winner = true;
            }
          }
          if (row - 3 >= 0 && col + 3 < 7) {
            if (board[row][col] == board[row - 1][col + 1] &&
                board[row][col] == board[row - 2][col + 2] &&
                board[row][col] == board[row - 3][col + 3]) {
              winner = true;
            }
          }
        }
      }
    }
    return winner;
  }

  public static void display(char[][] board) {
    System.out.println(" 0 1 2 3 4 5 6");
    System.out.println("---------------");
    for (int row = 0; row < 6; row++) {
      System.out.print(row + "|");
      for (int col = 0; col < 7; col++) {
        System.out.print(board[row][col] + " ");
      }
      System.out.println();
      System.out.println("---------------");
    }
  }
}