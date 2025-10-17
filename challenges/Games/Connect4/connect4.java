import java.util.Scanner;

/**
 * Console-based Connect 4 game for two players. Modernized, well-documented, and beginner-friendly
 * implementation. Board is 6 rows x 7 columns. Players alternate dropping pieces into columns.
 */
public class Connect4 {
  // Board dimensions
  private static final int ROWS = 6;
  private static final int COLS = 7;
  private static final char EMPTY = '.';
  private static final char PLAYER_A = 'A';
  private static final char PLAYER_B = 'B';

  /** Main entry point for the Connect 4 game. Handles game loop, input, and win/tie detection. */
  public static void main(String[] args) {
    try (Scanner inp = new Scanner(System.in)) {
      char[][] board = new char[ROWS][COLS];
      // Initialize board to empty
      for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
          board[i][j] = EMPTY;
        }
      }

      int move = 1;
      char player = PLAYER_A;
      boolean gameOver = false;

      while (!gameOver && move <= ROWS * COLS) {
        boolean validMove = false;
        int play = -1;
        // Input loop for valid column
        do {
          display(board);
          System.out.print("Player " + player + ": Enter a column number (1-7): ");
          if (inp.hasNextInt()) {
            play = inp.nextInt();
            if (play < 1 || play > COLS) {
              System.out.println("Invalid column number. Try again.");
            } else if (board[0][play - 1] != EMPTY) {
              System.out.println("Column is full. Try another.");
            } else {
              validMove = true;
            }
          } else {
            System.out.println("Please enter a number.");
            inp.next(); // consume invalid input
          }
        } while (!validMove);

        // Drop piece in the selected column
        for (int row = ROWS - 1; row >= 0; row--) {
          if (board[row][play - 1] == EMPTY) {
            board[row][play - 1] = player;
            break;
          }
        }

        gameOver = checkWinner(player, board);

        // Switch player
        player = (player == PLAYER_A) ? PLAYER_B : PLAYER_A;
        move++;
      }
      display(board);

      // Announce result
      if (gameOver) {
        // The player variable has already switched, so the previous player won
        char winner = (player == PLAYER_A) ? PLAYER_B : PLAYER_A;
        System.out.println("Player " + winner + " wins!");
      } else {
        System.out.println("It's a tie!");
      }
    }
  }

  /**
   * Checks if the given player has a winning sequence on the board.
   *
   * @param player The player character ('A' or 'B')
   * @param board The game board
   * @return true if the player has won, false otherwise
   */
  private static boolean checkWinner(char player, char[][] board) {
    // Check all directions for a connect-4
    for (int row = 0; row < ROWS; row++) {
      for (int col = 0; col < COLS; col++) {
        if (board[row][col] == player) {
          // Vertical
          if (row + 3 < ROWS
              && board[row + 1][col] == player
              && board[row + 2][col] == player
              && board[row + 3][col] == player) {
            return true;
          }
          // Horizontal
          if (col + 3 < COLS
              && board[row][col + 1] == player
              && board[row][col + 2] == player
              && board[row][col + 3] == player) {
            return true;
          }
          // Diagonal down-right
          if (row + 3 < ROWS
              && col + 3 < COLS
              && board[row + 1][col + 1] == player
              && board[row + 2][col + 2] == player
              && board[row + 3][col + 3] == player) {
            return true;
          }
          // Diagonal up-right
          if (row - 3 >= 0
              && col + 3 < COLS
              && board[row - 1][col + 1] == player
              && board[row - 2][col + 2] == player
              && board[row - 3][col + 3] == player) {
            return true;
          }
        }
      }
    }
    return false;
  }

  /**
   * Prints the current state of the board to the console.
   *
   * @param board The game board
   */
  public static void display(char[][] board) {
    System.out.println(" 1 2 3 4 5 6 7");
    System.out.println("---------------------");
    for (int row = 0; row < ROWS; row++) {
      System.out.print((row + 1) + "|");
      for (int col = 0; col < COLS; col++) {
        System.out.print(board[row][col] + " ");
      }
      System.out.println();
      System.out.println("---------------------");
    }
  }
}
