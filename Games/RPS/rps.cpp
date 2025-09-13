
//
// Rock Paper Scissors (RPS) Game - C++ Implementation
// Modernized, documented, and beginner-friendly code.
//
#include <iostream>
using namespace std;

// Constants for moves
char ROCK = 'r';
char PAPER = 'p';
char SCISSORS = 's';

// Randomly select a move for the computer
char getCompMove()
{
  char compMove;
  int random = rand() % 3;
  if (random == 0)
  {
    compMove = ROCK;
  }
  else if (random == 1)
  {
    compMove = PAPER;
  }
  else
  {
    compMove = SCISSORS;
  }
  return compMove;
}

// Prompt the user for their move
char getUserMove()
{
  char userMove;
  cout << "(r)ock, (p)aper, or (s)cissors? ";
  cin >> userMove;
  return userMove;
}

// Print the result of a round
void printResult(char userMove, char compMove)
{
  if (userMove == compMove)
  {
    cout << "It's a tie!" << endl;
  }
  else if (userMove == ROCK && compMove == PAPER)
  {
    cout << "You lose!" << endl;
  }
  else if (userMove == ROCK && compMove == SCISSORS)
  {
    cout << "You win!" << endl;
  }
  else if (userMove == PAPER && compMove == ROCK)
  {
    cout << "You win!" << endl;
  }
  else if (userMove == PAPER && compMove == SCISSORS)
  {
    cout << "You lose!" << endl;
  }
  else if (userMove == SCISSORS && compMove == ROCK)
  {
    cout << "You lose!" << endl;
  }
  else if (userMove == SCISSORS && compMove == PAPER)
  {
    cout << "You win!" << endl;
  }
}

// Main game loop
int main()
{
  char userMove, compMove;
  cout << "Welcome to Rock, Paper, Scissors!" << endl;
  cout << "The computer will randomly choose a move." << endl;
  cout << "You will then choose your move." << endl;
  cout << "The winner is the first to win 3 games." << endl;
  cout << "Good luck!" << endl;
  int userScore = 0;
  int compScore = 0;
  while (userScore < 3 && compScore < 3)
  {
    compMove = getCompMove();
    userMove = getUserMove();
    printResult(userMove, compMove);
    if (userMove == ROCK && compMove == PAPER)
    {
      compScore++;
    }
    else if (userMove == ROCK && compMove == SCISSORS)
    {
      userScore++;
    }
    else if (userMove == PAPER && compMove == ROCK)
    {
      userScore++;
    }
    else if (userMove == PAPER && compMove == SCISSORS)
    {
      compScore++;
    }
    else if (userMove == SCISSORS && compMove == ROCK)
    {
      compScore++;
    }
    else if (userMove == SCISSORS && compMove == PAPER)
    {
      userScore++;
    }
  }
  // Announce the winner
  if (userScore == 3)
  {
    cout << "Congratulations! You win the match!" << endl;
  }
  else
  {
    cout << "Computer wins the match. Better luck next time!" << endl;
  }
}