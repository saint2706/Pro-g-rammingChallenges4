class RPSGame {
    constructor() {
        // --- DOM Elements ---
        this.optionButtons = document.querySelectorAll(".options");
        this.playerScoreElem = document.getElementById("p-score");
        this.computerScoreElem = document.getElementById("c-score");
        this.playerMoveElem = document.getElementById("p-move");
        this.computerMoveElem = document.getElementById("c-move");
        this.resultTextElem = document.getElementById("result-text");

        // --- Game State ---
        this.playerScore = 0;
        this.computerScore = 0;
        this.gameOptions = ["Rock", "Paper", "Scissors"];
        this.winningConditions = {
            Rock: "Scissors",
            Paper: "Rock",
            Scissors: "Paper",
        };

        this.initEventListeners();
    }

    /**
     * Sets up the initial event listeners for the game buttons.
     */
    initEventListeners() {
        this.optionButtons.forEach(button => {
            button.addEventListener("click", () => this.playRound(button.value));
        });
    }

    /**
     * Executes a single round of the game.
     * @param {string} playerMove The move selected by the player.
     */
    playRound(playerMove) {
        if (this.playerScore === 5 || this.computerScore === 5) {
            return; // Game is over, do not play another round until reset.
        }

        const computerMove = this.gameOptions[Math.floor(Math.random() * 3)];
        this.updateMovesUI(playerMove, computerMove);

        const winner = this.determineWinner(playerMove, computerMove);
        this.updateScore(winner);
        this.updateResultText(winner, playerMove, computerMove);

        this.checkGameOver();
    }

    /**
     * Determines the winner of the round.
     * @param {string} player The player's move.
     * @param {string} computer The computer's move.
     * @returns {'Player' | 'Computer' | 'Tie'} The winner of the round.
     */
    determineWinner(player, computer) {
        if (player === computer) {
            return 'Tie';
        }
        if (this.winningConditions[player] === computer) {
            return 'Player';
        }
        return 'Computer';
    }

    /**
     * Updates the score based on the winner of the round.
     * @param {string} winner The winner of the round.
     */
    updateScore(winner) {
        if (winner === 'Player') {
            this.playerScore++;
        } else if (winner === 'Computer') {
            this.computerScore++;
        }
        this.playerScoreElem.textContent = this.playerScore;
        this.computerScoreElem.textContent = this.computerScore;
    }

    /**
     * Updates the UI to show the moves played.
     * @param {string} playerMove
     * @param {string} computerMove
     */
    updateMovesUI(playerMove, computerMove) {
        this.playerMoveElem.src = `./assets/${playerMove}.svg`;
        this.computerMoveElem.src = `./assets/${computerMove}.svg`;
    }

    /**
     * Updates the result text element with the outcome of the round.
     */
    updateResultText(winner, playerMove, computerMove) {
        if (winner === 'Tie') {
            this.resultTextElem.textContent = `${playerMove} vs ${computerMove} - It's a Tie!`;
        } else if (winner === 'Player') {
            this.resultTextElem.textContent = `${playerMove} beats ${computerMove} - You Win the Round!`;
        } else {
            this.resultTextElem.textContent = `${computerMove} beats ${playerMove} - Computer Wins the Round.`;
        }
    }

    /**
     * Checks if a player has reached the winning score and ends the game if so.
     */
    checkGameOver() {
        if (this.playerScore === 5 || this.computerScore === 5) {
            const winnerMessage = this.playerScore === 5
                ? "You win the game! Congratulations!"
                : "Computer wins the game! Try again next time!";

            this.resultTextElem.textContent = winnerMessage;

            // Add a button to reset the game
            const resetButton = document.createElement('button');
            resetButton.textContent = 'Play Again';
            resetButton.onclick = () => this.resetGame();
            this.resultTextElem.appendChild(document.createElement('br'));
            this.resultTextElem.appendChild(resetButton);
        }
    }

    /**
     * Resets the game state to its initial values.
     */
    resetGame() {
        this.playerScore = 0;
        this.computerScore = 0;
        this.playerScoreElem.textContent = this.playerScore;
        this.computerScoreElem.textContent = this.computerScore;
        this.resultTextElem.innerHTML = "Choose your move to start!";
        this.playerMoveElem.src = './assets/Rock.svg';
        this.computerMoveElem.src = './assets/Rock.svg';
    }
}

// --- Main Execution ---
document.addEventListener("DOMContentLoaded", () => {
    new RPSGame();
});
