
/**
 * Classic Snake Game (JavaScript Implementation)
 * ---------------------------------------------
 * Modern, well-documented, and beginner-friendly implementation of the classic Snake game for the web.
 * Features:
 * - Modular class-based design
 * - Responsive controls (WASD/Arrow keys, pause)
 * - Local storage for high score
 * - Clear comments and docstrings
 * - Optimized for readability and maintainability
 */

/**
 * Represents the Snake object, managing its position, movement, and body segments.
 */
class Snake {
    /**
     * @param {number} gridSize - The size of each grid cell in pixels.
     */
    constructor(gridSize) {
        this.gridSize = gridSize;
        this.reset();
    }

    /**
     * Resets the snake to its initial state.
     */
    reset() {
        this.x = 160;
        this.y = 160;
        this.dx = this.gridSize; // Start moving right
        this.dy = 0;
        this.cells = [];
        this.maxCells = 4;
    }

    /**
     * Updates the snake's position and body segments.
     */
    update() {
        this.x += this.dx;
        this.y += this.dy;

        // Wrap snake position horizontally on edge of screen
        if (this.x < 0) {
            this.x = this.canvasWidth - this.gridSize;
        } else if (this.x >= this.canvasWidth) {
            this.x = 0;
        }

        // Wrap snake position vertically on edge of screen
        if (this.y < 0) {
            this.y = this.canvasHeight - this.gridSize;
        } else if (this.y >= this.canvasHeight) {
            this.y = 0;
        }

        // Add new head to the beginning of the cells array
        this.cells.unshift({ x: this.x, y: this.y });

        // Remove cells as snake moves
        if (this.cells.length > this.maxCells) {
            this.cells.pop();
        }
    }

    /**
     * Draws the snake on the canvas.
     * @param {CanvasRenderingContext2D} context
     */
    draw(context) {
        context.fillStyle = 'green';
        this.cells.forEach(cell => {
            context.fillRect(cell.x, cell.y, this.gridSize - 1, this.gridSize - 1);
        });
    }

    /**
     * Handles keyboard input to change the snake's direction.
     * @param {KeyboardEvent} e
     */
    handleInput(e) {
        if ((e.key === 'a' || e.key === 'ArrowLeft') && this.dx === 0) {
            this.dx = -this.gridSize;
            this.dy = 0;
        } else if ((e.key === 'w' || e.key === 'ArrowUp') && this.dy === 0) {
            this.dy = -this.gridSize;
            this.dx = 0;
        } else if ((e.key === 'd' || e.key === 'ArrowRight') && this.dx === 0) {
            this.dx = this.gridSize;
            this.dy = 0;
        } else if ((e.key === 's' || e.key === 'ArrowDown') && this.dy === 0) {
            this.dy = this.gridSize;
            this.dx = 0;
        }
    }
}


/**
 * Represents the Apple object, managing its position.
 */
class Apple {
    /**
     * @param {number} gridSize
     * @param {number} canvasWidth
     * @param {number} canvasHeight
     */
    constructor(gridSize, canvasWidth, canvasHeight) {
        this.gridSize = gridSize;
        this.canvasWidth = canvasWidth;
        this.canvasHeight = canvasHeight;
        this.x = 0;
        this.y = 0;
        this.place();
    }

    /**
     * Places the apple at a random position on the grid.
     */
    place() {
        const maxX = this.canvasWidth / this.gridSize;
        const maxY = this.canvasHeight / this.gridSize;
        this.x = Math.floor(Math.random() * maxX) * this.gridSize;
        this.y = Math.floor(Math.random() * maxY) * this.gridSize;
    }

    /**
     * Draws the apple on the canvas.
     * @param {CanvasRenderingContext2D} context
     */
    draw(context) {
        context.fillStyle = 'red';
        context.fillRect(this.x, this.y, this.gridSize - 1, this.gridSize - 1);
    }
}


/**
 * Main Game class to orchestrate the game loop, state, and rendering.
 */
class Game {
    /**
     * @param {string} canvasId - The id of the canvas element.
     */
    constructor(canvasId) {
        this.canvas = document.getElementById(canvasId);
        this.context = this.canvas.getContext('2d');
        this.gridSize = 16;
        this.paused = false;

        this.scoreElem = document.getElementById('current-score');
        this.highScoreElem = document.getElementById('high-score');

        this.snake = new Snake(this.gridSize);
        this.snake.canvasWidth = this.canvas.width;
        this.snake.canvasHeight = this.canvas.height;

        this.apple = new Apple(this.gridSize, this.canvas.width, this.canvas.height);

        this.score = 0;
        this.highScore = Number(localStorage.getItem('snakeHighScore')) || 0;
        this.updateScoresUI();

        this.lastUpdateTime = 0;
        this.updateInterval = 100; // milliseconds between updates

        // Use arrow function to preserve 'this' context
        document.addEventListener('keydown', (e) => {
            if (e.key === 'p' || e.key === 'P') {
                this.togglePause();
            } else {
                this.snake.handleInput(e);
            }
        });
    }

    /**
     * Toggles the pause state of the game.
     */
    togglePause() {
        this.paused = !this.paused;
        if (!this.paused) {
            requestAnimationFrame((timestamp) => this.loop(timestamp));
        }
    }

    /**
     * The main game loop, called via requestAnimationFrame.
     * @param {DOMHighResTimeStamp} timestamp
     */
    loop(timestamp) {
        if (this.paused) return;

        requestAnimationFrame((ts) => this.loop(ts));

        const elapsed = timestamp - this.lastUpdateTime;
        if (elapsed > this.updateInterval) {
            this.lastUpdateTime = timestamp;
            this.update();
            this.draw();
        }
    }

    /**
     * Updates the game state (snake, collisions, etc).
     */
    update() {
        this.snake.update();
        this.checkCollisions();
    }

    /**
     * Checks for collisions (apple, self) and handles scoring and reset.
     */
    checkCollisions() {
        // Check collision with apple
        if (this.snake.cells[0].x === this.apple.x && this.snake.cells[0].y === this.apple.y) {
            this.snake.maxCells++;
            this.score++;
            this.apple.place();
            this.updateScoresUI();
        }

        // Check collision with self
        for (let i = 1; i < this.snake.cells.length; i++) {
            if (this.snake.cells[0].x === this.snake.cells[i].x && this.snake.cells[0].y === this.snake.cells[i].y) {
                this.resetGame();
            }
        }
    }

    /**
     * Resets the game state and updates high score if needed.
     */
    resetGame() {
        if (this.score > this.highScore) {
            this.highScore = this.score;
            localStorage.setItem('snakeHighScore', this.highScore);
        }
        this.score = 0;
        this.snake.reset();
        this.apple.place();
        this.updateScoresUI();
    }

    /**
     * Updates the score display in the UI.
     */
    updateScoresUI() {
        this.scoreElem.textContent = this.score;
        this.highScoreElem.textContent = this.highScore;
    }

    /**
     * Draws the game (snake and apple) on the canvas.
     */
    draw() {
        this.context.clearRect(0, 0, this.canvas.width, this.canvas.height);
        this.snake.draw(this.context);
        this.apple.draw(this.context);
    }
}


// --- Main Execution ---
window.onload = () => {
    const game = new Game('game-canvas');
    requestAnimationFrame((timestamp) => game.loop(timestamp));
};
