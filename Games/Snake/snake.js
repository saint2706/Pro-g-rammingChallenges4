/**
 * Represents the Snake object, managing its position, movement, and body segments.
 */
class Snake {
    constructor(gridSize) {
        this.gridSize = gridSize;
        this.reset();
    }

    reset() {
        this.x = 160;
        this.y = 160;
        this.dx = this.gridSize; // Start moving right
        this.dy = 0;
        this.cells = [];
        this.maxCells = 4;
    }

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

    draw(context) {
        context.fillStyle = 'green';
        this.cells.forEach(cell => {
            context.fillRect(cell.x, cell.y, this.gridSize - 1, this.gridSize - 1);
        });
    }

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
    constructor(gridSize, canvasWidth, canvasHeight) {
        this.gridSize = gridSize;
        this.canvasWidth = canvasWidth;
        this.canvasHeight = canvasHeight;
        this.x = 0;
        this.y = 0;
        this.place();
    }

    place() {
        const maxX = this.canvasWidth / this.gridSize;
        const maxY = this.canvasHeight / this.gridSize;
        this.x = Math.floor(Math.random() * maxX) * this.gridSize;
        this.y = Math.floor(Math.random() * maxY) * this.gridSize;
    }

    draw(context) {
        context.fillStyle = 'red';
        context.fillRect(this.x, this.y, this.gridSize - 1, this.gridSize - 1);
    }
}

/**
 * Main Game class to orchestrate the game loop, state, and rendering.
 */
class Game {
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
        this.highScore = localStorage.getItem('snakeHighScore') || 0;
        this.updateScoresUI();

        this.lastUpdateTime = 0;
        this.updateInterval = 100; // milliseconds between updates

        document.addEventListener('keydown', (e) => {
            if (e.key === 'p') {
                this.togglePause();
            } else {
                this.snake.handleInput(e);
            }
        });
    }

    togglePause() {
        this.paused = !this.paused;
        if (!this.paused) {
            requestAnimationFrame((timestamp) => this.loop(timestamp));
        }
    }

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

    update() {
        this.snake.update();
        this.checkCollisions();
    }

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

    updateScoresUI() {
        this.scoreElem.textContent = this.score;
        this.highScoreElem.textContent = this.highScore;
    }

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
