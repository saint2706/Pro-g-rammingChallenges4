var canvas = document.getElementById("game");
var context = canvas.getContext("2d");
var grid = 16;
var count = 0;
var score = 0;
if (localStorage.score) {
	document.getElementById("score").innerHTML = localStorage.score;
}

var max = 0;

var snake = {
	x: 160,
	y: 160,

	dx: grid,
	dy: 0,

	cells: [],

	maxCells: 4,
};
var apple = {
	x: 320,
	y: 320,
};

function getRandomInt(min, max) {
	return Math.floor(Math.random() * (max - min)) + min;
}

function loop() {
	requestAnimationFrame(loop);
	if (++count < 10) {
		return;
	}
	count = 0;
	context.clearRect(0, 0, canvas.width, canvas.height);
	snake.x += snake.dx;
	snake.y += snake.dy;
	if (snake.x < 0) {
		snake.x = canvas.width - grid;
	} else if (snake.x >= canvas.width) {
		snake.x = 0;
	}

	if (snake.y < 0) {
		snake.y = canvas.height - grid;
	} else if (snake.y >= canvas.height) {
		snake.y = 0;
	}
	snake.cells.unshift({ x: snake.x, y: snake.y });
	if (snake.cells.length > snake.maxCells) {
		snake.cells.pop();
	}
	context.fillStyle = "red";
	context.fillRect(apple.x, apple.y, grid - 1, grid - 1);
	context.fillStyle = "green";
	snake.cells.forEach(function (cell, index) {
		context.fillRect(cell.x, cell.y, grid - 1, grid - 1);
		if (cell.x === apple.x && cell.y === apple.y) {
			snake.maxCells++;
			score += 1;
			localStorage.setItem("score", score);
			document.getElementById("score").innerHTML = score;

			apple.x = getRandomInt(0, 25) * grid;
			apple.y = getRandomInt(0, 25) * grid;
		}
		for (var i = index + 1; i < snake.cells.length; i++) {
			if (cell.x === snake.cells[i].x && cell.y === snake.cells[i].y) {
				if (score > max) {
					max = score;
				}
				snake.x = 160;
				snake.y = 160;
				snake.cells = [];
				snake.maxCells = 4;
				snake.dx = grid;
				snake.dy = 0;
				apple.x = getRandomInt(0, 25) * grid;
				apple.y = getRandomInt(0, 25) * grid;
				document.getElementById("high").innerHTML = max;
			}
		}
	});
}
document.addEventListener("keydown", function (e) {
	if (e.key === "a" && snake.dx === 0) {
		snake.dx = -grid;
		snake.dy = 0;
	} else if (e.key === "w" && snake.dy === 0) {
		snake.dy = -grid;
		snake.dx = 0;
	} else if (e.key === "d" && snake.dx === 0) {
		snake.dx = grid;
		snake.dy = 0;
	} else if (e.key === "s" && snake.dy === 0) {
		snake.dy = grid;
		snake.dx = 0;
	}
});
requestAnimationFrame(loop);

function myFunction() {
	document.getElementById("end_msg").innerHTML = "";
	alert("Press confirm to continue");
}

$(document).ready(function () {
	$("#btn_stop").click(function () {
		document.getElementById("end_msg").innerHTML = "Game stoped by player";
		setTimeout(myFunction, 1000);
	});
});
