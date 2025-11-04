/**
 * spinny.js – Modern spinning cube (Canvas)
 * This script provides an interactive 3D spinning cube rendered on an HTML5 canvas.
 * It includes controls for speed, pausing, and resizing, and respects the user's
 * preference for reduced motion.
 */

class SpinningCube {
    /**
     * @param {HTMLCanvasElement} canvas The canvas element to render on.
     * @param {object} [options]
     * @param {number} [options.baseSpeed=1] The base speed multiplier for rotation.
     * @param {boolean} [options.autoresize=true] Whether to automatically resize the canvas.
     */
    constructor(canvas, { baseSpeed = 1, autoresize = true } = {}) {
        if (!(canvas instanceof HTMLCanvasElement)) throw new Error('A valid canvas element is required.');
        this.canvas = canvas;
        this.ctx = canvas.getContext('2d');
        if (!this.ctx) throw new Error('2D rendering context is not supported.');

        this.baseSpeed = baseSpeed;
        this.speedFactor = 1;
        this.isPaused = false;
        this.autoresize = autoresize;
        this.frameId = null;

        // The 8 vertices of a unit cube.
        this.baseNodes = [
            [-1, -1, -1], [-1, -1, 1], [-1, 1, -1], [-1, 1, 1],
            [1, -1, -1], [1, -1, 1], [1, 1, -1], [1, 1, 1],
        ];
        // The 12 edges connecting the vertices.
        this.edges = [
            [0, 1], [1, 3], [3, 2], [2, 0], [4, 5], [5, 7], [7, 6], [6, 4],
            [0, 4], [1, 5], [2, 6], [3, 7]
        ];

        this.scale = 200;
        this.angleX = Math.PI / 4;
        this.angleY = Math.atan(Math.sqrt(2));

        this.prefersReducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
        if (this.prefersReducedMotion) this.isPaused = true;

        this.resize();
        if (this.autoresize) this._installResizeListener();
    }

    _installResizeListener() {
        let timeout;
        window.addEventListener('resize', () => {
            if (!this.autoresize) return;
            clearTimeout(timeout);
            timeout = setTimeout(() => this.resize(), 80);
        });
    }

    resize() {
        this.canvas.width = window.innerWidth;
        this.canvas.height = window.innerHeight;
        this.scale = Math.min(this.canvas.width, this.canvas.height) * 0.3;
        this.draw();
    }

    setSpeedFactor(f) { this.speedFactor = f; }
    pause(p = true) { this.isPaused = p; if (!p) this._loop(); }
    togglePause() { this.pause(!this.isPaused); }
    reset() { this.angleX = Math.PI / 4; this.angleY = Math.atan(Math.sqrt(2)); this.draw(); }
    setAutoresize(v) { this.autoresize = v; }

    /** Computes the 2D projection of the rotated 3D cube. */
    _transformedNodes() {
        const sinX = Math.sin(this.angleX), cosX = Math.cos(this.angleX);
        const sinY = Math.sin(this.angleY), cosY = Math.cos(this.angleY);
        const nodes2d = [];
        for (const [x0, y0, z0] of this.baseNodes) {
            // Rotate around the X-axis, then the Y-axis.
            let y1 = y0 * cosX - z0 * sinX;
            let z1 = z0 * cosX + y0 * sinX;
            let x2 = x0 * cosY + z1 * sinY;
            nodes2d.push([x2 * this.scale, y1 * this.scale]);
        }
        return nodes2d;
    }

    /** Draws the cube on the canvas. */
    draw() {
        const ctx = this.ctx;
        ctx.save();
        ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
        ctx.translate(this.canvas.width / 2, this.canvas.height / 2);
        ctx.strokeStyle = '#fff';
        ctx.lineWidth = 2;
        const pts = this._transformedNodes();
        ctx.beginPath();
        for (const [a, b] of this.edges) {
            ctx.moveTo(pts[a][0], pts[a][1]);
            ctx.lineTo(pts[b][0], pts[b][1]);
        }
        ctx.stroke();
        ctx.restore();
    }

    _step(dt) {
        const speed = this.baseSpeed * this.speedFactor;
        this.angleX += dt * speed * 0.9;
        this.angleY += dt * speed * 1.2;
    }

    _loop(timestamp) {
        if (this.isPaused) {
            this.frameId = null;
            return;
        }
        const dt = (timestamp - (this.lastTime || timestamp)) / 1000;
        this.lastTime = timestamp;

        if (!this.prefersReducedMotion || dt > 0.25) {
            this._step(dt);
            this.draw();
        }
        this.frameId = requestAnimationFrame(t => this._loop(t));
    }

    animate() {
        if (this.frameId === null && !this.isPaused) {
            this.frameId = requestAnimationFrame(t => this._loop(t));
        }
    }
}

// --- UI Wiring ---

function init() {
    const canvas = document.getElementById('cubeCanvas');
    if (!canvas) throw new Error('Canvas with ID #cubeCanvas not found.');
    const cube = new SpinningCube(canvas);

    const speedInput = document.getElementById('speed');
    const pauseInput = document.getElementById('pause');
    const resetBtn = document.getElementById('reset');
    const autoresizeInput = document.getElementById('autoresize');
    const statusSpan = document.getElementById('status');

    function updateStatus() {
        statusSpan.textContent = cube.isPaused ? 'Paused' : 'Running';
    }
    updateStatus();

    speedInput?.addEventListener('input', () => cube.setSpeedFactor(Number(speedInput.value) / 100));
    pauseInput?.addEventListener('change', () => {
        cube.pause(pauseInput.checked);
        updateStatus();
    });
    autoresizeInput?.addEventListener('change', () => cube.setAutoresize(autoresizeInput.checked));
    resetBtn?.addEventListener('click', () => cube.reset());

    document.addEventListener('keydown', (e) => {
        if (e.key === ' ') {
            cube.togglePause();
            pauseInput.checked = cube.isPaused;
            updateStatus();
        } else if (e.key.toLowerCase() === 'r') {
            cube.reset();
        } else if (e.key === 'Escape') {
            cube.pause(true);
            pauseInput.checked = true;
            updateStatus();
        }
    });

    if (!cube.isPaused) cube.animate();
}

window.addEventListener('DOMContentLoaded', () => {
    try {
        init();
    } catch (err) {
        console.error(err);
        alert(err.message);
    }
});
