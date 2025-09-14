/**
 * spinny.js – Modern spinning cube (Canvas)
 * Enhancements:
 *  - Immutable base geometry (original cube retained; transformations applied to copies)
 *  - Simple 3D rotation matrices (YX order) producing smoother, consistent motion
 *  - UI bindings: speed slider, pause toggle, autoresize, reset button, keyboard shortcuts
 *  - Respects prefers-reduced-motion (starts paused & reduces update rate)
 *  - Responsive canvas resizing with debounced handler
 *  - Clear separation of state, rendering, and animation loop
 */

class SpinningCube {
    /**
     * @param {HTMLCanvasElement} canvas
     * @param {object} [options]
     * @param {number} [options.baseSpeed=1] Base speed multiplier.
     * @param {boolean} [options.autoresize=true] Whether to auto-fit canvas to window.
     */
    constructor(canvas, { baseSpeed = 1, autoresize = true } = {}) {
        if (!(canvas instanceof HTMLCanvasElement)) throw new Error('Canvas element required');
        this.canvas = canvas;
        this.ctx = canvas.getContext('2d');
        if (!this.ctx) throw new Error('2D context not supported');

        this.baseSpeed = baseSpeed;
        this.speedFactor = 1; // live user-controlled speed
        this.isPaused = false;
        this.autoresize = autoresize;
        this.frameId = null;

        // Geometry (unit cube corners)
        this.baseNodes = [
            [-1, -1, -1], [-1, -1, 1], [-1, 1, -1], [-1, 1, 1],
            [1, -1, -1], [1, -1, 1], [1, 1, -1], [1, 1, 1],
        ];
        this.edges = [
            [0, 1], [1, 3], [3, 2], [2, 0], [4, 5], [5, 7], [7, 6], [6, 4], [0, 4], [1, 5], [2, 6], [3, 7]
        ];

        // Render scaling (dynamic based on viewport)
        this.scale = 200;

        // Rotation state (accumulated angles)
        this.angleX = Math.PI / 4;
        this.angleY = Math.atan(Math.sqrt(2));

        // Motion preferences
        this.prefersReducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
        if (this.prefersReducedMotion) this.isPaused = true;

        this.resize();
        if (this.autoresize) this._installResizeListener();
    }

    _installResizeListener() {
        let timeout = null;
        window.addEventListener('resize', () => {
            if (!this.autoresize) return;
            clearTimeout(timeout);
            timeout = setTimeout(() => this.resize(), 80);
        });
    }

    resize() {
        this.canvas.width = window.innerWidth;
        this.canvas.height = window.innerHeight;
        this.scale = Math.min(this.canvas.width, this.canvas.height) * 0.3; // keep cube visible
        this.draw();
    }

    setSpeedFactor(f) { this.speedFactor = f; }
    pause(p = true) { this.isPaused = p; if (!p) this._loop(); }
    togglePause() { this.pause(!this.isPaused); }
    reset() { this.angleX = Math.PI / 4; this.angleY = Math.atan(Math.sqrt(2)); this.draw(); }
    setAutoresize(v) { this.autoresize = v; }

    /** Compute rotated & scaled 2D projection (orthographic) */
    _transformedNodes() {
        const sinX = Math.sin(this.angleX), cosX = Math.cos(this.angleX);
        const sinY = Math.sin(this.angleY), cosY = Math.cos(this.angleY);
        const nodes2d = [];
        for (const [x0, y0, z0] of this.baseNodes) {
            // Rotate around X: (y,z)
            let y1 = y0 * cosX - z0 * sinX;
            let z1 = z0 * cosX + y0 * sinX;
            // Rotate around Y: (x,z)
            let x2 = x0 * cosY + z1 * sinY;
            let z2 = -x0 * sinY + z1 * cosY; // z2 unused (no depth sort)
            nodes2d.push([x2 * this.scale, y1 * this.scale]);
        }
        return nodes2d;
    }

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
            const p1 = pts[a], p2 = pts[b];
            ctx.moveTo(p1[0], p1[1]);
            ctx.lineTo(p2[0], p2[1]);
        }
        ctx.stroke();
        ctx.restore();
    }

    _step(dt) {
        // dt in seconds; rotate proportionally
        const speed = this.baseSpeed * this.speedFactor;
        this.angleX += dt * speed * 0.9;
        this.angleY += dt * speed * 1.2;
    }

    _loop(timestamp) {
        if (this.isPaused) { this.frameId = null; return; }
        if (this.lastTime == null) this.lastTime = timestamp;
        const dtMs = timestamp - this.lastTime;
        this.lastTime = timestamp;
        const dt = dtMs / 1000;
        // Limit update rate for reduced motion users
        if (!this.prefersReducedMotion || dtMs > 250) {
            this._step(dt);
            this.draw();
        }
        this.frameId = requestAnimationFrame(t => this._loop(t));
    }

    animate() {
        if (this.frameId == null && !this.isPaused) this.frameId = requestAnimationFrame(t => this._loop(t));
    }
}

// -------------------- UI Wiring -------------------- //

function init() {
    const canvas = document.getElementById('cubeCanvas');
    if (!canvas) throw new Error('Canvas #cubeCanvas missing');
    const cube = new SpinningCube(canvas, { baseSpeed: 1 });

    const speedInput = document.getElementById('speed');
    const pauseInput = document.getElementById('pause');
    const resetBtn = document.getElementById('reset');
    const autoresizeInput = document.getElementById('autoresize');
    const statusSpan = document.getElementById('status');

    function updateStatus() {
        statusSpan.textContent = cube.isPaused ? 'Paused' : 'Running';
    }
    updateStatus();

    speedInput?.addEventListener('input', () => {
        const factor = Number(speedInput.value) / 100; // slider 0-300 -> 0-3.0
        cube.setSpeedFactor(factor);
    });
    pauseInput?.addEventListener('change', () => { cube.pause(pauseInput.checked); updateStatus(); });
    autoresizeInput?.addEventListener('change', () => cube.setAutoresize(autoresizeInput.checked));
    resetBtn?.addEventListener('click', () => { cube.reset(); });

    document.addEventListener('keydown', (e) => {
        if (e.key === ' ') { cube.togglePause(); pauseInput.checked = cube.isPaused; updateStatus(); }
        else if (e.key === 'r' || e.key === 'R') { cube.reset(); }
        else if (e.key === 'Escape') { cube.pause(true); pauseInput.checked = true; updateStatus(); }
    });

    if (!cube.isPaused) cube.animate();
}

window.addEventListener('DOMContentLoaded', () => {
    try { init(); } catch (err) { console.error(err); alert(err.message); }
});
