/**
 * A class to render and animate a 3D spinning cube on an HTML5 canvas.
 */
class SpinningCube {
    /**
     * @param {string} canvasSelector A CSS selector to find the canvas element.
     */
    constructor(canvasSelector) {
        this.canvas = document.querySelector(canvasSelector);
        if (!this.canvas) {
            throw new Error("Canvas element not found");
        }
        this.ctx = this.canvas.getContext("2d");

        this.canvas.width = window.innerWidth;
        this.canvas.height = window.innerHeight;

        // The 8 vertices (nodes) of a unit cube.
        this.nodes = [
            [-1, -1, -1], [-1, -1, 1], [-1, 1, -1], [-1, 1, 1],
            [1, -1, -1], [1, -1, 1], [1, 1, -1], [1, 1, 1],
        ];

        // The 12 edges connecting the vertices.
        this.edges = [
            [0, 1], [1, 3], [3, 2], [2, 0], [4, 5], [5, 7],
            [7, 6], [6, 4], [0, 4], [1, 5], [2, 6], [3, 7],
        ];

        // Set initial scale and rotation
        this.scale(200, 200, 200);
        this.rotate(Math.PI / 4, Math.atan(Math.sqrt(2)));
    }

    /**
     * Scales the cube's nodes by a given factor.
     * @param {number} factorX
     * @param {number} factorY
     * @param {number} factorZ
     */
    scale(factorX, factorY, factorZ) {
        this.nodes.forEach(node => {
            node[0] *= factorX;
            node[1] *= factorY;
            node[2] *= factorZ;
        });
    }

    /**
     * Rotates the cube's nodes around the X and Y axes.
     * @param {number} angleX The rotation angle around the X-axis (in radians).
     * @param {number} angleY The rotation angle around the Y-axis (in radians).
     */
    rotate(angleX, angleY) {
        const sinX = Math.sin(angleX);
        const cosX = Math.cos(angleX);
        const sinY = Math.sin(angleY);
        const cosY = Math.cos(angleY);

        this.nodes.forEach(node => {
            let [x, y, z] = node;

            // Rotate around the X-axis
            node[0] = x * cosX - z * sinX;
            node[2] = z * cosX + x * sinX;

            // Use the new z-coordinate for the Y-axis rotation
            z = node[2];

            // Rotate around the Y-axis
            node[1] = y * cosY - z * sinY;
            node[2] = z * cosY + y * sinY;
        });
    }

    /**
     * Draws the current state of the cube to the canvas.
     */
    draw() {
        this.ctx.save();
        this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
        this.ctx.translate(this.canvas.width / 2, this.canvas.height / 2);
        this.ctx.strokeStyle = "#FFFFFF";
        this.ctx.beginPath();

        this.edges.forEach(edge => {
            const p1 = this.nodes[edge[0]];
            const p2 = this.nodes[edge[1]];
            this.ctx.moveTo(p1[0], p1[1]);
            this.ctx.lineTo(p2[0], p2[1]);
        });

        this.ctx.closePath();
        this.ctx.stroke();
        this.ctx.restore();
    }

    /**
     * Starts the animation loop.
     */
    animate() {
        // Continuously rotate and draw the cube
        this.rotate(Math.PI / 180, Math.PI / 360);
        this.draw();

        // Use requestAnimationFrame for smooth, efficient animation
        requestAnimationFrame(() => this.animate());
    }
}

// --- Main Execution ---
document.addEventListener("DOMContentLoaded", () => {
    try {
        const cube = new SpinningCube("canvas");
        cube.animate();
    } catch (error) {
        console.error(error);
        document.body.innerHTML = `<p style="color: red;">${error.message}</p>`;
    }
});
