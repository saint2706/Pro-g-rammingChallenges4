/**
 * Solves the Towers of Hanoi problem for a given number of disks using recursion.
 *
 * @param {number} n The number of disks to move. Must be a positive integer.
 * @param {string} fromRod The name of the source rod.
 * @param {string} toRod The name of the destination rod.
 * @param {string} auxRod The name of the auxiliary/middle rod.
 */
const towersOfHanoi = (n, fromRod, toRod, auxRod) => {
  // Base case: if there are no disks to move, do nothing.
  if (n === 0) {
    return;
  }

  // 1. Move n-1 disks from the 'from' rod to the 'auxiliary' rod,
  //    using the 'to' rod as the temporary holder.
  towersOfHanoi(n - 1, fromRod, auxRod, toRod);

  // 2. Move the nth disk (the largest) from the 'from' rod to the 'to' rod.
  console.log(`Move disk ${n} from rod ${fromRod} to rod ${toRod}`);

  // 3. Move the n-1 disks from the 'auxiliary' rod to the 'to' rod,
  //    using the 'from' rod as the temporary holder.
  towersOfHanoi(n - 1, auxRod, toRod, fromRod);
};

/**
 * Main function to run the Towers of Hanoi solver.
 * It takes the number of disks from the command line or uses a default value.
 */
const main = () => {
  console.log("--- Towers of Hanoi Solver ---");

  // Default to 4 disks if no command-line argument is provided or if it's invalid.
  let numDisks = 4;

  // process.argv[0] is the node executable
  // process.argv[1] is the script path
  // process.argv[2] is the first actual argument
  if (process.argv.length > 2) {
    const arg = parseInt(process.argv[2], 10);
    if (!isNaN(arg) && arg > 0) {
      numDisks = arg;
    } else {
      console.log(`Invalid input '${process.argv[2]}'. It must be a positive integer. Using default of ${numDisks} disks.`);
    }
  }

  console.log(`\nSolving for ${numDisks} disks from rod A to B, using C as auxiliary:`);
  towersOfHanoi(numDisks, "A", "B", "C");
};

// Execute the solver.
main();
