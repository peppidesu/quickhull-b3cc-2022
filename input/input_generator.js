// Usage: node input_generator.js n m k s > file.dat
// to generate an n x m board with k random, distinct points
// s is either c (circle) or r (rectangular), denoting in which shape
// the points must be taken

const args = process.argv.slice(2);
if (args.length !== 4) throw new Error("Invalid arguments");
const n = parseInt(args[0], 10);
const m = parseInt(args[1], 10);
const k = parseInt(args[2], 10);
const circle = args[3] === "c";

const generated = new Set();

for (let i = 0; i < k;) {
  let x = Math.floor(Math.random() * n);
  let y = Math.floor(Math.random() * m);
  if (circle) {
    while (square(x / n - 1 / 2) + square(y / m - 1 / 2) > square(1 / 2)) {
      x = Math.floor(Math.random() * n);
      y = Math.floor(Math.random() * m);
    }
  }
  const str = x + " " + y;
  if (!generated.has(str)) {
    generated.add(str);
    console.log(str);
    i++;
  }
}

function square(x) { return x * x; }

