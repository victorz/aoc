const fs = require("fs");

const input = fs
  .readFileSync("input", "utf8")
  .split("\n")
  .filter((s) => s !== "")
  .map(Number);

const target = 2020;
const products = new Map();

input.forEach((i) => {
  input.forEach((j) => {
    products.set(i + j, [i, j]);
  });
});

const e = input.find((e) => products.has(target - e));
const [i, j] = products.get(target - e);
console.log(`${e} x ${i} x ${j} = ${e * i * j}`);
