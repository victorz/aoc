const fs = require("fs");

const input = fs
  .readFileSync("input", "utf8")
  .split("\n")
  .filter((s) => s !== "")
  .map(Number);

const set = new Set(input);
const target = 2020;
const found = input.find((i) => set.has(target - i));

if (found !== undefined) {
  console.log(`${found} x ${target - found} = ${found * (target - found)}`);
}
