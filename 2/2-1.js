import fs from "fs";

export const isValidPassword = ([passwd, char, low, high]) => {
  const n = [...passwd].filter((c) => c === char).length;
  return n >= low && n <= high;
};

const numValidPassWords = fs
  .readFileSync("input.txt", "utf8")
  .split("\n")
  .filter((s) => s.trim().length > 0)
  .map((s) => {
    const [_, low, high, char, passwd] = s.match(/(\d+)-(\d+) ([a-z]): (.+)/);
    const pwspec = [passwd.trim(), char, Number(low), Number(high)];
    return pwspec;
  })
  .filter(isValidPassword).length;

console.log(`Number of valid passwords: ${numValidPassWords}`);
