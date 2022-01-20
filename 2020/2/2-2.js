import fs from "fs";

export const isValidPassword = ([passwd, char, low, high]) => {
  const lowIsChar = passwd[low - 1] === char;
  const highIsChar = passwd[high - 1] === char;
  return (lowIsChar || highIsChar) && lowIsChar !== highIsChar;
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
