import { linesOf, sum, getInput } from "./Util.ts"

type Char = string
type Score = number

const ILLEGLAL_CHAR_SCORE: Map<Char, Score> = new Map([
  [")", 3],
  ["]", 57],
  ["}", 1197],
  [">", 25137],
])

const CHUNK_PAIRS: Map<Char, Char> = new Map([
  ["(", ")"],
  ["[", "]"],
  ["{", "}"],
  ["<", ">"],
])

function errorScore(char: Char): number {
  return ILLEGLAL_CHAR_SCORE.get(char) ?? 0
}

const isPair = (open: Char = "", close: Char = ""): boolean =>
  CHUNK_PAIRS.has(open) && close === CHUNK_PAIRS.get(open)
const isOpener = (open: Char): boolean => CHUNK_PAIRS.has(open)

const lineErrorScore = (line: string): number => {
  const stack: Char[] = []
  let errorChar: Char = ""

  for (const ch of line) {
    if (isOpener(ch)) {
      stack.push(ch)
    } else if (isPair(stack.at(-1), ch)) {
      stack.pop()
    } else {
      errorChar = ch
      break
    }
  }

  return errorScore(errorChar)
}

function part1(input: string) {
  console.log(sum(linesOf(input).map(lineErrorScore)))
}

function autoComplete(line: string): string[] {
  const stack: Char[] = []

  for (const ch of line) {
    if (isOpener(ch)) {
      stack.push(ch)
    } else if (isPair(stack.at(-1), ch)) {
      stack.pop()
    }
  }

  return stack
}

const COMPLETE_SCORE_MAP: Map<Char, number> = new Map([
  ["(", 1],
  ["[", 2],
  ["{", 3],
  ["<", 4],
])

function autoCompleteScore(stack: string[]): number {
  return stack
    .map(ch => COMPLETE_SCORE_MAP.get(ch) ?? 0)
    .reduceRight((acc, score) => acc * 5 + score, 0)
}

function part2(input: string) {
  const scores = linesOf(input)
    .filter(l => lineErrorScore(l) === 0)
    .map(autoComplete)
    .map(autoCompleteScore)
    .sort((a, b) => a - b)
  console.log(scores[Math.floor(scores.length / 2)])
}

function main() {
  // const input = getInput("input/day10-input-sample.txt")
  const input = getInput("input/day10-input.txt")

  part1(input)
  part2(input)
}

main()
