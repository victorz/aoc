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

function part1(input: string) {
  console.log(
    sum(
      linesOf(input).map(line => {
        const stack: Char[] = []
        let errorChar: Char = ""

        for (let ch of Array.from(line)) {
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
      })
    )
  )
}
function part2(input: string) {}

function main() {
  // const input = getInput("input/day10-input-sample.txt")
  const input = getInput("input/day10-input.txt")

  part1(input)
  part2(input)
}

main()
