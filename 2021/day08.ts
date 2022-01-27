import { sum, getInput, linesOf } from "./Util.ts"

function part1(lines: string[]) {
  const result = lines
    .flatMap(l =>
      l
        .split("| ")[1]
        .split(" ")
        .map(d => d.length)
    )
    .filter(len => [2, 4, 3, 7].includes(len)).length

  console.log(result)
}

type Segment = string
type SegmentCount = number
type Digit = string
type Pattern = string

interface LineData {
  patterns: Pattern[]
  output: Pattern[]
}

const SEGMENT_SCORE_MAP: Map<SegmentCount, Digit> = new Map([
  [42, "0"],
  [17, "1"],
  [34, "2"],
  [39, "3"],
  [30, "4"],
  [37, "5"],
  [41, "6"],
  [25, "7"],
  [49, "8"],
  [45, "9"],
])

function makeDigitMap(patterns: Pattern[]): Map<Pattern, Digit> {
  const sets = patterns.map(p => new Set(p))
  const scores = Array.from("abcdefg").reduce(
    (map, segment) =>
      map.set(segment, sets.filter(set => set.has(segment)).length),
    new Map<Segment, SegmentCount>()
  )

  const patScore = (pat: string): number =>
    sum(Array.from(pat).map(seg => scores.get(seg)!))

  return new Map(
    patterns.map(pat => [pat, SEGMENT_SCORE_MAP.get(patScore(pat))!] as const)
  )
}

function part2(lines: string[]) {
  const lineData = lines.map<LineData>(l => {
    const [patterns, output] = l
      .split(" | ")
      .map(s => s.split(" ").map(s => Array.from(s).sort().join("")))

    return {
      patterns,
      output,
    }
  })

  console.log(
    sum(
      lineData.map(({ patterns, output }) => {
        const digitMap = makeDigitMap(patterns)
        return Number(output.map(digitPat => digitMap.get(digitPat)!).join(""))
      })
    )
  )
}

function main() {
  // const input = getInput("input/day08-input-sample.txt")
  const input = getInput("input/day08-input.txt")
  const lines = linesOf(input)

  part1(lines)
  part2(lines)
}

main()
