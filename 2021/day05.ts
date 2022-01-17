import { getInput, linesOf, isNotNull, range, zip2 } from "./Util.ts"

interface LineSegment {
  x1: number
  y1: number
  x2: number
  y2: number
}

const toLineSegments = (line: string): LineSegment | null => {
  const groups = line.match(
    /(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)/
  )?.groups

  if (groups == null) {
    return null
  }

  return {
    x1: Number(groups.x1),
    y1: Number(groups.y1),
    x2: Number(groups.x2),
    y2: Number(groups.y2),
  }
}

const onlyHorVert = ({ x1, y1, x2, y2 }: LineSegment) => {
  return x1 === x2 || y1 === y2
}

const toPointString = (a: number, b: number) => `${a},${b}`

const toPointStrings = ({ x1, y1, x2, y2 }: LineSegment) => {
  // Vertical line
  if (x1 === x2) {
    return Array.from(range(y1, y2)).map(y => toPointString(x1, y))
  }

  // Horizontal line
  if (y1 === y2) {
    return Array.from(range(x1, x2)).map(x => toPointString(x, y1))
  }

  // Diagonal (45 deg) line
  return zip2(Array.from(range(x1, x2)), Array.from(range(y1, y2))).map(
    ([x, y]) => toPointString(x, y)
  )
}

function part1(lines: string[]) {
  const vents = new Map<string, number>()
  lines
    .map(toLineSegments)
    .filter(isNotNull)
    .filter(onlyHorVert)
    .flatMap(toPointStrings)
    .forEach(s => {
      vents.set(s, (vents.get(s) ?? 0) + 1)
    })

  const dangerous = Array.from(vents.entries()).filter(([, n]) => n >= 2)
  console.log(dangerous.length)
}

function part2(lines: string[]) {
  const vents = new Map<string, number>()
  lines
    .map(toLineSegments)
    .filter(isNotNull)
    .flatMap(toPointStrings)
    .forEach(s => {
      vents.set(s, (vents.get(s) ?? 0) + 1)
    })

  const dangerous = Array.from(vents.entries()).filter(([, n]) => n >= 2)
  console.log(dangerous.length)
}

function main() {
  // const input = "input/day05-input-sample.txt"
  const input = "input/day05-input.txt"

  const lines = linesOf(getInput(input))

  // console.log(Array.from(range(1, 5)))
  // console.log(Array.from(range(5, 1)))

  part1(lines)
  part2(lines)
}

main()
