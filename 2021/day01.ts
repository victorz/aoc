import { linesOf, getInput } from "./Util.ts"

function* getTopo(depths: number[]) {
  for (let i = 0; i < depths.length - 1; i++) {
    const a = depths[i]
    const b = depths[i + 1]

    yield a === b ? 0 : Math.sign(b - a)
  }
}

const add = (a: number, b: number) => a + b
const sum = (xs: number[]) => xs.reduce(add, 0)

function* getTopo3(depths: number[]) {
  for (let i = 0; i < depths.length - 3; i++) {
    const A = sum([depths[i], depths[i + 1], depths[i + 2]])
    const B = sum([depths[i + 1], depths[i + 2], depths[i + 3]])

    yield A === B ? 0 : Math.sign(B - A)
  }
}

function part1(depths: number[]) {
  const count = [...getTopo(depths)].filter(t => t > 0).length
  console.log(`Part 1 answer: ${count}`)
}

function part2(depths: number[]) {
  const count = [...getTopo3(depths)].filter(t => t > 0).length
  console.log(`Part 2 answer: ${count}`)
}

function main() {
  const depths = linesOf(getInput("input/day01-input.txt")).map(s =>
    Number.parseInt(s)
  )

  part1(depths)
  part2(depths)
}

main()
