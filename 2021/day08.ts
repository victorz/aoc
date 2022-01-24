import { linesOf, getInput } from "./Util.ts"

function part1(lines: string[]) {
  console.debug(lines)
  const result = lines
    .flatMap(l =>
      l
        .split("|")[1]
        .trimStart()
        .split(" ")
        .map(d => d.length)
    )
    .filter(len => [2, 4, 3, 7].includes(len)).length

  console.log(result)
}

function part2() {}

function main() {
  // const input = getInput("input/day08-input-sample.txt")
  const input = getInput("input/day08-input.txt")

  const lines = linesOf(input)

  part1(lines)
  part2()
}

main()
