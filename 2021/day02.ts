import { linesOf, getInput, nonEmpty } from "./Util.ts"

function main() {
  const lines = linesOf(getInput("input/day02-input.txt")).filter(nonEmpty)

  part2(lines)
}

main()

function part2(lines: string[]) {
  let aim = 0
  let depth = 0
  let horiz = 0
  lines.forEach(line => {
    const [cmd, x] = line.split(" ")
    const X = Number.parseInt(x)

    switch (cmd) {
      case "up":
        aim -= X
        break
      case "down":
        aim += X
        break
      case "forward":
        horiz += X
        depth += aim * X
        break
      default:
        console.assert(false, `unknown command: "${cmd}"`)
    }
  })

  console.log(horiz * depth)
}
