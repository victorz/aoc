import { getInput, sum, min, max, rangeToArray } from "./Util.ts"

type Crabs = number[]

function cost(crabs: Crabs, pos: number): number {
  return sum(crabs.map(c => Math.abs(c - pos)))
}

function cost2(crabs: Crabs, pos: number): number {
  return sum(crabs.map(c => partialSum(Math.abs(c - pos))))
}

const partialSum = (n: number) => (n * (n + 1)) / 2

function part1(crabs: Crabs) {
  const [minPos, maxPos] = [min(crabs), max(crabs)]
  const costs = rangeToArray(minPos, maxPos).map(pos => cost(crabs, pos))
  const minCost = min(costs)
  console.log(minCost)
}

function part2(crabs: Crabs) {
  const [minPos, maxPos] = [min(crabs), max(crabs)]
  const costs = rangeToArray(minPos, maxPos).map(pos => cost2(crabs, pos))
  const minCost = min(costs)
  console.log(minCost)
}

function main() {
  const input = getInput("input/day07-input.txt").split(",").map(Number)
  // const input = getInput("input/day07-input-sample.txt").split(",").map(Number)

  part1(input)
  part2(input)
}

main()
