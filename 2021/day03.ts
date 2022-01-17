import { linesOf, getInput, rangeToArray, nonEmpty } from "./Util.ts"

const maskBit = (x: number, k: number) => x & (1 << k)
const isSet = (x: number, k: number, to = 1) => maskBit(x, k) === to << k

const mostCommonBit = (xs: number[], k: number): 0 | 1 =>
  xs.filter(x => isSet(x, k)).length >= xs.length / 2 ? 1 : 0

const gamma = (xs: number[], bitLength: number) =>
  rangeToArray(bitLength - 1).reduce(
    (gamma, k) => gamma | (mostCommonBit(xs, k) << k),
    0
  )

const epsilon = (gamma: number, bitLength: number) =>
  ~gamma & ~(~0 << bitLength)

function part1(xs: number[], bitLength: number) {
  const g = gamma(xs, bitLength)
  const e = epsilon(g, bitLength)
  console.log("power consumption:", g * e)
}

type compareFunc = (x: number, k: number, mcb: number) => boolean

const filterByCriteria = (cr: compareFunc, xs: number[], bitLength = 12) =>
  rangeToArray(bitLength - 1).reduceRight((xss, k) => {
    if (xss.length > 1) {
      const mcb = mostCommonBit(xss, k)
      return xss.filter(x => cr(x, k, mcb))
    }

    return xss
  }, xs)[0]

function part2(xs: number[], bitLength: number) {
  const oxygen = filterByCriteria(
    (x, k, mcb) => isSet(x, k, mcb),
    xs,
    bitLength
  )
  const co2Rating = filterByCriteria(
    (x, k, mcb) => !isSet(x, k, mcb),
    xs,
    bitLength
  )

  console.log("life support rating:", oxygen * co2Rating)
}

function main() {
  const lines = linesOf(getInput("input/day03-input.txt")).filter(nonEmpty)
  const numbers = lines.map(l => Number.parseInt(l, 2))
  const bitLength = lines[0]?.length

  part1(numbers, bitLength)
  part2(numbers, bitLength)
}

main()
