import { getInput } from "./Util.ts"

function tick(fish: number[]) {
  let numNew = 0

  for (let i = 0; i < fish.length; i++) {
    fish[i]--

    if (fish[i] < 0) {
      numNew++
      fish[i] = 6
    }
  }

  for (let i = 0; i < numNew; i++) {
    fish.push(8)
  }
}

function part1(fish: number[]) {
  for (let i = 0; i < 80; i++) {
    tick(fish)
  }

  console.log(fish.length)
}

function part2(fish: number[]) {
  // FIXME: Runs out of memory, need to compress by grouping fish with the same life span, should be only 9 slots! Very fast!
  for (let i = 0; i < 256; i++) {
    tick(fish)
  }

  console.log(fish.length)
}

function main() {
  // const inputSrc = "input/day06-input-sample.txt"
  const inputSrc = "input/day06-input.txt"

  const input = getInput(inputSrc).split(",").map(Number)

  part1(input)
  part2(input)
}

main()
