import { getInput, sum } from "./Util.ts"

type FishSim = number[]

function tick2(fish: FishSim) {
  const spawning = fish[0]
  fish.shift()
  fish[6] += spawning
  fish[8] = spawning
}

function tick(fish: FishSim) {
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

function part1(fish: FishSim) {
  for (let i = 0; i < 80; i++) {
    tick(fish)
  }

  console.log(fish.length)
}

function part2(ages: FishSim) {
  const sim = buildSim(ages)

  for (let i = 0; i < 256; i++) {
    tick2(sim)
  }

  console.log(sum(sim))
}

function buildSim(ages: number[]): FishSim {
  return ages.reduce((sim, a) => {
    sim[a]++
    return sim
  }, new Array(9).fill(0))
}

function main() {
  const inputSrc = "input/day06-input.txt"
  const input = getInput(inputSrc).split(",").map(Number)

  part1(input.slice())
  part2(input)
}

main()
