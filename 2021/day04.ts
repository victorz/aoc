import { sum, getInput, rangeToArray } from "./Util.ts"

type Steps = number[]
type BoardRow = BoardCell[]
type Board = BoardRow[]

interface BoardCell {
  value: number
  marked: boolean
}

const parseBoard = (board: string): Board =>
  board
    .split("\n")
    .filter(l => l.trim().length > 0)
    .map(parseBoardLine)

const parseBoardLine = (l: string): BoardRow =>
  l
    .split(" ")
    .filter(c => c.trim().length > 0)
    .map(c => ({ value: Number(c), marked: false }))

function checkRow(row: BoardRow) {
  return row.every(c => c.marked)
}

function checkColumn(board: Board, col: number) {
  return board.map(row => row[col]).every(c => c.marked)
}

function checkWin(board: Board) {
  return (
    board.some(checkRow) ||
    rangeToArray(board[0].length - 1).some(col => checkColumn(board, col))
  )
}

function boardScore(board: Board) {
  return sum(
    board
      .flat()
      .filter(c => !c.marked)
      .map(c => c.value)
  )
}

function mark(board: Board, step: number) {
  board.forEach(row => {
    row.forEach(c => {
      if (c.value === step) {
        c.marked = true
      }
    })
  })
}

function resetBoard(board: Board) {
  board.flat().forEach(c => {
    c.marked = false
  })
}

function debugBoard(board: Board): string {
  return board
    .map(
      row =>
        row.map(c => `${c.value}`.padStart(2)).join(" ") +
        "\t" +
        row.map(c => (c.marked ? "x" : ".").padStart(2)).join(" ")
    )
    .join("\n")
}

function part1(boards: Board[], steps: Steps) {
  for (let step of steps) {
    for (let board of boards) {
      mark(board, step)

      if (checkWin(board)) {
        console.log(boardScore(board) * step)
        return
      }
    }
  }
}

function part2(boards: Board[], steps: Steps) {
  /**
   * Algo:
   * - Process each step
   * - Keep track of last winning board and its winning step
   * - After all steps have been processed or all boards have won
   *   - Print score of last winning board multiplied by its winning step.
   */
  const [lastWinner, winningStep] = steps.reduce<
    [Board | undefined, number | undefined]
  >(
    ([lastWinner, winningStep], step) => {
      const roundWinners = new Set<Board>()

      boards.forEach(board => {
        mark(board, step)

        if (checkWin(board)) {
          lastWinner = board
          winningStep = step
          roundWinners.add(board)
        }
      })

      boards = boards.filter(b => !roundWinners.has(b))

      return [lastWinner, winningStep]
    },
    [undefined, undefined]
  )

  console.assert(lastWinner !== undefined, "no last winner")
  console.assert(winningStep !== undefined, "no last winning step")

  console.log(boardScore(lastWinner!) * winningStep!)
}

function main() {
  // const input = getInput("input/day04-input-sample.txt")
  const input = getInput("input/day04-input.txt")
  const [stepsStr, ...boardsStrArr] = input.split("\n\n")
  const steps: Steps = stepsStr.split(",").map(Number)
  const boards: Board[] = boardsStrArr.map(parseBoard)

  part1(boards, steps)
  boards.forEach(resetBoard)
  part2(boards, steps)
}

main()
