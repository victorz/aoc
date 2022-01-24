export const getInput = (filename: string): string =>
  Deno.readTextFileSync(filename)

export const linesOf = (text: string): string[] => text.split("\n")
export const paragraphsOf = (text: string): string[] => text.split("\n\n")
export const nonEmpty = (text: string): boolean => text.length > 0
export const isNotNull = <T>(v: T | null): v is T => v != null

export const zip2 = <T, U>(ts: T[], us: U[]): [T, U][] =>
  ts.map((t, i) => [t, us[i]])

export const sum = (xs: number[]) => xs.reduce((a, b) => a + b, 0)
export const min = (xs: number[]) => xs.reduce((a, b) => Math.min(a, b))
export const max = (xs: number[]) => xs.reduce((a, b) => Math.max(a, b))

export function* range(start: number, end?: number) {
  if (end === undefined) {
    end = start
    start = 0
  }

  const reversed = end < start

  for (let i = start; reversed ? i >= end : i <= end; reversed ? i-- : i++) {
    yield i
  }
}

export const rangeToArray = (start: number, end: number) =>
  Array.from(range(start, end))
