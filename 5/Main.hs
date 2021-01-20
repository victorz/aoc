import Data.List.Extra
import Data.List.Split ()
import qualified Data.Set as Set

binSearch l h start end [x]
  | x == l = start
  | x == h = end
  | otherwise = error "invalid seating"
binSearch l h start end (x : xs)
  | x == l = binSearch l h start mid xs
  | x == h = binSearch l h (mid + 1) end xs
  | otherwise = error "invalid seating"
  where
    mid = div (start + end) 2

findRow = binSearch 'F' 'B' 0 127

findColumn = binSearch 'L' 'R' 0 7

findSeat s = id
  where
    r = fromInteger $ findRow $ take 7 s
    c = fromInteger $ findColumn $ takeEnd 3 s
    id = r * 8 + c

main :: IO ()
main = do
  input <- readFile "input"
  let seatIds = map findSeat $ filter notNull $ splitOn "\n" input
  let maxId = maximum seatIds
  print maxId
  let minId = minimum seatIds
  let idSet = Set.fromList seatIds
  let allIds = Set.fromList [minId .. maxId]
  print $ head $ Set.toList $ Set.difference allIds idSet
