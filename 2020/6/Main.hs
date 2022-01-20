import Data.List.Extra (splitOn, trim)
import qualified Data.Set as Set

allAnswered xs = foldr Set.intersection allGroupQuestions xsSets
  where
    allGroupQuestions = (Set.fromList . concat) xs
    xsSets = map Set.fromList xs

main :: IO ()
main = do
  input <- readFile "input"
  let groups = map trim . splitOn "\n\n" $ input
  -- part 1:
  print $ sum $ map (Set.size . Set.fromList . concat . lines) groups
  -- part 2:
  print $ sum $ map (Set.size . allAnswered . lines) groups
