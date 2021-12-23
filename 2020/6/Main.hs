import Data.List.Extra (splitOn, trim)
import qualified Data.Set as Set

groupStrings = map trim . splitOn "\n\n"

groupQuestions = filter (/= '\n')

allAnswered xs = foldr Set.intersection allGroupQuestions xsSets
  where
    allGroupQuestions = (Set.fromList . concat) xs
    xsSets = map Set.fromList xs

main :: IO ()
main = do
  input <- readFile "input"
  let groups = groupStrings input
  print $ sum $ map (Set.size . Set.fromList . groupQuestions) groups
  print $ sum $ map (Set.size . allAnswered . splitOn "\n") groups
