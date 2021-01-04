import Data.List ()
import Data.List.Extra (replace, splitOn, trim)

entryStrings :: String -> [String]
entryStrings s = filter (not . null) $ map (trim . replace "\n" " ") $ splitOn "\n\n" s

entryPair :: String -> [(String, String)]
entryPair s = map (pairFromList . (splitOn ":")) $ words s

pairFromList xs = (head xs, last xs)

main = do
  input <- readFile "input"
  print $ map entryPair $ entryStrings input

  return ()
