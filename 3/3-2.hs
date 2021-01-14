import Data.List ()

readInput = lines <$> readFile "./input.txt"

isTree '#' = True
isTree _ = False

everyf n [] = []
everyf n xs = head xs : everyf n (drop n xs)

every n = everyf n . drop (n - 1)

numTrees forest rStep cStep =
  length $ filter isTree [cycle line !! n | (n, line) <- zip [0, cStep ..] $ everyf rStep forest]

{-
  Right 1, down 1.
  Right 3, down 1. (This is the slope you already checked.)
  Right 5, down 1.
  Right 7, down 1.
  Right 1, down 2.
-}
paths =
  [ (1, 1),
    (1, 3),
    (1, 5),
    (1, 7),
    (2, 1)
  ]

main = do
  forest <- readInput
  print $ product $ map (uncurry $ numTrees forest) paths
  return ()
