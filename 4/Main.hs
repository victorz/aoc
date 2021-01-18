import Data.Char (isDigit, isHexDigit)
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Extra (notNull, replace, splitOn, trim)

entryStrings :: String -> [String]
entryStrings s = filter notNull $ map (trim . replace "\n" " ") $ splitOn "\n\n" s

pairFromList :: [a] -> (a, a)
pairFromList xs = (head xs, last xs)

entryPairs :: String -> [(String, String)]
entryPairs s = map (pairFromList . splitOn ":") $ words s

validInterval :: (Ord a) => a -> a -> a -> Bool
validInterval low high x = low <= x && x <= high

validHeight :: String -> Bool
validHeight hgt
  | "cm" `isSuffixOf` hgt =
    let cm = read $ head $ splitOn "cm" hgt :: Integer
     in validInterval 150 193 cm
  | "in" `isSuffixOf` hgt =
    let inch = read $ head $ splitOn "in" hgt :: Integer
     in validInterval 59 76 inch
  | otherwise = False

validEntry :: (String, String) -> Bool
validEntry ("byr", val) =
  let yr = read val :: Integer
   in validInterval 1920 2002 yr
validEntry ("iyr", val) =
  let yr = read val :: Integer
   in validInterval 2010 2020 yr
validEntry ("eyr", val) =
  let yr = read val :: Integer
   in validInterval 2020 2030 yr
validEntry ("hgt", val) = validHeight val
validEntry ("hcl", val) = "#" `isPrefixOf` val && all isHexDigit (tail val)
validEntry ("ecl", val) = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validEntry ("pid", val) = length val == 9 && all isDigit val
validEntry ("cid", _) = True
validEntry _ = False

validPassport :: [(String, String)] -> Bool
validPassport passport = length p == 7 && all validEntry passport
  where
    p = filter (\x -> fst x /= "cid") passport

main :: IO ()
main = do
  input <- readFile "input"
  print $ length $ filter validPassport $ map entryPairs $ entryStrings input
