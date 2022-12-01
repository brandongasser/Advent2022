import Data.List

main :: IO ()
main = do input <- readFile "input.txt"
          let nums = map (map read) $ splitOn "" $ lines input :: [[Int]]
          -- Part 1
          print $ maximum $ map sum nums
          -- Part 2
          print $ sum $ take 3 $ reverse $ sort $ map sum nums

{-|
Splits a list into a list of lists without including the delimeter

splitOn "" ["1", "2", "", "3", "4", "5", "" "7"] => [["1", "2"], ["3", "4", "5"], ["7"]]
-}
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn e [] = []
splitOn e xs = if e `elem` xs
                then takeWhile (/=e) xs : splitOn e (tail $ dropWhile (/=e) xs)
                else [xs]