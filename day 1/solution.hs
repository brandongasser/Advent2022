import Utils (splitOn)
import Data.List (sort)

main :: IO ()
main = do input <- readFile "input.txt"
          let nums = map (map read) $ splitOn "" $ lines input
          -- Part 1
          print $ maximum $ map sum nums
          -- Part 2
          print $ sum $ take 3 $ reverse $ sort $ map sum nums