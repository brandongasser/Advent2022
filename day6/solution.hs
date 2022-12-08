import Utility.Utils (unique)

main :: IO ()
main = do input <- readFile "day6/input.txt"
          -- Part 1
          print $ ans 4 input
          -- Part 2
          print $ ans 14 input

ans :: Int -> String -> Int
ans n input = let gs = groups n input
              in  fst $ head $ filter (different . snd) $ zip [n..] gs

groups :: Int -> [a] -> [[a]]
groups n xs
    | length xs < n = []
    | otherwise     = take n xs : groups n (tail xs)

different :: (Eq a) => [a] -> Bool
different xs = unique xs == xs