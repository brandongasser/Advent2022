import Utils (splitOn)
import Data.Set (Set, intersection, fromList)

type Assignment = Set Int

toAssignment :: String -> Assignment
toAssignment = (\[s, e] -> fromList [s..e]) . map read . splitOn '-'

fullyContained :: Assignment -> Assignment -> Bool
fullyContained a b = let i = intersection a b in i == a || i == b

overlap :: Assignment -> Assignment -> Bool
overlap a b = not $ null $ intersection a b

main :: IO ()
main = do input <- readFile "day4/input.txt"
          let assignments = map ((\[a, b] -> (a, b)) . map toAssignment . splitOn ',') $ lines input
          -- Part 1
          print $ length $ filter (uncurry fullyContained) assignments
          -- Part 2
          print $ length $ filter (uncurry overlap) assignments