import Utils (splitOn)
import Data.Set (Set, intersection, fromList)

type Assignment = Set Int

{-|
takes a string in the form of "start-end" and converts it into an assignment
containing all numbers between start and end inclusive.
-}
toAssignment :: String -> Assignment
toAssignment = (\[s, e] -> fromList [s..e]) . map read . splitOn '-'

fullyContained :: Assignment -> Assignment -> Bool
fullyContained a b = let i = intersection a b in i == a || i == b

overlap :: Assignment -> Assignment -> Bool
overlap a b = not $ null $ intersection a b

main :: IO ()
main = do input <- readFile "day 4/input.txt"
          let assignments = map (map toAssignment . splitOn ',') $ lines input
          -- Part 1
          print $ length $ filter (\[a, b] -> fullyContained a b) assignments
          -- Part 2
          print $ length $ filter (\[a, b] -> overlap a b) assignments