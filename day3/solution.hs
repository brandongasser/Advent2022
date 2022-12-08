import Data.Char (ord, isUpper)
import Utility.Utils (unique)

type Backpack = String
type Group = (Backpack, Backpack, Backpack)

priority :: Char -> Int
priority c
    | isUpper c = ord c - 38
    | otherwise = ord c - 96

backpackPriority :: Backpack -> Int
backpackPriority xs = sum $ map priority $ unique $ filter (`elem` secondCompartment) firstCompartment
    where
        firstCompartment = take (length xs `div` 2) xs
        secondCompartment = drop (length xs `div` 2) xs

groups :: [Backpack] -> [Group]
groups (x:y:z:xs) = (x, y, z) : groups xs
groups _          = []

badge :: Group -> Char
badge (x, y, z) = head $ filter (\c -> c `elem` y && c `elem` z) x

main :: IO ()
main = do input <- readFile "day3/input.txt"
          -- Part 1
          print $ sum $ map backpackPriority $ lines input
          -- Part 2
          print $ sum $ map (priority . badge) $ groups $ lines input