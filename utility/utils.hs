module Utility.Utils where

{-|
Splits a list into a list of lists without including the delimeter.

splitOn "" ["", "1", "2", "", "3", "4", "5", "", "", "7", ""] => [[], ["1", "2"], ["3", "4", "5"], [], ["7"], []]
-}
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn e [] = []
splitOn e xs = if e `elem` xs
                then takeWhile (/=e) xs : splitOn e (tail $ dropWhile (/=e) xs)
                else [xs]

{-|
Removes all non-unique elements from a list.
The last occurrance of an element is the one that is kept in the result.

unique [1, 2, 3, 4, 2, 4] => [1, 3, 2, 4]
-}
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
    | x `elem` xs = unique xs
    | otherwise   = x : unique xs