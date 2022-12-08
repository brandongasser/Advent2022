{-# LANGUAGE FlexibleContexts #-}

import Data.Char
import Data.List

type Forest = [[Int]]
type Position = (Int, Int)

visible :: Position -> Forest -> Bool
visible (x, y) forest = or [visibleRow lefty (head forests !! leftx),
                            visibleRow topy (forests !! 1 !! topx),
                            visibleRow righty (forests !! 2 !! rightx),
                            visibleRow bottomy (forests !! 3 !! bottomx)]
    where
        width = length $ head forest
        height = length $ head $ transpose forest
        forests = fromEdges forest
        (leftx, lefty) = (x, y)
        (topx, topy) = (y, x)
        (rightx, righty) = (x, width - y - 1)
        (bottomx, bottomy) = (y, height - x - 1)
        visibleRow i row = all (< (row !! i)) (take i row)

fromEdges :: Forest -> [Forest]
fromEdges forest = [forest, transpose forest, map reverse forest, map reverse (transpose forest)]

parseForest :: IO Forest
parseForest = do input <- readFile "day8/input.txt"
                 return $ map (map digitToInt) (lines input)

ans1 :: Forest -> Int
ans1 forest = length $ filter (`visible` forest) allPoses
    where
        maxX = length forest - 1
        maxY = length (head forest) - 1
        allPoses = [(x, y) | x <- [0..maxX], y <- [0..maxY]]

main :: IO ()
main = do forest <- parseForest
          -- Part 1
          print $ ans1 forest
          -- Part 2
          print $ ans2 forest

score :: Position -> Forest -> Int
score (x, y) forest = product [seen $ drop lefty (head forests !! leftx),
                               seen $ drop topy (forests !! 1 !! topx),
                               seen $ drop righty (forests !! 2 !! rightx),
                               seen $ drop bottomy (forests !! 3 !! bottomx)]
    where
        width = length $ head forest
        height = length $ head $ transpose forest
        forests = fromEdges forest
        (leftx, lefty) = (x, y)
        (topx, topy) = (y, x)
        (rightx, righty) = (x, width - y - 1)
        (bottomx, bottomy) = (y, height - x - 1)
        seen row = let raw = takeWhile (< head row) (tail row)
                   in  if   (head row : raw) == row
                       then length raw
                       else length raw + 1

ans2 :: Forest -> Int
ans2 forest = maximum $ map (`score` forest) allPoses
    where
        maxX = length forest - 1
        maxY = length (head forest) - 1
        allPoses = [(x, y) | x <- [0..maxX], y <- [0..maxY]]