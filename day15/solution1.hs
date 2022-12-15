import Utility.Parse (int, newline, parse, stringP, Parser)
import Control.Applicative (Alternative((<|>), some))
import Data.Maybe (fromJust)

type Pos = (Int, Int)
type Reading = (Pos, Pos)
type Readings = [Reading]
type PartialSet = [(Int, Int)]

readingP :: Parser Reading
readingP = do stringP "Sensor at x="
              sx <- int
              stringP ", y="
              sy <- int
              stringP ": closest beacon is at x="
              bx <- int
              stringP ", y="
              by <- int
              return ((sx, sy), (bx, by))

readingsP :: Parser Readings
readingsP = some (do reading <- readingP
                     newline
                     return reading
                   <|> readingP)

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

testRow :: Int
testRow = 2000000

notHere :: Reading -> Maybe PartialSet
notHere ((sx, sy), (bx, by)) = let (start, end) = (sx - dist (sx, sy) (bx, by) + abs (testRow - sy), sx + dist (sx, sy) (bx, by) - abs (testRow - sy))
                         in  if   bx == start
                             then sequence [fix (start + 1, end)]
                             else if bx == end
                                  then sequence [fix (start, end - 1)]
                                  else sequence [fix (start, end)]
    where
        fix (a, b) = if a > b then Nothing else Just (a, b)

union' :: PartialSet -> PartialSet -> PartialSet
union' a [] = a
union' [] b = b
union' ((astart, aend):as) ((bstart, bend):bs) = if   astart <= bstart
                                                then let newstart = astart
                                                     in  if   aend < bstart - 1
                                                         then (newstart, aend):union' as ((bstart, bend):bs)
                                                         else union' ((newstart, max aend bend):as) bs
                                                else let newstart = bstart
                                                     in  if   bend < astart - 1
                                                         then (newstart, bend):union' ((astart, aend):as) bs
                                                         else union' ((newstart, max aend bend):as) bs

size :: PartialSet -> Int
size [] = 0
size ((start, end):xs) = end - start + 1 + size xs

notInTestRow :: Readings -> Int
notInTestRow = size . foldr union' [] . fromJust . sequence . filter (/=Nothing) . map notHere

main :: IO ()
main = do input <- readFile "day15/input.txt"
          let [(readings, input')] = parse readingsP input
          print $ notInTestRow readings