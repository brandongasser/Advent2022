import Utility.Parse (int, newline, parse, stringP, Parser)
import Control.Applicative (Alternative((<|>), some))
import Data.Maybe (fromJust)
import Data.List (transpose)

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

magicNumber :: Int
magicNumber = 4000000

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

excluded :: Reading -> [Maybe PartialSet]
excluded ((sx, sy), bp) = replicate minY Nothing ++ map (\row -> sequence [fix (sx - dist (sx, sy) bp + abs (row - sy), sx + dist (sx, sy) bp - abs (row - sy))]) [max 0 minY..min magicNumber maxY] ++ replicate (magicNumber - min magicNumber maxY) Nothing
    where
        d = dist (sx, sy) bp
        minY = sy - d
        maxY = sy + d
        fix (a, b) = if a > b then Nothing else Just (a, b)

ans :: Readings -> Int
ans = (\[(y, [(_, x), (_, _)])] -> (x + 1) * 4000000 + y) . filter ((==2) . length . snd) . zip [0..magicNumber] . map (foldr union' [] . fromJust . sequence . filter (/=Nothing)) . transpose . map excluded

main :: IO ()
main = do input <- readFile "day15/input.txt"
          let [(readings, input')] = parse readingsP input
          print $ ans readings