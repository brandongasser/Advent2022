import Stack
import Control.Monad.State (execStateT, MonadState(put, get), StateT(runStateT))
import Data.List (transpose)

type Column = Int
type Move = (Int, Column, Column)

type Cargo = Char
type CargoStack = Stack Cargo
type Ship = [CargoStack]
type ShipT = StateT Ship Maybe

toMove :: String -> Move
toMove str = (\[a, b, c] -> (a, b, c)) $ map read $ [x | (i, x) <- zip [0..] (words str), odd i]

moveToShipT :: Move -> ShipT ()
moveToShipT (n, from, to) = do ship <- get
                               let fromStack = ship !! (from - 1)
                               let toStack = ship !! (to - 1)
                               let Just (xs, fromStack') = runStateT (popN n) fromStack
                               let Just toStack' = execStateT (pushList $ reverse xs) toStack
                               let ship' = [if i == from then fromStack' else if i == to then toStack' else st | (i, st) <- zip [1..] ship]
                               put ship'

parseShip :: String -> Ship
parseShip str = parts
    where
        ls = init $ lines str
        parts = map (filter (/=' ')) $ transpose $ map (\xs -> [x | (i, x) <- zip [0..] xs, i `mod` 4 == 1]) ls

parseMoves :: String -> [Move]
parseMoves = map toMove . lines

parseInput :: String -> (Ship, [Move])
parseInput str = (parseShip $ unlines $ takeWhile (/="") ls, parseMoves $ unlines $ tail $ dropWhile (/="") ls)
    where
        ls = lines str

main :: IO ()
main = do input <- readFile "day 5/input.txt"
          let (ship, moves) = parseInput input
          let shipTs = map moveToShipT moves
          let Just ship' = execStateT (sequence shipTs) ship
          putStrLn [head st | st <- ship']