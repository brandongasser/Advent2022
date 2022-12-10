import Utility.Parse (int, newline, parse, stringP, ws, Parser)
import Utility.Utils (breakBy)
import Control.Applicative (Alternative((<|>), some))
import Control.Monad.State (runState, MonadState(put, get), State)

type Program = [Op]
data Op = Noop | Add Int
    deriving Show

initial :: Int
initial = 1

type RegisterT = State Int

noopP :: Parser Op
noopP = do stringP "noop"
           return Noop

addP :: Parser Op
addP = do stringP "addx"
          ws
          Add <$> int

opP :: Parser Op
opP = noopP <|> addP

programP :: Parser Program
programP = concat <$> some (do op <- opP
                               newline
                               case op of
                                    Noop -> return [Noop]
                                    Add x -> return [Noop, Add x]
                             <|> do op <- opP
                                    case op of
                                        Noop -> return [Noop]
                                        Add x -> return [Noop, Add x])

opToRegisterT :: Op -> RegisterT Int
opToRegisterT Noop    = get
opToRegisterT (Add x) = do reg <- get
                           let reg' = reg + x
                           put reg'
                           return reg

runProgram :: Program -> Int -> ([Int], Int)
runProgram prog = runState (mapM opToRegisterT prog)

main :: IO ()
main = do input <- readFile "day10/input.txt"
          let [(program, _)] = parse programP input
          let (result, _) = runProgram program initial
          -- Part 1
          print $ ans1 result
          -- Part 2
          putStrLn $ ans2 result
          -- Part 2 but more readable
          putStr $ ans2Readability result

ans1 :: [Int] -> Int
ans1 result = sum wanted
    where
        wanted = take 6 [i * x | (i, x) <- zip [1..] result, (i - 20) `mod` 40 == 0]

ans2 :: [Int] -> String
ans2 result = screen
    where
        pixels = [if abs ((i `mod` 40) - x) <= 1 then '#' else '.' | (i, x) <- zip [0..] result]
        screen = unlines $ breakBy 40 pixels

ans2Readability :: [Int] -> String
ans2Readability = map (\c -> if c == '.' then ' ' else c) . ans2