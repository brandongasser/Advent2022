import Utility.Parse
import Control.Applicative
import Data.Char
import Control.Monad.State
import Data.List

data Monkey = Monkey {
    inspected :: Integer,
    items :: [Integer],
    operation :: Integer -> Integer,
    test :: Integer -> Integer
}

type Monkeys = [Monkey]

itemsP :: Parser [Integer]
itemsP = do ws
            stringP "Starting items: "
            raw <- many $ sat (/='\n')
            return $ read $ "[" ++ raw ++ "]"

operatorP :: Parser (Integer -> Integer -> Integer)
operatorP = (*) <$ charP '*' <|> (+) <$ charP '+'

operationP :: Parser (Integer -> Integer)
operationP = do ws
                stringP "Operation: new = old "
                op <- operatorP
                ws
                r <- some (sat (/='\n'))
                case r of
                    "old" -> return $ \x -> op x x
                    _     -> return $ \x -> op x (read r)

testP :: Parser (Integer -> Integer)
testP = do ws
           stringP "Test: divisible by "
           n <- read <$> some (sat isDigit)
           newline
           ws
           stringP "If true: throw to monkey "
           t <- read <$> some (sat isDigit)
           newline
           ws
           stringP "If false: throw to monkey "
           f <- read <$> some (sat isDigit)
           return $ \x -> if x `mod` n == 0 then t else f

monkeyP :: Parser Monkey
monkeyP = do stringP "Monkey "
             nat
             charP ':'
             newline
             i <- itemsP
             newline
             op <- operationP
             newline
             Monkey 0 i op <$> testP

monkeysP :: Parser [Monkey]
monkeysP = some (do m <- monkeyP
                    newline
                    newline
                    return m
                  <|> monkeyP)

type MonkeyT = State Monkey
type MonkeysT = State [Monkey]

move :: Int -> MonkeysT Int
move n  = do monkeys <- get
             let from = monkeys !! n
             case items from of
                [] -> do put monkeys
                         return $ n + 1
                (x:xs) -> do let from' = Monkey (inspected from + 1) xs (operation from) (test from)
                             let x' = operation from x `mod` 9699690 -- This magic number is the lcm of all of the test divisibilities. I cheated again, sorry :(
                             let toIndex = fromIntegral (test from x')
                             let to = monkeys !! toIndex
                             let to' = Monkey (inspected to) (x' : items to) (operation to) (test to)
                             put [if i == n then from' else if i == toIndex then to' else x | (i, x) <- zip [0..] monkeys]
                             return n

doRound :: Int -> MonkeysT ()
doRound n = do monkeys <- get
               if n >= length monkeys
               then put monkeys
               else do let (next, monkeys') = runState (move n) monkeys
                       let monkeys'' = execState (doRound next) monkeys'
                       put monkeys''
           
runGame :: Int -> MonkeysT ()
runGame n = do monkeys <- get
               if n <= 0
               then put monkeys
               else do let monkeys' = execState (doRound 0) monkeys
                       let monkeys'' = execState (runGame (n - 1)) monkeys'
                       put monkeys''

main :: IO ()
main = do input <- readFile "day11/input.txt"
          let [(monkeys, _)] = parse monkeysP input
          print $ product $ take 2 $ reverse $ sort $ map inspected $ execState (runGame 10000) monkeys