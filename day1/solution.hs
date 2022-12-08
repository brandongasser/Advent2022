import Data.List (sort)
import Utility.Parse (Parser, parse, newline, int)
import Control.Applicative (Alternative((<|>), some))

type Elf = [Int]

elfP :: Parser Elf
elfP = some (do x <- int
                newline
                return x
              <|> int)

parser :: Parser [Elf]
parser = some (do elf <- elfP
                  newline
                  return elf
                <|> elfP)

main :: IO ()
main = do input <- readFile "day1/input.txt"
          let [(elves, input')] = parse parser input
          -- Part 1
          print $ maximum $ map sum elves
          -- Part 2
          print $ sum $ take 3 $ reverse $ sort $ map sum elves