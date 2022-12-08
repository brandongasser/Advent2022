data Move = Rock | Paper | Scissors
    deriving (Show, Eq)

data GameType = Loss | Draw | Win
    deriving Show

toGameType :: String -> GameType
toGameType "X" = Loss
toGameType "Y" = Draw
toGameType "Z" = Win
toGameType _   = undefined

winScore :: Move -> Move -> Int
winScore Rock Scissors  = 6
winScore Paper Rock     = 6
winScore Scissors Paper = 6
winScore me opp
    | me == opp = 3
    | otherwise = 0

moveScore :: Move -> Int
moveScore Rock     = 1
moveScore Paper    = 2
moveScore Scissors = 3

score :: Move -> Move -> Int
score me opp = moveScore me + winScore me opp

winningMove :: Move -> Move
winningMove Rock     = Paper
winningMove Paper    = Scissors
winningMove Scissors = Rock

losingMove :: Move -> Move
losingMove Rock     = Scissors
losingMove Paper    = Rock
losingMove Scissors = Paper

drawingMove :: Move -> Move
drawingMove = id

hypScore :: Move -> GameType -> Int
hypScore move Draw = score (drawingMove move) move
hypScore move Loss = score (losingMove move) move
hypScore move Win  = score (winningMove move) move

toMove :: String -> Move
toMove str
    | str == "A" || str == "X" = Rock
    | str == "B" || str == "Y" = Paper
    | str == "C" || str == "Z" = Scissors
    | otherwise                = undefined

main :: IO ()
main = do input <- readFile "day2/input.txt"
          -- Part 1
          let games = map (map toMove . words) $ lines input
          print $ sum $ map (\[x, y] -> score y x) games
          -- Part 2
          let games = map ((\[x, y] -> (toMove x, toGameType y)) . words) $ lines input
          print $ sum $ map (uncurry hypScore) games