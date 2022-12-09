import Utility.Parse (int, newline, parse, stringP, Parser)
import Utility.Utils (unique)
import Control.Applicative (Alternative((<|>), some))
import Control.Monad.State (MonadState(put, get), evalState, execState, runState, State)

-- Initial Segments

seg1 :: Segment
seg1 = Rope (0, 0) (Just seg2)

seg2 :: Segment
seg2 = Rope (0, 0) (Just seg3)

seg3 :: Segment
seg3 = Rope (0, 0) (Just seg4)

seg4 :: Segment
seg4 = Rope (0, 0) (Just seg5)

seg5 :: Segment
seg5 = Rope (0, 0) (Just seg6)

seg6 :: Segment
seg6 = Rope (0, 0) (Just seg7)

seg7 :: Segment
seg7 = Rope (0, 0) (Just seg8)

seg8 :: Segment
seg8 = Rope (0, 0) (Just seg9)

seg9 :: Segment
seg9 = Rope (0, 0) (Just seg10)

seg10 :: Segment
seg10 = Rope (0, 0) Nothing

-- Answer

data Move = MUp | MDown | MLeft | MRight | MUpLeft | MUpRight | MDownLeft | MDownRight
    deriving Show

data Segment = Rope Pos (Maybe Segment)
    deriving Show

position :: Segment -> Pos
position (Rope pos _) = pos

tPos :: Segment -> Pos
tPos (Rope pos Nothing)   = pos
tPos (Rope _ (Just next)) = tPos next

follower :: Segment -> Maybe Segment
follower (Rope _ f) = f

type Pos = (Int, Int)
type PosT = State Pos
type SegmentT = State Segment

parseUp :: Parser [Move]
parseUp = do stringP "U "
             n <- int
             return $ replicate n MUp

parseDown :: Parser [Move]
parseDown = do stringP "D "
               n <- int
               return $ replicate n MDown

parseLeft :: Parser [Move]
parseLeft = do stringP "L "
               n <- int
               return $ replicate n MLeft

parseRight :: Parser [Move]
parseRight = do stringP "R "
                n <- int
                return $ replicate n MRight

parseMove :: Parser [Move]
parseMove = parseUp <|> parseDown <|> parseLeft <|> parseRight

parseMoves :: Parser [Move]
parseMoves = concat <$> some (do m <- parseMove
                                 newline
                                 return m
                               <|> parseMove)

main :: IO ()
main = do input <- readFile "day9/input.txt"
          let [(moves, _)] = parse parseMoves input
          let poses = evalState (mapM moveToSegmentT moves) seg1
          print $ length $ unique poses

moveToPosT :: Move -> PosT ()
moveToPosT MUp        = do (x, y) <- get
                           put (x, y + 1)
moveToPosT MDown      = do (x, y) <- get
                           put (x, y - 1)
moveToPosT MLeft      = do (x, y) <- get
                           put (x - 1, y)
moveToPosT MRight     = do (x, y) <- get
                           put (x + 1, y)
moveToPosT MUpLeft    = do moveToPosT MUp
                           moveToPosT MLeft
moveToPosT MUpRight   = do moveToPosT MUp
                           moveToPosT MRight
moveToPosT MDownLeft  = do moveToPosT MDown
                           moveToPosT MLeft
moveToPosT MDownRight = do moveToPosT MDown
                           moveToPosT MRight

moveToSegmentT :: Move -> SegmentT Pos
moveToSegmentT move = do s <- get
                         case s of
                            Rope pos Nothing -> do let newPos = execState (moveToPosT move) pos
                                                   put $ Rope newPos Nothing
                                                   return newPos
                            Rope pos (Just f) -> do let nextPos = execState (moveToPosT move) pos
                                                    if   dist nextPos (position f) > 1
                                                    then do let (result, newFollower) = runState (moveToSegmentT (directionToMove nextPos (position f))) f
                                                            put $ Rope nextPos (Just newFollower)
                                                            return result
                                                    else do put $ Rope nextPos (Just f)
                                                            return (tPos f)

directionToMove :: Pos -> Pos -> Move
directionToMove (x1, y1) (x2, y2)
    | x1 > x2 && y1 == y2 = MRight
    | x1 > x2 && y1 /= y2 = if y1 > y2 then MUpRight else MDownRight
    | x1 < x2 && y1 == y2 = MLeft
    | x1 < x2 && y1 /= y2 = if y1 > y2 then MUpLeft else MDownLeft
    | otherwise           = if y1 > y2 then MUp else MDown

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))