import Utility.Parse (charP, int, newline, parse, stringP, Parser)
import Utility.Utils (unique)
import Control.Applicative (Alternative((<|>), some))
import Control.Monad.State (MonadState(put, get), runState, State, execState)
import Data.Maybe (fromJust)

data Square = Empty | Sand | Wall
    deriving (Show, Eq)

type Grid = [[Square]]
type Pose = (Int, Int)
type GridT = State Grid

poseP :: Parser Pose
poseP = do x <- int
           charP ','
           y <- int
           return (x, y)

fillWall :: [Pose] -> [Pose]
fillWall poses = concatMap between pairs
    where
        pairs = zip poses (tail poses)
        between ((x1, y1), (x2, y2)) = if   x1 == x2
                                       then zip [x1,x1..] [min y1 y2..max y1 y2]
                                       else zip [min x1 x2..max x1 x2] [y1,y1..]

wallP :: Parser [Pose]
wallP = fillWall <$> some (do p <- poseP
                              stringP " -> "
                              return p
                            <|> poseP)

wallsP :: Parser [Pose]
wallsP = unique . concat <$> some (do w <- wallP
                                      newline
                                      return w
                                    <|> wallP)

xRange :: [Pose] -> (Int, Int)
xRange poses = (minimum xs, maximum xs)
    where
        xs = map fst poses

yRange :: [Pose] -> (Int, Int)
yRange poses = (0, maximum xs)
    where
        xs = map snd poses

gridP :: Parser Grid
gridP = do walls <- wallsP
           return [[squareType (x, y) walls | y <- [0..snd (yRange walls) + 2]] | x <- [0..snd (xRange walls) + 500]]
    where
        squareType (x, y) walls = if   (x, y) `elem` walls || y == snd (yRange walls) + 2
                                  then Wall
                                  else Empty

find :: Pose -> Grid -> Maybe Square
find (x, y) poses
    | x > maxX || x < 0 || y > maxY || y < 0 = Nothing
    | otherwise                              = Just (poses !! x !! y)
        where
            maxX = length poses - 1
            maxY = length (head poses) - 1

sandSpot :: Pose
sandSpot = (500, 0)

canMoveDown :: Pose -> Grid -> Maybe Bool
canMoveDown (x, y) poses = case find (x, y + 1) poses of
                                Nothing    -> Nothing
                                Just Empty -> Just True
                                Just t     -> Just False

canMoveLeft :: Pose -> Grid -> Maybe Bool
canMoveLeft (x, y) poses = case find (x - 1, y + 1) poses of
                                Nothing    -> Nothing
                                Just Empty -> Just True
                                Just t     -> Just False

canMoveRight :: Pose -> Grid -> Maybe Bool
canMoveRight (x, y) poses = case find (x + 1, y + 1) poses of
                                Nothing    -> Nothing
                                Just Empty -> Just True
                                Just t     -> Just False

findRestingSpot :: Pose -> Grid -> Maybe Pose
findRestingSpot (x, y) poses = case canMoveDown (x, y) poses of
                                    Nothing -> Nothing
                                    Just True -> findRestingSpot (x, y + 1) poses
                                    Just False -> case canMoveLeft (x, y) poses of
                                                    Nothing -> Nothing
                                                    Just True -> findRestingSpot (x - 1, y + 1) poses
                                                    Just False -> case canMoveRight (x, y) poses of
                                                                    Nothing -> Nothing
                                                                    Just True -> findRestingSpot (x + 1, y + 1) poses
                                                                    Just False -> Just (x, y)

addSand :: Pose -> Grid -> Grid
addSand (x, y) poses = [[if (x', y') == (x, y) then Sand else fromJust $ find (x', y') poses | y' <- [0..length (head poses) - 1]] | x' <- [0..length poses - 1]]

-- dropSand :: GridT Bool
-- dropSand = do grid <- get
--               case findRestingSpot sandSpot grid of
--                 Nothing -> return True
--                 Just pose -> do put $ addSand pose grid
--                                 return False

-- firstMatch :: Int -> Grid -> Int
-- firstMatch n poses = let (fellOff, grid') = runState dropSand poses
--                      in if fellOff
--                         then error "not enough space" 
--                         else if   find sandSpot grid' == Just Sand
--                              then n
--                              else firstMatch (n + 1) grid'

connected :: Pose -> Grid -> [Pose]
connected (x, y) poses = result''
    where
        result = case canMoveDown (x, y) poses of
                    Just True -> [(x, y + 1)]
                    _         -> []
        result' = case canMoveLeft (x, y) poses of
                    Just True -> (x - 1, y + 1):result
                    _         -> result
        result'' = case canMoveRight (x, y) poses of
                    Just True -> (x + 1, y + 1):result'
                    _         -> result'

bfs :: [Pose] -> [Pose] -> Grid -> Int
bfs at visited poses = if   null at
                       then length visited + length at
                       else bfs next (visited ++ at) poses
    where
        next = unique $ concatMap (`connected` poses) at

filled :: Grid -> Int
filled = bfs [sandSpot] []

main :: IO ()
main = do input <- readFile "day14/input.txt"
          let [(grid, input')] = parse gridP input
          print $ filled grid