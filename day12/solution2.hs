import Data.Char (ord)
import Control.Monad.State (execState, MonadState(put, get), State)
import Utility.Utils (unique)

type Coords = (Int, Int)
type Heightmap = [Position]

data Position = Pos {
    coords :: Coords,
    height :: Int,
    up :: Maybe Coords,
    down :: Maybe Coords,
    left :: Maybe Coords,
    right :: Maybe Coords
} deriving Show

instance Eq Position where
    a == b = coords a == coords b

createPos :: Coords -> Int -> Position
createPos c h = Pos c h Nothing Nothing Nothing Nothing

addUp :: Position -> Coords -> Position
addUp (Pos c h _ d l r) u = Pos c h (Just u) d l r

addDown :: Position -> Coords -> Position
addDown (Pos c h u _ l r) d = Pos c h u (Just d) l r

addLeft :: Position -> Coords -> Position
addLeft (Pos c h u d _ r) l = Pos c h u d (Just l) r

addRight :: Position -> Coords -> Position
addRight (Pos c h u d l _) r = Pos c h u d l (Just r)

findPos :: Coords -> Heightmap -> Maybe Position
findPos c poses = case filter (== Pos c 0 Nothing Nothing Nothing Nothing) poses of
                    []     -> Nothing
                    (x:xs) -> Just x

dist :: Coords -> Coords -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

canConnect :: Coords -> Coords -> Heightmap -> Bool
canConnect c1 c2 poses = case findPos c1 poses of
                            Nothing -> False
                            Just p1 -> case findPos c2 poses of
                                            Nothing -> False
                                            Just p2 -> height p2 >= height p1 - 1 && dist c1 c2 == 1

tryUp :: Heightmap -> State Position ()
tryUp poses = do pos <- get
                 let (x, y) = coords pos
                 if   canConnect (x, y) (x, y - 1) poses
                 then put $ addUp pos (x, y - 1)
                 else put pos

tryDown :: Heightmap -> State Position ()
tryDown poses = do pos <- get
                   let (x, y) = coords pos
                   if   canConnect (x, y) (x, y + 1) poses
                   then put $ addDown pos (x, y + 1)
                   else put pos

tryLeft :: Heightmap -> State Position ()
tryLeft poses = do pos <- get
                   let (x, y) = coords pos
                   if   canConnect (x, y) (x - 1, y) poses
                   then put $ addLeft pos (x - 1, y)
                   else put pos

tryRight :: Heightmap -> State Position ()
tryRight poses = do pos <- get
                    let (x, y) = coords pos
                    if   canConnect (x, y) (x + 1, y) poses
                    then put $ addRight pos (x + 1, y)
                    else put pos

connect :: Heightmap -> State Position ()
connect poses = do pos <- get
                   let (x, y) = coords pos
                   let pos' = execState (tryUp poses) pos
                   let pos'' = execState (tryDown poses) pos'
                   let pos''' = execState (tryLeft poses) pos''
                   let pos'''' = execState (tryRight poses) pos'''
                   put pos''''

makeConnections :: State Heightmap ()
makeConnections = do poses <- get
                     let poses' = map (execState (connect poses)) poses
                     put poses'

closestHeight :: Int -> [Coords] -> [Coords] -> Heightmap -> Int
closestHeight _ [] _ _ = error "no path"
closestHeight target at visited poses
    | target `elem` map (\x -> let Just p = findPos x poses in height p) at = 0
    | otherwise = 1 + closestHeight target next visited poses
        where
            next = unique $ filter (\pos -> pos `notElem` visited && pos `notElem` at) $ concatMap ((\p -> let Just result = sequence $ filter (/=Nothing) [up p, down p, left p, right p] in result) . (\x -> let Just p = findPos x poses in p)) at
            nextVisited = visited ++ at

main :: IO ()
main = do input <- readFile "day12/input.txt"
          let rawPoses = concat [[(x, y, c) | (x, c) <- zip [1..] row] | (y, row) <- zip [1..] (lines input)]
          let startCoords = (\(a, b, _) -> (a, b)) $ head $ filter (\(_, _, c) -> c == 'S') rawPoses :: Coords
          let endCoords = (\(a, b, _) -> (a, b)) $ head $ filter (\(_, _, c) -> c == 'E') rawPoses :: Coords
          let poses = execState makeConnections (map (\(x, y, c) -> createPos (x, y) (charHeight c)) rawPoses) :: Heightmap
          print $ closestHeight (charHeight 'a') [endCoords] [] poses

charHeight :: Char -> Int
charHeight 'S' = charHeight 'a'
charHeight 'E' = charHeight 'z'
charHeight c   = ord c