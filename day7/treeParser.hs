module Day7.TreeParser where

import Utility.Parse (nat, newline, parse, sat, stringP, ws, Parser)
import Control.Applicative (Alternative((<|>), some))
import Data.Char (isSpace)
import Control.Monad.State (MonadState(put, get), execState, State)

data Tree = File Int String | Folder String [Tree]
    deriving (Eq, Show)

isFolder :: Tree -> Bool
isFolder (File _ _)   = False
isFolder (Folder _ _) = True

name :: Tree -> String
name (File _ n)   = n
name (Folder n _) = n

size :: Tree -> Int
size (File s _) = s
size (Folder _ xs) = sum $ map size xs

type TreeT = State Tree
type Path = [String]
type Pose = (Path, Tree)
type PoseT = State Pose

addSubtree :: Path -> Tree -> TreeT ()
addSubtree [] sub = do t <- get
                       case t of
                            Folder n subs -> do let subs' = sub : subs
                                                put $ Folder n subs'
                            File _ _      -> undefined
addSubtree (x:xs) sub = do t <- get
                           case t of
                                Folder n subs -> do let sub' = execState (addSubtree xs sub) (head (filter (\s -> name s == x && isFolder s) subs))
                                                    put $ Folder n (sub' : filter (\s -> name s /= x || not (isFolder s)) subs)
                                File _ _      -> undefined

lineToPoseT :: Line -> PoseT ()
lineToPoseT (Cd "..") = do (path, t) <- get
                           put (init path, t)
lineToPoseT (Cd next) = do (path, t) <- get
                           put (path ++ [next], t)
lineToPoseT (F s n) = do (path, t) <- get
                         let t' = execState (addSubtree path (File s n)) t
                         put (path, t')
lineToPoseT (Dir n) = do (path, t) <- get
                         let t' = execState (addSubtree path (Folder n [])) t
                         put (path, t')
lineToPoseT Ls = do pose <- get
                    put pose

linesToTree :: [Line] -> Tree
linesToTree ls = let (_, t) = execState (mapM lineToPoseT ls) ([], Folder "/" [])
                 in  t

data Line = Cd String | Ls | F Int String | Dir String
    deriving (Eq, Show)

nameP :: Parser String
nameP = some $ sat $ not . isSpace

cdP :: Parser Line
cdP = do stringP "$ cd "
         Cd <$> nameP

lsP :: Parser Line
lsP = do stringP "$ ls"
         return Ls

fP :: Parser Line
fP = do s <- nat
        ws
        F s <$> nameP

dirP :: Parser Line
dirP = do stringP "dir "
          Dir <$> nameP

lineP :: Parser Line
lineP = cdP <|> lsP <|> fP <|> dirP

linesP :: Parser [Line]
linesP = some (do line <- lineP
                  newline
                  return line
                <|> lineP)

parseTree :: IO Tree
parseTree = do input <- readFile "day7/input.txt"
               let [(lines, input')] = parse linesP input
               let neededLines = tail $ filter (/= Ls) lines
               return $ linesToTree neededLines