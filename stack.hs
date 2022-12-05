{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWithM" #-}
module Stack where

import Control.Monad.State (StateT, MonadState(get, put))

type Stack a = [a]
type StackT a = StateT (Stack a) Maybe

push :: a -> StackT a ()
push elem = do st <- get
               put $ elem : st

pushList :: [a] -> StackT a [()]
pushList xs = sequence $ zipWith ($) pushes xs
    where
        pushes = push : pushes

pop :: StackT a a
pop = do xs <- get
         case xs of
            [] -> fail "Empty Stack"
            _  -> do put $ init xs
                     return $ last xs

peek :: Stack a -> Maybe a
peek xs
    | null xs   = Nothing
    | otherwise = Just $ last xs

empty :: Stack a -> Bool
empty = null