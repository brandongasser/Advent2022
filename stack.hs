{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Stack where

import Control.Monad.State (StateT, MonadState(get, put))
import Control.Monad (replicateM, zipWithM)

type Stack a = [a]
type StackT a = StateT (Stack a) Maybe

{-|
Pushes one element onto the stack.
-}
push :: a -> StackT a ()
push elem = do st <- get
               put $ elem : st

{-|
Pushes all elements in the list onto the stack.
Elements are pushed left to right.
-}
pushList :: [a] -> StackT a [()]
pushList = zipWithM ($) pushes
    where
        pushes = push : pushes

{-|
Removes the top element from the stack and returns it.
-}
pop :: StackT a a
pop = do xs <- get
         case xs of
            [] -> fail "Empty Stack"
            _  -> do put $ tail xs
                     return $ head xs

{-|
Removes the top n elements from the stack and returns them.
The first element in the list is the first element popped.
-}
popN :: Int -> StackT a [a]
popN n = replicateM n pop

{-|
Returns the top element on the stack without removing it.
-}
peek :: Stack a -> Maybe a
peek xs
    | null xs   = Nothing
    | otherwise = Just $ last xs

{-|
Returns True if the stack has no elements, otherwise False.
-}
empty :: Stack a -> Bool
empty = null