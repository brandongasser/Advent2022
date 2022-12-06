{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parse where
import Control.Applicative
import Data.Char (isDigit, isSpace)

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

instance Functor Parser where
    fmap f p = P $ \input -> case parse p input of
                                []            -> []
                                [(x, input')] -> [(f x, input')]

instance Applicative Parser where
    pure x = P $ \input -> [(x, input)]
    pf <*> px = P $ \input -> case parse pf input of
                                []            -> []
                                [(f, input')] -> parse (fmap f px) input'

instance Alternative Parser where
    empty = P $ const []
    pa <|> pb = P $ \input -> case parse pa input of
                                [] -> parse pb input
                                result -> result

instance Monad Parser where
    p >>= f = P $ \input -> case parse p input of
                                [] -> []
                                [(x, input')] -> parse (f x) input'

item :: Parser Char
item = P $ \input -> case input of
                        []     -> []
                        (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if   p x
           then return x
           else empty

charP :: Char -> Parser Char
charP c = sat (==c)

stringP :: String -> Parser String
stringP []     = return []
stringP (x:xs) = do charP x
                    stringP xs
                    return (x:xs)

newline :: Parser Char
newline = charP '\n'

ws :: Parser String
ws = many (sat isSpace)

nat :: Parser Int
nat = do xs <- some (sat isDigit)
         return $ read xs

int :: Parser Int
int = do charP '-'
         n <- nat
         return (-n)
       <|> nat

double :: Parser Double
double = do i <- int
            charP '.'
            d <- nat
            return (read (show i ++ "." ++ show d))
          <|> fromIntegral <$> int