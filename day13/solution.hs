{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Utility.Parse
import Control.Applicative
import Data.Maybe (fromJust)

data Packet = Val Int | Ls [Packet]
    deriving (Show, Eq)

type PacketPair = (Packet, Packet)

valP :: Parser Packet
valP = Val <$> int

packetP :: Parser Packet
packetP = do charP '['
             items <- many (do item <- packetP <|> valP
                               charP ','
                               return item
                             <|> packetP <|> valP)
             charP ']'
             return $ Ls items

pairP :: Parser PacketPair
pairP = do p1 <- packetP
           newline
           p2 <- packetP
           return (p1, p2)

pairsP :: Parser [PacketPair]
pairsP = many (do p <- pairP
                  newline
                  newline
                  return p
                <|> pairP)

-- inOrder :: PacketPair -> Bool
-- inOrder (Ls [], _) = True
-- inOrder (_, Ls []) = False
-- inOrder (Ls (Val x:xs), Ls (Val y:ys)) = if x > y
--                                          then False
--                                          else if x < y
--                                               then True
--                                               else inOrder (Ls xs, Ls ys)
-- inOrder (Ls (Ls xs':xs), Ls (Ls ys':ys)) = inOrder (Ls xs', Ls ys') && inOrder (Ls xs, Ls ys)
-- inOrder (Ls (Val x:xs), (Ls (Ls ys':ys))) = inOrder (Ls (Ls [Val x]:xs), Ls (Ls ys':ys))
-- inOrder (Ls (Ls xs':xs), (Ls (Val y:ys))) = inOrder (Ls (Ls xs':xs), Ls (Ls [Val y]:ys))

inOrder :: PacketPair -> Maybe Bool
inOrder (Ls [], Ls []) = Nothing
inOrder (Ls [], _) = Just True
inOrder (_, Ls []) = Just False
inOrder (Ls (x:xs), Ls (y:ys)) = case x of
                                    Val a -> case y of
                                                Val b -> if a < b
                                                         then Just True
                                                         else if a > b
                                                              then Just False
                                                              else inOrder (Ls xs, Ls ys)
                                                Ls bs -> case inOrder (Ls [Val a], Ls bs) of
                                                                Nothing -> inOrder (Ls xs, Ls ys)
                                                                Just result -> Just result
                                    Ls as -> case y of
                                                Val b -> case inOrder (Ls as, Ls [Val b]) of
                                                                Nothing -> inOrder (Ls xs, Ls ys)
                                                                Just result -> Just result
                                                Ls bs -> case inOrder (Ls as, Ls bs) of
                                                                Nothing -> inOrder (Ls xs, Ls ys)
                                                                Just result -> Just result

sortPackets :: [Packet] -> [Packet]
sortPackets [] = []
sortPackets (x:xs) = left ++ [x] ++ right
    where
        left = sortPackets [p | p <- xs, fromJust $ inOrder (p, x)]
        right = sortPackets [p | p <- xs, fromJust $ inOrder (x, p)]

decoderKey :: [Packet] -> Int
decoderKey packets = product $ map fst dividers
    where
        dividers = filter (isDivider . snd) $ zip [1..] packets
        isDivider x = x == Ls [Ls [Val 2]] || x == Ls [Ls [Val 6]]

main :: IO ()
main = do input <- readFile "day13/input.txt"
          let [(pairs, _)] = parse pairsP input
          let indexed = zip [1..] pairs
          -- Part 1
          let good = filter (fromJust . inOrder . snd) indexed
          print $ sum $ map fst good
          -- Part 2
          let packets = concatMap (\(x, y) -> [x, y]) (pairs ++ [(Ls [Ls [Val 2]], Ls [Ls [Val 6]])])
          let sortedPackets = sortPackets packets
          print $ decoderKey sortedPackets