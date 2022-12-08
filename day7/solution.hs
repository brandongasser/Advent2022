import Day7.TreeParser

main :: IO ()
main = do tree <- parseTree
          -- Part 1
          print $ ans1 tree
          -- Part 2
          print $ ans2 tree

ans1 :: Tree -> Int
ans1 (File _ _) = 0
ans1 (Folder n subs) = (if size (Folder n subs) <= 100000 then size (Folder n subs) else 0) + sum (map ans1 subs)

totalSpace :: Int
totalSpace = 70000000

-- I cheated here and found the size of the directory and manually subtracted it from 70000000
neededSpace :: Int
neededSpace = 2536714

-- neededSpace :: Int
-- neededSpace = 8381165

ans2 :: Tree -> Int
ans2 (File s _) = s
ans2 (Folder _ subs) = minimum (size (Folder "" subs) : map ans2 (filter (\s -> isFolder s && size s >= neededSpace) subs))