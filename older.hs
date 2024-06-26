{-# LANGUAGE MultiWayIf #-}
import Data.Char

data Tree a = Nil 
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

contextToWordList :: String -> [String]
contextToWordList ctx = aux [] "" ctx
    where aux :: [String] -- current word list
              -> String -- current word
              -> String -- context
              -> [String] -- result
          aux ws w "" = 
              if | w == "" -> ws
                 | otherwise -> reverse w : ws
          aux ws w (x:xs) = 
              let x' = toLower x
               in if | x' `elem` s -> 
                      if | w == "" -> aux ws w xs
                         | otherwise -> aux (reverse w : ws) "" xs
                     | otherwise -> aux ws (x':w) xs
              where s = [' ', ',', '.', '!', '?', ';', ':', '\n', '/', '\\', '-', '_', '"', '(', ')', '[', ']', '{', '}', '<', '>', '&', '~', '|', '#', '@', '`']

inorder :: Tree a -> [a]
inorder Nil = []
inorder (Node v l r) = inorder l ++ [v] ++ inorder r

qsort :: [(String, Int)] -> [(String, Int)]
qsort [] = []
qsort (x@(w,n):xs) = qsort (filter (\y -> snd y > n) xs) 
                  ++ [x] 
                  ++ qsort (filter (\y -> snd y <= n) xs)

insertToTree :: String -> Tree (String, Int) -> Tree (String, Int)
insertToTree x Nil = Node (x, 1) Nil Nil
insertToTree x (Node v@(w, n) l r) = 
    if | x == w -> Node (w, n + 1) l r
       | x < w -> Node v (insertToTree x l) r
       | x > w -> Node v l (insertToTree x r)

wordListToTree :: [String] -> Tree (String, Int)
wordListToTree ws = aux Nil ws
    where aux :: Tree (String, Int) -- current tree
              -> [String] -- current word list
              -> Tree (String, Int) -- result
          aux t [] = t
          aux t (w:ws) = aux (insertToTree w t) ws

treeToLookupTable :: Tree (String, Int) -> [(String, Int)]
treeToLookupTable = qsort . inorder

contextToSortedLookupTable :: String -> [(String, Int)]
contextToSortedLookupTable = treeToLookupTable . wordListToTree . contextToWordList

formatOutput :: (String, Int) -> IO ()
formatOutput (w, n) = let l = length w
                       in putStrLn $ w ++ replicate (15 - l) ' ' ++ show n 

main :: IO ()
main = do
    ctx <- getContents
    let r = contextToSortedLookupTable ctx
    let g = "\n" ++ "word" ++ replicate 11 ' ' ++ "times\n" ++ replicate 20 '-'
    putStrLn g
    mapM_ formatOutput r
