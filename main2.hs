import Data.List as L
import Data.Map as M
import Data.Char (toLower)

data Trie = Node Int (M.Map Char Trie)
    deriving Show

insertToTrie :: String -> Trie -> Trie
insertToTrie "" (Node i m) = Node (i + 1) m
insertToTrie (x:xs) (Node i m) = case M.lookup x m of 
                                   Nothing -> Node i $ M.insert x (insertToTrie xs $ Node 0 M.empty) m
                                   Just n -> Node i $ M.insert x (insertToTrie xs n) m

contextToTrie :: String -> Trie
contextToTrie = aux (Node 0 M.empty)
    where s = [' ', ',', '.', '!', '?', ';', ':', '\n', '/', '\\', '-', '_', '"', '(', ')', '[', ']', '{', '}', '<', '>', '&', '~', '|', '#', '@', '`'] 
          popWord y "" = (reverse y, "")
          popWord y (x:xs) = if x `elem` s then (reverse y, xs)
                                           else popWord (toLower x : y) xs
          aux t "" = t
          aux t s = let (x, s') = popWord "" s
                     in if L.null x then aux t s'
                                    else aux (insertToTrie x t) s'

trieToDict :: Trie -> [(String, Int)]
trieToDict (Node i m) = let ks = M.keys m
                            acc = [("", i) | i > 0]
                         in L.foldr (\k a -> let (Just t) = M.lookup k m in a ++ L.map (\(s, i') -> (k:s, i')) (trieToDict t)) acc ks

qsort :: [(String, Int)] -> [(String, Int)]
qsort [] = []
qsort (x@(w,n):xs) = qsort (L.filter (\y -> snd y > n) xs) 
                  ++ [x] 
                  ++ qsort (L.filter (\y -> snd y <= n) xs)

formatOutput :: (String, Int) -> IO ()
formatOutput (w, n) = let l = length w
                       in putStrLn $ w ++ replicate (15 - l) ' ' ++ show n 

main :: IO ()
main = do
    ctx <- getContents
    let r = qsort $ trieToDict $ contextToTrie ctx
    let g = "\n" ++ "word" ++ replicate 11 ' ' ++ "times\n" ++ replicate 20 '-'
    putStrLn g
    mapM_ formatOutput r
