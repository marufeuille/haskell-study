import Data.List

data Tree a = EmptyNode | Node a (Tree a) (Tree a) deriving (Show, Eq)

val :: Tree a -> a
val (Node v _ _) = v

countElm :: (Ord a) => [a] -> [(a, Int)]
countElm x = map (\(x:xs) -> (x, length (x:xs))) (group . sort $ x)

tuplesToTreeNode :: [(a, Int)] -> [Tree (a, Int)]
tuplesToTreeNode = map (\x -> Node x EmptyNode EmptyNode)

sortNodes :: [Tree (a, Int)] -> [Tree (a, Int)]
sortNodes = sortBy (\x -> compare (snd (val x)) . snd . val)

huffmanCoding :: [Tree (a, Int)] -> Tree (a, Int)
huffmanCoding [x0] = x0
huffmanCoding (x0:x1:xs)
   | null xs = newNode
   | otherwise = huffmanCoding nodes
   where
       newNode = Node (fst (val x0), snd (val x0) + snd (val x1)) x0 x1
       nodes = sortNodes (newNode:xs)
huffmanCoding [] = EmptyNode

getCodes :: String -> Tree (a, Int) -> [(a, String)]
getCodes _ EmptyNode = []
getCodes current (Node v EmptyNode EmptyNode) = [(fst v, current)]
getCodes current (Node v leftNode rightNode) = getCodes (current ++ "0") leftNode ++ getCodes (current ++ "1") rightNode