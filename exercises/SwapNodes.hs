module SwapNodes where
import Data.List (nub)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

buildTree :: [[Int]] -> Tree Int
buildTree xs = buildTree' xs 1
  where 
    buildTree' :: [[Int]] -> Int -> Tree Int
    buildTree' xs i
      | i == -1 = Empty
      | otherwise =
          Node
            i
            (buildTree' xs (head (xs !! (i - 1))))
            (buildTree' xs (xs !! (i - 1) !! 1))
            

swapTree :: Tree a -> [Int] -> [Tree a]
swapTree _ [] = []
swapTree tree (x:xs) = tree' : swapTree tree' xs
  where
    tree' = swap tree (x : xs)


swap :: Tree a -> [Int] -> Tree a
swap tree [] = tree
swap tree depths = swap' tree (nub depths)
  where
    swap' tree' [] = tree'
    swap' tree' (d:ds) = swap' (swapAtDepth tree' d) ds

    swapAtDepth Empty _ = Empty
    swapAtDepth (Node v left right) d
      | d <= 1 = Node v right left
      | otherwise = Node v (swapAtDepth left (d - 1)) (swapAtDepth right (d - 1))


inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node val Empty Empty) = [val]
inorder (Node val Empty right) = [val] ++ inorder right
inorder (Node val left Empty) = inorder left ++ [val]
inorder (Node val left right) = inorder left ++ [val] ++ inorder right

swapNodesMain :: [[Int]] -> [Int] -> [[Int]]
swapNodesMain xs swaps = map inorder swappedTrees
  where
    initialTree :: Tree Int
    initialTree = buildTree xs

    swappedTrees :: [Tree Int]
    swappedTrees = swapTree initialTree swaps

-- Arboles creados:
tree1 :: Tree Int
tree1 = buildTree [[2, 3], [(-1), (-1)], [(-1), (-1)]]

resultTree1 :: [[Int]]
resultTree1 = swapNodesMain [[2, 3], [(-1), (-1)], [(-1), (-1)]] [1,1]

-- ======================================================================

tree2 :: Tree Int
tree2 = buildTree [[2, 3], [(-1), 4], [(-1), 5], [(-1), (-1)], [(-1), (-1)]]

resultTree2 :: [[Int]]
resultTree2 = swapNodesMain [[2, 3], [(-1), 4], [(-1), 5], [(-1), (-1)], [(-1), (-1)]] [2]

-- ======================================================================

tree3 :: Tree Int
tree3 = buildTree [[2, 3], [4, (-1)], [5, (-1)], [6, (-1)], [7, 8], [(-1), 9], [(-1), (-1)], [10, 11], [(-1), (-1)], [(-1), (-1)], [(-1), (-1)]]

resultTree3 :: [[Int]]
resultTree3 = swapNodesMain [[2, 3], [4, (-1)], [5, (-1)], [6, (-1)], [7, 8], [(-1), 9], [(-1), (-1)], [10, 11], [(-1), (-1)], [(-1), (-1)], [(-1), (-1)]] [2,4]
