
module BTree where
import Control.Applicative

data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
 1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal = 
 let leftSorted = isSortedTree leftSubtree minVal x
     rightSorted = isSortedTree rightSubtree x maxVal
 in x >= minVal && x < maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node x leftSubtree rightSubtree) = 
 x + (treeSum leftSubtree) + (treeSum rightSubtree)

treeMax :: Tree -> Int 
treeMax Leaf = 0
treeMax (Node x leftSubtree rightSubtree) = 
 maximum [x, treeMax leftSubtree, treeMax rightSubtree]

--has :: Int -> Tree -> Bool
--has _ Leaf = False
--has e (Node x leftSubtree rightSubtree) = x == e || has e leftSubtree || has e rightSubtree 

--lca :: Int -> Int -> Tree -> Maybe Int
--lca _ _ Leaf = Nothing
--lca m n (Node x leftSubtree rightSubtree) | has m (Node x leftSubtree Leaf) && has n (Node x Leaf rightSubtree) = Just x
--                                          | otherwise = lca m n leftSubtree <|> lca m n rightSubtree
lca :: Int -> Int -> Tree -> Int
lca m n ~(Node x leftSubtree rightSubtree) | n < x     = lca m n leftSubtree
                      					   | m > x     = lca m n rightSubtree
                      					   | otherwise = x
