
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


--has :: Eq a => a -> Tree a -> Bool
--has _ Leaf = False
--has e (Node v leftSubtree rightSubtree) = v == e || has e leftSubtree || has e rightSubtree 

--lca :: Ord a => a -> a -> Tree a -> Maybe a
--lca _ _ Leaf = Nothing
--lca m n (Node v leftSubtree rightSubtree) | has m (Node v leftSubtree Nil) && has n (Node v Leaf rightSubtree) = Just v
--                                          | otherwise = lca m n leftSubtree <|> lca m n rightSubtree
