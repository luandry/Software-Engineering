import BTree
import Data.Graph.DAG.Edge
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do

 describe "Binary Tree Depth Tester" $ do
  it "Depth is Functioning" $ do
   let t = Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf Leaf)
   (treeDepth t) `shouldBe` 2


 describe "Binary Tree is Sorted Tester" $ do
  it "Is Sorted is Functioning" $ do
    isSortedTree (Node 5 (Node 4 Leaf Leaf) (Node 6 Leaf Leaf)) minBound maxBound `shouldBe` True


 describe "Binary Tree Tree Sum Tester" $ do
  it "Binary Tree Sum is Functioning" $ do
   let t = Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf Leaf)
   (treeSum t) `shouldBe` 9

 describe "Add New Max to Binary Tree Tester" $ do
  it "The Add New Max is Functioning" $ do
   let t = Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf Leaf)
   let u = (addNewMax t)
   (treeMax u) `shouldBe` 5

 describe "Find the Lowest Common Ancestor of two Nodes in the Binary Tree" $ do
  it "LCA is Functioning" $ do 
   let t = Node 12 (Node 10 (Node 8 Leaf Leaf)(Node 11 Leaf Leaf))(Node 14 Leaf Leaf)
   (lca 8 11 t) `shouldBe` 10

 


 

