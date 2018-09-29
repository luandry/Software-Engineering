import BTree
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do

 describe "Binary Tree Depth Tester" $ do
  it "Create tree node" $ do
   let t = Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf Leaf)
   (treeDepth t) `shouldBe` 2


 describe "Binary Tree is Sorted Tester" $ do
  it "Create tree node" $ do
    isSortedTree (Node 5 (Node 4 Leaf Leaf) (Node 6 Leaf Leaf)) minBound maxBound `shouldBe` True


 

