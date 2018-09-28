import BTree
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do

 describe "Binary Tree functionality testing." $ do
  it "Create tree node" $ do
   let t = Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf Leaf)
   (treeDepth t) `shouldBe` 2