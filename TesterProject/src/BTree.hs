
module BTree where
--import Control.Applicative
import DAG

edges = 
 ECons(Edge :: EdgeValue "foo" "bar") $
 ECons(Edge :: EdgeValue "bar" "baz") $
 ECons(Edge :: EdgeValue "foo" "baz") $
 unique -- ENil, but casted for uniquely edged graphs

data Cool = AllRight
 | Radical
 | Superduper

nodes = 
 nadd "foo" AllRight $
 nadd "bar" Radical $
 nadd "baz" Superduper $
 nempty


graph = DAG edges nodes
