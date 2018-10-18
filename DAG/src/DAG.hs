module DAG where

import Data.Graph.Types
import Data.Graph.DGraph
import Data.Graph.Connectivity


myGraph :: DGraph Int ()
myGraph = fromArcsList
    [ 1 --> 4
    , 1 --> 5
    , 1 --> 9
    , 2 --> 4
    , 2 --> 6
    , 3 --> 5
    , 3 --> 8
    , 3 --> 10
    , 4 --> 5
    , 4 --> 10
    , 5 --> 8
    , 6 --> 8
    , 6 --> 9
    , 7 --> 8
    ]