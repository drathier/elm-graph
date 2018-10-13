# elm-graph [![Build Status](https://travis-ci.com/drathier/elm-graph.svg?token=z813a3NqyNRAhrQwc49e&branch=master)](https://travis-ci.com/drathier/elm-graph)
Elm-graph is a simple library for mathematical graphs (nodes and edges). 

Keys used to identify nodes can be any `comparable`, and both nodes and edges can have any kind of metadata associated with them.

All operations that look at a single node are at most `O(log n)`. Operations that look at all elements in the graph are at most `O(n log n)`.

InsertEdge functions insert missing nodes as needed. 
`Data` suffix insert functions overwrite metadata. Insert functions without `Data` suffix clear metadata.


```
example =
    let
        graph =
            empty
                |> insertEdge 5 1 -- automatically creates missing nodes
                |> insertEdge 1 2
                |> insertEdge 3 2
                |> insertEdge 5 2
                |> insertEdge 1 3
                |> insertEdge 2 3
                |> insertEdge 2 4
                |> insertEdge 3 5
                |> insertEdge 4 5
                |> insert 42 -- explicitly create node without edges or metadata
                |> insertData 42 "node metadata" -- overwrite node, replacing metadata (if there is any)
                |> insertEdgeData 42 55 ("metadata", "for", "edge")
    in 
    incoming 2 graph -- [ 1, 3, 5 ]
```
