module Graph
  exposing
    ( Graph
      -- query
    , getData
    , member
    , memberEdge
    , incoming
    , outgoing
    , size
    , keys
    , nodes
    , edges
      -- build
    , empty
    , insertNode
    , insertNodeData
    , insertEdge
    , removeNode
    , removeEdge
      -- transformation
    , map
    , foldl
    , foldr
      -- set operations
    , partition
    , union
    , intersect
      -- graph traversal
    , topologicalSort
    , postOrder
    , isAcyclic
    )

{-|

A simple functional graph library. Keys used to identify nodes can be any `comparable` and nodes can have any kind of metadata associated with them.

All operations that look at a single node are at most `O(log n)`.
Operations that look at all elements in the graph are at most `O(n log n)`.

# Types
@docs Graph

# Build
@docs empty, insertNode, insertNodeData, insertEdge, removeNode, removeEdge

# Query
@docs getData, member, memberEdge, incoming, outgoing, size, keys, nodes, edges, isAcyclic

# Transform
@docs map, foldl, foldr

# Set operations
@docs partition, union, intersect

# Algorithms and Traversal
@docs postOrder, topologicalSort


-}

import Graph.Internal as I
  exposing
    ( Graph
      -- build
    , empty
    , insertNode
    , insertNodeData
    , insertEdge
    , removeNode
    , removeEdge
      -- query
    , getData
    , member
    , memberEdge
    , incoming
    , outgoing
    , size
    , keys
    , nodes
    , edges
      -- transformation
    , map
    , foldl
    , foldr
      -- set operations
    , partition
    , union
    , intersect
      -- graph traversal
    , topologicalSort
    , postOrder
    , isAcyclic
      -- debugging
    )
import Set exposing (Set)


{-| A directed graph. `Graph Int String` is a graph that uses `Int`s for identifying its nodes, and lets you store a `String` on each node.
-}
type alias Graph comparable data =
  I.Graph comparable data


-- NODE


{-| Get the data associated with a specific node.
-}
getData : comparable -> I.Graph comparable data -> Maybe data
getData key graph =
  I.getData key graph


{-| Get the set of incoming edges to a node.
-}
incoming : comparable -> I.Graph comparable data -> Set comparable
incoming key graph =
  I.incoming key graph


{-| Get the set of outgoing edges from a node.
-}
outgoing : comparable -> I.Graph comparable data -> Set comparable
outgoing key graph =
  I.outgoing key graph


-- BUILD


{-| Create an empty graph.
-}
empty : I.Graph comparable data
empty =
  I.empty


{-| Insert a node. Does not overwrite metadata if node already exists.
-}
insertNode : comparable -> I.Graph comparable data -> I.Graph comparable data
insertNode key graph =
  I.insertNode key graph


{-| Update metadata for a node. Creates the node if it does not already exist.
-}
insertNodeData : comparable -> data -> I.Graph comparable data -> I.Graph comparable data
insertNodeData key data graph =
  I.insertNodeData key data graph


{-| Insert an edge between two nodes. Creates any nodes that do not already exist.
-}
insertEdge : ( comparable, comparable ) -> I.Graph comparable data -> I.Graph comparable data
insertEdge ( from, to ) graph =
  I.insertEdge ( from, to ) graph


{-| Remove a node by its key. No-op if node doesn't exist.
-}
removeNode : comparable -> I.Graph comparable data -> I.Graph comparable data
removeNode key graph =
  I.removeNode key graph


{-| Remove an edge identified by its source and target keys. No-op if source, target or edge doesn't exist.
-}
removeEdge : ( comparable, comparable ) -> I.Graph comparable data -> I.Graph comparable data
removeEdge ( from, to ) graph =
  I.removeEdge ( from, to ) graph


-- QUERY


{-| Determine the number of nodes in the graph.
-}
size : I.Graph comparable data -> Int
size graph =
  I.size graph


{-| Determine if a node identified by a key is in the graph.
-}
member : comparable -> I.Graph comparable data -> Bool
member key graph =
  I.member key graph


{-| Determine if an edge identified by a pair of keys is in the graph.
-}
memberEdge : ( comparable, comparable ) -> I.Graph comparable data -> Bool
memberEdge ( from, to ) graph =
  I.memberEdge ( from, to ) graph


{-| Get the (key, data) pair for each node in the graph.
-}
nodes : I.Graph comparable data -> List ( comparable, Maybe data )
nodes =
  I.nodes


{-| Get the keys for all nodes in the graph.
-}
keys : I.Graph comparable data -> List comparable
keys graph =
  I.keys graph


{-| Get the (from, to) pair for each edge in the graph.
-}
edges : I.Graph comparable data -> List ( comparable, comparable )
edges graph =
  I.edges graph


{-| Determine if a graph contains any loops or cycles.
-}
isAcyclic : I.Graph comparable data -> Bool
isAcyclic graph =
  I.isAcyclic graph


-- TRANSFORM


{-| Apply a function to the data associated with each node in a graph.
-}
map : (comparable -> Maybe data1 -> Maybe data2) -> I.Graph comparable data1 -> I.Graph comparable data2
map func graph =
  I.map func graph


{-| Fold over the node keys and data in a graph, in order from lowest `key` to highest `key`.
-}
foldl :
  (comparable -> Maybe data -> a -> a)
  -> a
  -> I.Graph comparable data
  -> a
foldl func acc graph =
  I.foldl func acc graph


{-| Fold over the node keys and data in a graph, in order from highest `key` to lowest `key`.
-}
foldr :
  (comparable -> Maybe data -> a -> a)
  -> a
  -> I.Graph comparable data
  -> a
foldr func acc graph =
  I.foldr func acc graph


-- SET OPERATIONS


{-| Partition a graph into two parts, one with the nodes where the predicate function returned `True`, and one where it returned `False`.
-}
partition : (comparable -> Maybe data -> Bool) -> I.Graph comparable data -> ( I.Graph comparable data, I.Graph comparable data )
partition func graph =
  I.partition func graph


{-| Join two graphs together. If an edge appears between two nodes in either of the graphs, it will be in the resulting graph. If a node identified by a specific key appears in any of the graphs, it will be in the resulting graph. If both graphs have metadata for the same key, the metadata in the left graph will be used.
-}
union : I.Graph comparable data -> I.Graph comparable data -> I.Graph comparable data
union a b =
  I.union a b


{-| Create a graph based on the intersection of two graphs. If both graphs have the same node, edge or associated metadata, it will be in the resulting graph. If one graph has metadata and the other only has the node, the node will be in the resulting graph, but the metadata will not.
-}
intersect : I.Graph comparable data -> I.Graph comparable data -> I.Graph comparable data
intersect a b =
  I.intersect a b


-- TRAVERSAL


{-| Get a topological sorting of the graph, if the graph doesn't contain any loops or cycles.
-}
topologicalSort : I.Graph comparable data -> Maybe (List comparable)
topologicalSort graph =
  I.topologicalSort graph


{-| Get a list of all keys according to a postorder traversal of a spanning tree of the graph.
-}
postOrder : I.Graph comparable data -> List comparable
postOrder graph =
  I.postOrder graph
