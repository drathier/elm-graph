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

# Graphs
@docs Graph

# Query
@docs getData, member, memberEdge, incoming, outgoing, size, nodes, edges

# Build
@docs empty, insertNode, insertNodeData, insertEdge, removeNode, removeEdge

# Transform
@docs map, foldl, foldr

# Combine
@docs partition, union, intersect

# Algorithms and Traversal
@docs topologicalSort

-}

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Tuple


{-

   It is theoretically possible to get O(1) path traversals without enforcing a DAG or violating immutability, but that would make all inserts and modifications at least O(n), because all nodes reference all other nodes (except if the graph is disjoint). If I figure out a way to do the conversion in in linear time, I can add that later.

-}


{-| A directed graph. `(Graph Int String) is a graph that uses `Int`s for identifying its nodes, and lets you store a `String` on each node.
-}
type Graph comparable a
  = Graph { nodes : Dict comparable (Node comparable a) }


-- NOTE: type system doesn't help differentiate between incoming and outgoing edges
-- NODE
-- TODO: are all exposed functions tested?


type Node comparable data
  = Node
      { data : Maybe data
      , incoming : Set comparable
      , outgoing : Set comparable
      }


{-| Get the data associated with a specific node.
-}
getData : comparable -> Graph comparable data -> Maybe data
getData key graph =
  get key graph |> Maybe.map (\(Node node) -> node.data) |> Maybe.Extra.join


{-| Get the set of incoming edges to a node.
-}
incoming : comparable -> Graph comparable data -> Set comparable
incoming key graph =
  get key graph |> Maybe.map (\(Node node) -> node.incoming) |> Maybe.withDefault Set.empty


{-| Get the set of outgoing edges from a node.
-}
outgoing : comparable -> Graph comparable data -> Set comparable
outgoing key graph =
  get key graph |> Maybe.map (\(Node node) -> node.outgoing) |> Maybe.withDefault Set.empty


-- NODE HELPERS


node : Node comparable data
node =
  Node
    { data = Nothing
    , incoming = Set.empty
    , outgoing = Set.empty
    }


nodeData : data -> Node comparable data
nodeData data =
  Node
    { data = Just data
    , incoming = Set.empty
    , outgoing = Set.empty
    }


insert : comparable -> Node comparable data -> Graph comparable data -> Graph comparable data
insert key node (Graph graph) =
  Graph { graph | nodes = Dict.insert key node graph.nodes }


-- BUILD


{-| Create an empty graph.
-}
empty : Graph comparable data
empty =
  Graph { nodes = Dict.empty }


{-| Insert a node. Does not overwrite metadata if node already exists.
-}
insertNode : comparable -> Graph comparable data -> Graph comparable data
insertNode key graph =
  insert key (getOrCreate key graph) graph


{-| Update metadata for a node. Creates the node if it does not already exist.
-}
insertNodeData : comparable -> data -> Graph comparable data -> Graph comparable data
insertNodeData key data (Graph graph) =
  Graph { graph | nodes = Dict.insert key (nodeData data) graph.nodes }


{-| Insert an edge between two nodes. Creates any nodes that do not already exist.
-}
insertEdge : ( comparable, comparable ) -> Graph comparable data -> Graph comparable data
insertEdge ( from, to ) graph =
  let
    (Node fromNode) =
      getOrCreate from graph

    (Node toNode) =
      getOrCreate to graph
  in
    if from == to then
      -- self-edge, fill both incoming and outgoing
      graph
        |> insert from
            (Node
              { fromNode
                | incoming = Set.insert from fromNode.incoming
                , outgoing = Set.insert from fromNode.outgoing
              }
            )
    else
      graph
        |> insert to (Node { toNode | incoming = Set.insert from toNode.incoming })
        |> insert from (Node { fromNode | outgoing = Set.insert to fromNode.outgoing })


getOrCreate key graph =
  get key graph |> Maybe.withDefault node


{-| Remove a node by its key. No-op if node doesn't exist.
-}
removeNode : comparable -> Graph comparable data -> Graph comparable data
removeNode key (Graph graph) =
  case get key (Graph graph) of
    Nothing ->
      (Graph graph)

    Just (Node node) ->
      let
        incomingEdgesToRemove =
          List.map (\in_ -> ( in_, key )) (Set.toList node.incoming)

        outgoingEdgesToRemove =
          List.map (\out -> ( key, out )) (Set.toList node.outgoing)

        newGraph =
          Graph { graph | nodes = Dict.remove key graph.nodes }
      in
        List.foldl removeEdge newGraph (incomingEdgesToRemove ++ outgoingEdgesToRemove)


{-| Remove an edge identified by its source and target keys. No-op if source, target or edge doesn't exist.
-}
removeEdge : ( comparable, comparable ) -> Graph comparable data -> Graph comparable data
removeEdge ( from, to ) graph =
  let
    updateIncoming =
      \(Node node) -> Node { node | incoming = Set.remove from node.incoming }

    updateOutgoing =
      \(Node node) -> Node { node | outgoing = Set.remove to node.outgoing }
  in
    graph
      |> updateNode updateIncoming to
      |> updateNode updateOutgoing from


-- QUERY


get : comparable -> Graph comparable data -> Maybe (Node comparable data)
get key (Graph graph) =
  Dict.get key graph.nodes


{-| Determine the number of nodes in the graph.
-}
size : Graph comparable data -> Int
size (Graph graph) =
  Dict.size graph.nodes


{-| Determine if a node identified by a key is in the graph.
-}
member : comparable -> Graph comparable data -> Bool
member key (Graph graph) =
  Dict.member key graph.nodes


{-| Determine if an edge identified by a pair of keys is in the graph.
-}
memberEdge : ( comparable, comparable ) -> Graph comparable data -> Bool
memberEdge ( from, to ) graph =
  outgoing from graph
    |> Set.member to


{-| Get the (key, data) pair for each node in the graph.
-}
nodes : Graph comparable data -> List ( comparable, Maybe data )
nodes =
  foldl (\key data list -> ( key, data ) :: list) []


{-| Get the (from, to) pair for each edge in the graph.
-}
edges : Graph comparable data -> List ( comparable, comparable )
edges graph =
  foldl
    (\key data list ->
      (outgoing key graph
        |> Set.toList
        |> List.map (\out -> ( key, out ))
      )
        ++ list
    )
    []
    graph


-- UPDATE


updateNode : (Node comparable data -> Node comparable data) -> comparable -> Graph comparable data -> Graph comparable data
updateNode func key (Graph graph) =
  -- Update a node if it exists, otherwise do nothing.
  case get key (Graph graph) of
    Just node ->
      Graph { graph | nodes = Dict.insert key (func node) graph.nodes }

    Nothing ->
      Graph graph


{-| Apply a function to the data associated with each node in a graph.
-}
map : (comparable -> Maybe data1 -> Maybe data2) -> Graph comparable data1 -> Graph comparable data2
map func (Graph graph) =
  Graph { graph | nodes = Dict.map (\key (Node node) -> Node { node | data = func key node.data }) graph.nodes }


{-| Fold over the node keys and data in a graph, in order from lowest key to highest key.
-}
foldl :
  (comparable -> Maybe data -> a -> a)
  -> a
  -> Graph comparable data
  -> a
foldl func acc (Graph graph) =
  Dict.foldl (\key (Node node) -> func key node.data) acc graph.nodes


{-| Fold over the node keys and data in a graph, in order from highest key to lowest key.
-}
foldr :
  (comparable -> Maybe data -> a -> a)
  -> a
  -> Graph comparable data
  -> a
foldr func acc (Graph graph) =
  Dict.foldr (\key (Node node) -> func key node.data) acc graph.nodes


{-| Partition a graph into two parts, one with the nodes where the predicate function returned True, and one where it returned False.
-}
partition : (comparable -> Maybe data -> Bool) -> Graph comparable data -> ( Graph comparable data, Graph comparable data )
partition func (Graph graph) =
  let
    add key (Node node) ( left, right ) =
      if func key node.data then
        ( Dict.insert key (Node node) left, right )
      else
        ( left, Dict.insert key (Node node) right )
  in
    Dict.foldl add ( Dict.empty, Dict.empty ) graph.nodes
      |> Tuple.mapFirst (\x -> Graph { nodes = x })
      |> Tuple.mapSecond (\x -> Graph { nodes = x })
      |> Tuple.mapFirst cleanup
      |> Tuple.mapSecond cleanup


cleanup : Graph comparable data -> Graph comparable data
cleanup (Graph graph) =
  let
    -- remove edges pointing to non-existent nodes
    removeDeadEdges =
      \key (Node node) ->
        Node
          { node
            | incoming = node.incoming |> Set.filter (\key -> get key (Graph graph) /= Nothing)
            , outgoing = node.outgoing |> Set.filter (\key -> get key (Graph graph) /= Nothing)
          }
  in
    Graph { graph | nodes = Dict.map removeDeadEdges graph.nodes }


-- COMBINE


{-| Join two graphs together. If an edge appears between two nodes in either of the graphs, it will be in the resulting graph. If a node identified by a specific key appears in any of the graphs, it will be in the resulting graph. If both graphs have metadata for the same key, the metadata in the left graph will be used.
-}
union : Graph comparable data -> Graph comparable data -> Graph comparable data
union (Graph a) (Graph b) =
  Graph
    { nodes =
        Dict.merge
          (\key node dict -> Dict.insert key node dict)
          (\key (Node n1) (Node n2) dict ->
            Dict.insert key
              (Node
                { data = Maybe.Extra.or n1.data n2.data
                , incoming = Set.union n1.incoming n2.incoming
                , outgoing = Set.union n1.outgoing n2.outgoing
                }
              )
              dict
          )
          (\key node dict -> Dict.insert key node dict)
          a.nodes
          b.nodes
          Dict.empty
    }
    |> cleanup


{-| Create a graph based on the intersection of two graphs. If both graphs have the same node, edge or associated metadata, it will be in the resulting graph.
-}
intersect : Graph comparable data -> Graph comparable data -> Graph comparable data
intersect (Graph a) (Graph b) =
  Graph
    { nodes =
        Dict.merge
          (\key node dict -> dict)
          (\key (Node n1) (Node n2) dict ->
            Dict.insert key
              (Node
                { data =
                    if n1.data == n2.data then
                      n1.data
                    else
                      Nothing
                , incoming = Set.intersect n1.incoming n2.incoming
                , outgoing = Set.intersect n1.outgoing n2.outgoing
                }
              )
              dict
          )
          (\key node dict -> dict)
          a.nodes
          b.nodes
          Dict.empty
    }
    |> cleanup


-- OTHER


isAcyclic : Graph comparable data -> Bool
isAcyclic graph =
  isAcyclicHelper (List.reverse <| reversePostOrder graph) Set.empty graph


isAcyclicHelper : List comparable -> Set comparable -> Graph comparable data -> Bool
isAcyclicHelper topSortedNodes seen graph =
  case topSortedNodes of
    [] ->
      True

    key :: keys ->
      if Set.member key (incoming key graph) then
        -- has an edge to itself
        False
      else if not <| Set.isEmpty <| Set.intersect seen (incoming key graph) then
        -- found a backwards edge
        False
      else
        isAcyclicHelper
          keys
          (Set.insert key seen)
          graph


{-| Get a topological sorting of the graph, if the graph doesn't contain any loops or cycles.
-}
topologicalSort : Graph comparable data -> Maybe (List comparable)
topologicalSort graph =
  let
    revpo =
      reversePostOrder graph
  in
    if isAcyclicHelper (List.reverse <| revpo) Set.empty graph then
      Just revpo
    else
      Nothing


{-| Get a list of all keys in postorder.
-}
postOrder : Graph comparable data -> List comparable
postOrder graph =
  List.reverse <| reversePostOrder graph


reversePostOrder : Graph comparable data -> List comparable
reversePostOrder (Graph graph) =
  Tuple.second <| reversePostOrderHelper (Dict.keys graph.nodes) [] Set.empty (Graph graph)


reversePostOrderHelper : List comparable -> List comparable -> Set comparable -> Graph comparable data -> ( Set comparable, List comparable )
reversePostOrderHelper nodeKeys keyOrder seenKeys graph =
  case nodeKeys of
    [] ->
      ( seenKeys, keyOrder )

    key :: keys ->
      if Set.member key seenKeys then
        reversePostOrderHelper keys keyOrder seenKeys graph
      else
        let
          ( seen, order ) =
            reversePostOrderHelper
              (outgoing key graph |> Set.toList)
              keyOrder
              (Set.insert key seenKeys)
              graph
        in
          reversePostOrderHelper keys (key :: order) seen graph
