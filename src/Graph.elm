module Graph
  exposing
    ( Graph
      -- query
    , getData
    , getEdgeData
    , member
    , memberEdge
    , incoming
    , outgoing
    , isEmpty
    , size
    , keys
    , nodes
    , edges
    , edgesWithData
      -- build
    , empty
    , insert
    , insertData
    , insertEdge
    , insertEdgeData
    , remove
    , removeData
    , removeEdge
    , removeEdgeData
    , update
    , updateEdge
      -- transformation
    , map
    , mapEdge
    , foldl
    , foldr
      -- set operations
    , filter
    , partition
    , union
    , intersect
      -- graph traversal
    , topologicalSort
    , postOrder
    , isAcyclic
      -- debugging
    , valid
    )

{-|

A simple functional graph library. Keys used to identify nodes can be any `comparable` and nodes can have any kind of metadata associated with them.

All operations that look at a single node are at most `O(log n)`.
Operations that look at all elements in the graph are at most `O(n log n)`.

# Graphs
@docs Graph

# Query
@docs getData, getEdgeData, member, memberEdge, incoming, outgoing, isEmpty, size, keys, nodes, edges, edgesWithData, isAcyclic

# Build
@docs empty, insert, insertData, insertEdge, insertEdgeData, remove, removeData, removeEdge, removeEdgeData, update, updateEdge

# Transform
@docs map, mapEdge, foldl, foldr

# Combine
@docs filter, partition, union, intersect

# Algorithms and Traversal
@docs postOrder, topologicalSort

# Debugging tools
@docs valid
-}

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Tuple


{-

   It is theoretically possible to get O(1) path traversals without enforcing a DAG or violating immutability, but that would make all inserts and modifications at least O(n), because all nodes reference all other nodes (except if the graph is disjoint). If I figure out a way to do the conversion in in linear time, I can add that later.

-}


{-| A directed graph. `(Graph Int String Float)` is a graph that uses `Int`s for identifying its nodes, and lets you store a `String` on each node and a `Float` on each edge.
-}
type Graph comparable data edgeData
  = Graph { nodes : Dict comparable (Node comparable data edgeData) }


-- NODE


type Node comparable data edgeData
  = Node
      { data : Maybe data
      , incoming : Set comparable
      , outgoing : Dict comparable (Maybe edgeData)
      , reachable : Set comparable
      }


{-| Get the data associated with a specific node.
-}
getData : comparable -> Graph comparable data edgeData -> Maybe data
getData key graph =
  get key graph |> Maybe.andThen (\(Node node) -> node.data)


{-| Get the data associated with a specific edge.

If you want to pass the edge as a 2-tuple instead, you can use [`getEdgeData`](Graph-Pair#getEdgeData) from the [Pair](Graph-Pair) module.
-}
getEdgeData : comparable -> comparable -> Graph comparable data edgeData -> Maybe edgeData
getEdgeData from to graph =
  get from graph |> Maybe.andThen (\(Node node) -> node.outgoing |> Dict.get to) |> Maybe.Extra.join


{-| Get the set of incoming edges to a node.
-}
incoming : comparable -> Graph comparable data edgeData -> Set comparable
incoming key graph =
  get key graph |> Maybe.map (\(Node node) -> node.incoming) |> Maybe.withDefault Set.empty


{-| Get the set of outgoing edges from a node.
-}
outgoing : comparable -> Graph comparable data edgeData -> Set comparable
outgoing key graph =
  get key graph |> Maybe.map (\(Node node) -> node.outgoing |> Dict.keys |> Set.fromList) |> Maybe.withDefault Set.empty


-- NODE HELPERS


node : Node comparable data edgeData
node =
  Node
    { data = Nothing
    , incoming = Set.empty
    , outgoing = Dict.empty
    , reachable = Set.empty
    }


nodeData : data -> Node comparable data edgeData
nodeData data =
  Node
    { data = Just data
    , incoming = Set.empty
    , outgoing = Dict.empty
    , reachable = Set.empty
    }


insertRawNode : comparable -> Node comparable data edgeData -> Graph comparable data edgeData -> Graph comparable data edgeData
insertRawNode key node (Graph graph) =
  Graph { graph | nodes = Dict.insert key node graph.nodes }


-- BUILD


{-| Create an empty graph.
-}
empty : Graph comparable data edgeData
empty =
  Graph { nodes = Dict.empty }


{-| Insert a node. Does not overwrite metadata if node already exists.
-}
insert : comparable -> Graph comparable data edgeData -> Graph comparable data edgeData
insert key graph =
  insertRawNode key (getOrCreate key graph) graph


{-| Update metadata for a node. Creates the node if it does not already exist.
-}
insertData : comparable -> data -> Graph comparable data edgeData -> Graph comparable data edgeData
insertData key data (Graph graph) =
  case getOrCreate key (Graph graph) of
    Node node ->
      Graph
        { graph
          | nodes =
              Dict.insert key
                (Node { node | data = Just data })
                graph.nodes
        }


{-| Insert an edge between two nodes. Creates any nodes that do not already exist.

If you want to pass the edge as a 2-tuple instead, you can use [`insertEdge`](Graph-Pair#insertEdge) from the [Pair](Graph-Pair) module.
-}
insertEdge : comparable -> comparable -> Graph comparable data edgeData -> Graph comparable data edgeData
insertEdge from to graph =
  insertEdgeDataHelper from to Nothing graph


{-| Insert an edge with some metadata between two nodes. Creates any nodes that do not already exist.

If you want to pass the edge as a 2-tuple instead, you can use [`insertEdgeData`](Graph-Pair#insertEdgeData) from the [Pair](Graph-Pair) module.
-}
insertEdgeData : comparable -> comparable -> edgeData -> Graph comparable data edgeData -> Graph comparable data edgeData
insertEdgeData from to edgeData graph =
  insertEdgeDataHelper from to (Just edgeData) graph


insertEdgeDataHelper : comparable -> comparable -> Maybe edgeData -> Graph comparable data edgeData -> Graph comparable data edgeData
insertEdgeDataHelper from to wrappedEdgeData graph =
  let
    (Node fromNode) =
      getOrCreate from graph

    (Node toNode) =
      getOrCreate to graph
  in
    if from == to then
      -- self-edge, fill both incoming and outgoing
      graph
        |> insertRawNode from
            (Node
              { fromNode
                | incoming = Set.insert from fromNode.incoming
                , outgoing = Dict.insert from wrappedEdgeData fromNode.outgoing
              }
            )
    else
      graph
        |> insertRawNode to (Node { toNode | incoming = Set.insert from toNode.incoming })
        |> insertRawNode from (Node { fromNode | outgoing = Dict.insert to wrappedEdgeData fromNode.outgoing })


getOrCreate key graph =
  get key graph |> Maybe.withDefault node


{-| Remove a node by its key. No-op if node doesn't exist.
-}
remove : comparable -> Graph comparable data edgeData -> Graph comparable data edgeData
remove key (Graph graph) =
  case get key (Graph graph) of
    Nothing ->
      (Graph graph)

    Just (Node node) ->
      let
        incomingEdgesToRemove =
          List.map (\in_ -> ( in_, key )) (Set.toList node.incoming)

        outgoingEdgesToRemove =
          List.map (\out -> ( key, out )) (Dict.keys node.outgoing)

        newGraph =
          Graph { graph | nodes = Dict.remove key graph.nodes }
      in
        List.foldl (uncurry removeEdge) newGraph (incomingEdgesToRemove ++ outgoingEdgesToRemove)


{-| Remove an edge identified by its source and target keys. No-op if source, target or edge doesn't exist.

If you want to pass the edge as a 2-tuple instead, you can use [`removeEdge`](Graph-Pair#removeEdge) from the [Pair](Graph-Pair) module.
-}
removeEdge : comparable -> comparable -> Graph comparable data edgeData -> Graph comparable data edgeData
removeEdge from to graph =
  let
    updateIncoming =
      \(Node node) -> Node { node | incoming = Set.remove from node.incoming }

    updateOutgoing =
      \(Node node) -> Node { node | outgoing = Dict.remove to node.outgoing }
  in
    graph
      |> updateNode updateIncoming to
      |> updateNode updateOutgoing from


{-| Remove the metadata associated with a specific node.
-}
removeData : comparable -> Graph comparable data edgeData -> Graph comparable data edgeData
removeData key (Graph graph) =
  Graph
    { graph
      | nodes = graph.nodes |> Dict.update key (Maybe.map (\(Node node) -> Node { node | data = Nothing }))
    }


{-| Remove the metadata associated with a specific edge.

If you want to pass the edge as a 2-tuple instead, you can use [`removeEdgeData`](Graph-Pair#removeEdgeData) from the [Pair](Graph-Pair) module.
-}
removeEdgeData : comparable -> comparable -> Graph comparable data edgeData -> Graph comparable data edgeData
removeEdgeData from to graph =
  graph |> removeEdge from to |> insertEdge from to


{-| Create a graph with a single node with metadata.
-}
singleton : comparable -> data -> Graph comparable data edgeData
singleton key data =
  empty |> insertData key data


{-| Update the metadata associated with a specific node.
-}
update : comparable -> (Maybe data -> Maybe data) -> Graph comparable data edgeData -> Graph comparable data edgeData
update key fn graph =
  getData key graph |> fn |> Maybe.map (\data -> insertData key data graph) |> Maybe.withDefault graph


{-| Update the metadata associated with a specific edge.

If you want to pass the edge as a 2-tuple instead, you can use [`updateEdge`](Graph-Pair#updateEdge) from the [Pair](Graph-Pair) module.
-}
updateEdge : comparable -> comparable -> (Maybe edgeData -> Maybe edgeData) -> Graph comparable data edgeData -> Graph comparable data edgeData
updateEdge from to fn graph =
  getEdgeData from to graph |> fn |> Maybe.map (\edgeData -> insertEdgeData from to edgeData graph) |> Maybe.withDefault graph


-- QUERY


get : comparable -> Graph comparable data edgeData -> Maybe (Node comparable data edgeData)
get key (Graph graph) =
  Dict.get key graph.nodes


{-| Determine if the graph is empty.
-}
isEmpty : Graph comparable data edgeData -> Bool
isEmpty (Graph graph) =
  Dict.isEmpty graph.nodes


{-| Determine the number of nodes in the graph.
-}
size : Graph comparable data edgeData -> Int
size (Graph graph) =
  Dict.size graph.nodes


{-| Determine if a node identified by a key is in the graph.
-}
member : comparable -> Graph comparable data edgeData -> Bool
member key (Graph graph) =
  Dict.member key graph.nodes


{-| Determine if an edge identified by a pair of keys is in the graph.
-}
memberEdge : ( comparable, comparable ) -> Graph comparable data edgeData -> Bool
memberEdge ( from, to ) graph =
  outgoing from graph
    |> Set.member to


{-| Get the keys for all nodes in the graph.
-}
keys : Graph comparable data edgeData -> List comparable
keys (Graph graph) =
  Dict.keys graph.nodes


{-| Get the (key, data) pair for each node in the graph.
-}
nodes : Graph comparable data edgeData -> List ( comparable, Maybe data )
nodes =
  foldl (\key data list -> ( key, data ) :: list) []


{-| Get the (from, to) pair for each edge in the graph.
-}
edges : Graph comparable data edgeData -> List ( comparable, comparable )
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


{-| Get the (from, to, Maybe edgeData) pair for each edge in the graph.
-}
edgesWithData : Graph comparable data edgeData -> List ( comparable, comparable, Maybe edgeData )
edgesWithData graph =
  foldl
    (\key _ list ->
      (get key graph
        |> Maybe.map (\(Node node) -> node.outgoing)
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
        |> List.map (\( out, edgeData ) -> ( key, out, edgeData ))
      )
        ++ list
    )
    []
    graph


{-| Determine if a graph contains any loops or cycles.
-}
isAcyclic : Graph comparable data edgeData -> Bool
isAcyclic graph =
  isAcyclicHelper (List.reverse <| reversePostOrder graph) Set.empty graph


isAcyclicHelper : List comparable -> Set comparable -> Graph comparable data edgeData -> Bool
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


-- UPDATE


updateNode : (Node comparable data edgeData -> Node comparable data edgeData) -> comparable -> Graph comparable data edgeData -> Graph comparable data edgeData
updateNode func key (Graph graph) =
  -- Update a node if it exists, otherwise do nothing.
  case get key (Graph graph) of
    Just node ->
      Graph { graph | nodes = Dict.insert key (func node) graph.nodes }

    Nothing ->
      Graph graph


{-| Apply a function to the data associated with each node in a graph.
-}
map : (comparable -> Maybe data1 -> Maybe data2) -> Graph comparable data1 edgeData -> Graph comparable data2 edgeData
map func (Graph graph) =
  Graph { graph | nodes = Dict.map (\key (Node node) -> Node { node | data = func key node.data }) graph.nodes }


{-| Apply a function to the data associated with each edge in a graph.

If you want to pass the edge as a 2-tuple instead, you can use [`mapEdge`](Graph-Pair#mapEdge) from the [Pair](Graph-Pair) module.
-}
mapEdge : (comparable -> comparable -> Maybe edgeData1 -> Maybe edgeData2) -> Graph comparable data edgeData1 -> Graph comparable data edgeData2
mapEdge func (Graph graph) =
  Graph { graph | nodes = Dict.map (\from (Node node) -> Node { node | outgoing = Dict.map (func from) node.outgoing }) graph.nodes }


{-| Fold over the node keys and data in a graph, in order from lowest key to highest key.
-}
foldl :
  (comparable -> Maybe data -> a -> a)
  -> a
  -> Graph comparable data edgeData
  -> a
foldl func acc (Graph graph) =
  Dict.foldl (\key (Node node) -> func key node.data) acc graph.nodes


{-| Fold over the node keys and data in a graph, in order from highest key to lowest key.
-}
foldr :
  (comparable -> Maybe data -> a -> a)
  -> a
  -> Graph comparable data edgeData
  -> a
foldr func acc (Graph graph) =
  Dict.foldr (\key (Node node) -> func key node.data) acc graph.nodes


{-| Create a copy of the graph, only keeping nodes where the predicate function returned True.
-}
filter : (comparable -> Maybe data -> Bool) -> Graph comparable data edgeData -> Graph comparable data edgeData
filter func graph =
  partition func graph |> Tuple.first


{-| Partition a graph into two parts. The left one, with the nodes where the predicate function returned True, and right one, where it returned False.
-}
partition : (comparable -> Maybe data -> Bool) -> Graph comparable data edgeData -> ( Graph comparable data edgeData, Graph comparable data edgeData )
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


cleanup : Graph comparable data edgeData -> Graph comparable data edgeData
cleanup (Graph graph) =
  let
    -- remove edges pointing to non-existent nodes
    removeDeadEdges =
      \key (Node node) ->
        Node
          { node
            | incoming = node.incoming |> Set.filter (\key -> get key (Graph graph) /= Nothing)
            , outgoing = node.outgoing |> Dict.filter (\key _ -> get key (Graph graph) /= Nothing)
          }
  in
    Graph { graph | nodes = Dict.map removeDeadEdges graph.nodes }


-- COMBINE


{-| Join two graphs together. If an edge appears between two nodes in either of the graphs, it will be in the resulting graph. If a node identified by a specific key appears in any of the graphs, it will be in the resulting graph. If both graphs have metadata for the same node or edge, the metadata in the left graph will be used.
-}
union : Graph comparable data edgeData -> Graph comparable data edgeData -> Graph comparable data edgeData
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
                , outgoing = Dict.union n1.outgoing n2.outgoing
                , reachable = Set.empty
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


{-| Create a graph based on the intersection of two graphs. If both graphs have the same node, edge or associated metadata, it will be in the resulting graph. If both graphs have metadata for the same node or edge, the metadata in the left graph will be used.
-}
intersect : Graph comparable data edgeData -> Graph comparable data edgeData -> Graph comparable data edgeData
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
                , outgoing = Dict.intersect n1.outgoing n2.outgoing
                , reachable = Set.empty
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


-- TRAVERSAL


{-| Get a topological sorting of the graph, if the graph doesn't contain any loops or cycles.
-}
topologicalSort : Graph comparable data edgeData -> Maybe (List comparable)
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
postOrder : Graph comparable data edgeData -> List comparable
postOrder graph =
  List.reverse <| reversePostOrder graph


reversePostOrder : Graph comparable data edgeData -> List comparable
reversePostOrder (Graph graph) =
  Tuple.second <|
    reversePostOrderHelper (Dict.keys graph.nodes) [] Set.empty (Graph graph)


reversePostOrderHelper : List comparable -> List comparable -> Set comparable -> Graph comparable data edgeData -> ( Set comparable, List comparable )
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


{-| Validate checks that all invariants in the graph are correct. Useful for debugging.
-}
valid : Graph comparable data edgeData -> Result String ()
valid (Graph graph) =
  -- Only validate if a tag is set; that means we're running our own unit tests.
  let
    hasDanglingOutgoingEdges key =
      (outgoing key (Graph graph)) |> Set.toList |> List.map (\key -> incoming key (Graph graph)) |> List.all (Set.member key) |> not

    hasDanglingIncomingEdges key =
      (incoming key (Graph graph)) |> Set.toList |> List.map (\key -> outgoing key (Graph graph)) |> List.all (Set.member key) |> not

    reachable : comparable -> Set comparable
    reachable key =
      (Graph graph)
        |> get key
        |> Maybe.map (\(Node node) -> node.reachable)
        |> Maybe.withDefault Set.empty

    reachabilityCacheIsStale key =
      (outgoing key (Graph graph) /= Set.empty)
        && (reachable key
              /= (List.foldl
                    Set.union
                    (outgoing key (Graph graph))
                  <|
                    List.map (\key -> reachable key) <|
                      (Set.toList <| outgoing key (Graph graph))
                 )
           )
  in
    if keys (Graph graph) |> List.any hasDanglingOutgoingEdges then
      Err (toString ( "found dangling outgoing edge", (Graph graph) ))
    else if keys (Graph graph) |> List.any hasDanglingIncomingEdges then
      Err (toString ( "found dangling incoming edge", (Graph graph) ))
    else
      Ok ()
