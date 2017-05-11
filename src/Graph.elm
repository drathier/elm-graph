module Graph
  exposing
    ( Graph
    , RelativeOrdering(Before, After, Concurrent)
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
    , reachable
    , relativeOrder
      -- build
    , empty
    , emptyDag
    , insertNode
    , insertNodeData
    , insertEdge
    , removeNode
    , removeEdge
      -- feature flags
    , enableDagReachability
    , disableDagReachability
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
    , nearestCommonAncestor
    )

{-|

A simple functional graph library. Keys used to identify nodes can be any `comparable` and nodes can have any kind of metadata associated with them.

All operations that look at a single node are at most `O(log n)`.
Operations that look at all elements in the graph are at most `O(n log n)`.

# Graphs
@docs Graph

# Query
@docs getData, member, memberEdge, incoming, outgoing, size, nodes, edges, isAcyclic, reachable, nearestCommonAncestor

# Build
@docs empty, emptyDag, insertNode, insertNodeData, insertEdge, removeNode, removeEdge

# Transform
@docs map, foldl, foldr

# Combine
@docs partition, union, intersect

# Algorithms and Traversal
@docs postOrder, topologicalSort

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
  = Graph
      { nodes : Dict comparable (Node comparable a)
      , dagReachabilityState : FeatureState
      }


{-| The three possible states of a feature. UpToDateButDisabled is used when a feature is disabled, but still up to date. It may become stale, and moved to Disabled later, but moving from UpToDateButDisabled to UpToDate is free, since all data is already there.
-}
type FeatureState
  = Disabled
  | UpToDate
  | UpToDateButDisabled


-- NODE
-- NOTE: type system doesn't help differentiate between incoming and outgoing edges


type Node comparable data
  = Node
      { data : Maybe data
      , incoming : Set comparable
      , outgoing : Set comparable
      , reachable : Set comparable
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
    , reachable = Set.empty
    }


nodeData : data -> Node comparable data
nodeData data =
  Node
    { data = Just data
    , incoming = Set.empty
    , outgoing = Set.empty
    , reachable = Set.empty
    }


insert : comparable -> Node comparable data -> Graph comparable data -> Graph comparable data
insert key node (Graph graph) =
  Graph { graph | nodes = Dict.insert key node graph.nodes }
    |> ifDynamicDag (updateReachabilityFrom key)


--- TODO: plan forward: update reachability on self, then on all incoming, recursively. Use reverse postorder traversal starting at self (including self) and update based on parents.


updateReachabilityFrom : comparable -> Graph comparable data -> Graph comparable data
updateReachabilityFrom key graph =
  let
    calculatedReachableNodes : comparable -> Graph comparable data -> Set comparable
    calculatedReachableNodes key graph =
      Set.foldl
        (\key acc -> Set.union acc <| reachable key graph)
        (outgoing key graph)
        (outgoing key graph)
  in
    if calculatedReachableNodes key graph == reachable key graph then
      graph
    else
      let
        g =
          updateNode (\(Node node) -> Node { node | reachable = calculatedReachableNodes key graph }) key graph
      in
        Set.foldl updateReachabilityFrom g (incoming key g)


debugValue msg b =
  let
    x =
      Debug.log ("-> " ++ msg) ()

    y =
      b
  in
    Debug.log "<- " y


debugTag msg b =
  let
    x =
      Debug.log msg ()
  in
    b


--Debug.log "notEqual" <|
-- BUILD


{-| Create an empty graph.
-}
empty : Graph comparable data
empty =
  Graph { nodes = Dict.empty, dagReachabilityState = Disabled }


{-| Create an empty directed acyclic graph.
-}
emptyDag : Graph comparable data
emptyDag =
  Graph { nodes = Dict.empty, dagReachabilityState = UpToDate }


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
        |> ifDynamicDag (updateReachabilityFrom from)


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
          Graph { graph | nodes = Dict.remove key graph.nodes, dagReachabilityState = Disabled }
      in
        List.foldl removeEdge newGraph (incomingEdgesToRemove ++ outgoingEdgesToRemove)


-- TODO: figure out where we have to disable dagReachability, or recalculate it


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


-- DYNAMIC FEATURES


ifDynamicDag : (Graph comparable data -> Graph comparable data) -> Graph comparable data -> Graph comparable data
ifDynamicDag fn (Graph graph) =
  if graph.dagReachabilityState == Disabled then
    Graph graph
  else
    fn (Graph graph)


{-| Enable the dynamic reachability optimization for *directed acyclic graphs*. This allows O(log n) queries for the relative ordering of two elements in a partially ordered set, and O(log n) queries for the set of nodes that are reachable from a specific node.

The downside is that modifying the graph now takes O(log n * (nodes before this node)) time on average. Modifying the beginning of the graph is thus quite fast, but inserting an edge at the end takes O(n log n) time. The other downside is that this optimization only works on directed acyclic graphs.
-}
enableDagReachability : Graph comparable data -> Maybe (Graph comparable data)
enableDagReachability (Graph graph) =
  -- NOTE: first update, then mark as enabled
  updateReachability (Graph graph)
    |> Maybe.map (\(Graph graph) -> Graph { graph | dagReachabilityState = UpToDate })


{-| Disable the dynamic reachability optimization for *directed acyclic graphs*.
-}
disableDagReachability : Graph comparable data -> Graph comparable data
disableDagReachability (Graph graph) =
  Graph { graph | dagReachabilityState = Disabled }


updateReachability : Graph comparable data -> Maybe (Graph comparable data)
updateReachability graph =
  topologicalSort graph
    |> Maybe.map (List.foldl updateReachabilityFrom graph)


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


{-| Get the keys for all nodes in the graph.
-}
keys : Graph comparable data -> List comparable
keys (Graph graph) =
  Dict.keys graph.nodes


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


{-| Determine if a graph contains any loops or cycles.
-}
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


{-| Get the set of reachable nodes from a key, following outgoing edges any number of steps.
-}
reachable : comparable -> Graph comparable data -> Set comparable
reachable key (Graph graph) =
  Graph graph
    |> get key
    |> Maybe.map (\(Node node) -> node.reachable)
    |> Maybe.withDefault Set.empty


-- TODO: test!


type RelativeOrdering
  = Before
  | After
  | Concurrent


{-| Returns the relative ordering of two keys in a *directed acyclic graph*. If there is a path from a to b over outgoing edges, a is Before b. If there is no path between them, they compare EQ.
-}
relativeOrder : comparable -> comparable -> Graph comparable data -> RelativeOrdering
relativeOrder a b graph =
  if graph |> reachable a |> Set.member b then
    Before
  else if graph |> reachable b |> Set.member a then
    After
  else
    Concurrent


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
      |> Tuple.mapFirst (\x -> Graph { dagReachabilityState = Disabled, nodes = x })
      |> Tuple.mapSecond (\x -> Graph { dagReachabilityState = Disabled, nodes = x })
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
    { dagReachabilityState = Disabled
    , nodes =
        Dict.merge
          (\key node dict -> Dict.insert key node dict)
          (\key (Node n1) (Node n2) dict ->
            Dict.insert key
              (Node
                { data = Maybe.Extra.or n1.data n2.data
                , incoming = Set.union n1.incoming n2.incoming
                , outgoing = Set.union n1.outgoing n2.outgoing
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


{-| Create a graph based on the intersection of two graphs. If both graphs have the same node, edge or associated metadata, it will be in the resulting graph.
-}
intersect : Graph comparable data -> Graph comparable data -> Graph comparable data
intersect (Graph a) (Graph b) =
  Graph
    { dagReachabilityState = Disabled
    , nodes =
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


{-| The tight lower bound of a set of elements S is the set of elements B such
that each element in B comes before all elements in S in a directed acyclic
graph, and such that no element in B comes before any other element in B. By
`a comes before b`, I mean that there exists a path through the graph from a
to b.
-}
nearestCommonAncestor : Set comparable -> Graph comparable data -> Set comparable
nearestCommonAncestor sources graph =
  sources
