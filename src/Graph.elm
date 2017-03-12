module Graph exposing (..)

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Set exposing (Set)


type Graph comparable a
  = Graph { nodes : Dict comparable (Node comparable a) }


-- NOTE: type system doesn't help differentiate between incoming and outgoing edges


type Node comparable data
  = Node
      { key : comparable
      , data : Maybe data
      , incoming : Set comparable
      , outgoing : Set comparable
      }


-- HELPERS


nodeKey : comparable -> Node comparable data
nodeKey key =
  Node
    { key = key
    , data = Nothing
    , incoming = Set.empty
    , outgoing = Set.empty
    }


nodeKeyData : comparable -> data -> Node comparable data
nodeKeyData key data =
  Node
    { key = key
    , data = Just data
    , incoming = Set.empty
    , outgoing = Set.empty
    }


insert : Node comparable data -> Graph comparable data -> Graph comparable data
insert node (Graph graph) =
  Graph <| { graph | nodes = Dict.insert (nodeId node) node graph.nodes }


-- BUILD


{-| Create an empty graph.
-}
empty : Graph comparable data
empty =
  Graph
    { nodes = Dict.empty
    }


{-| Insert a node. Does not overwrite metadata if node already exists.
-}
insertNode : comparable -> Graph comparable data -> Graph comparable data
insertNode key (Graph graph) =
  case get key (Graph graph) of
    Nothing ->
      Graph { graph | nodes = Dict.insert key (nodeKey key) graph.nodes }

    Just _ ->
      Graph graph


{-| Insert a node with some associated metadata. Overwrites metadata if node already exists.
-}
insertNodeData : comparable -> data -> Graph comparable data -> Graph comparable data
insertNodeData key data (Graph graph) =
  Graph { graph | nodes = Dict.insert key (nodeKeyData key data) graph.nodes }


{-| Insert an edge between two nodes. If the nodes are not already in the graph, they will be inserted.
-}
insertEdge : comparable -> comparable -> Graph comparable data -> Graph comparable data
insertEdge from to (Graph graph) =
  Graph
    { graph
      | nodes =
          let
            (Node fromNode) =
              getOrInsert from (Graph graph)
          in
            Dict.insert from
              (Node
                { fromNode
                  | outgoing = Set.insert from (outgoing (Node fromNode))
                }
              )
            <|
              let
                (Node toNode) =
                  getOrInsert to (Graph graph)
              in
                Dict.insert to
                  (Node
                    { toNode
                      | incoming = Set.insert to (incoming (Node toNode))
                    }
                  )
                <|
                  graph.nodes
    }


{-| Remove a node by its key. No-op if node doesn't exist.
-}
removeNode : comparable -> Graph comparable data -> Graph comparable data
removeNode key (Graph graph) =
  case get key (Graph graph) of
    Nothing ->
      (Graph graph)

    Just node ->
      let
        edgesToRemove =
          List.map (\in_ -> ( in_, key )) (Set.toList <| incoming node)
            ++ List.map (\out -> ( key, out )) (Set.toList <| outgoing node)

        newGraph =
          Graph { graph | nodes = Dict.remove key graph.nodes }
      in
        List.foldl removeEdge newGraph edgesToRemove


{-| Remove an edge its source and target keys. No-op if edge doesn't exist.
-}
removeEdge : ( comparable, comparable ) -> Graph comparable data -> Graph comparable data
removeEdge ( from, to ) graph =
  let
    updateIncoming =
      \(Node node) ->
        Node
          { node
            | incoming = Set.remove from (incoming (Node node))
          }

    updateOutgoing =
      \(Node node) ->
        Node
          { node
            | outgoing = Set.remove to (outgoing (Node node))
          }
  in
    graph
      |> updateNodeIfExists updateIncoming to
      |> updateNodeIfExists updateOutgoing from


-- QUERY


{-| Get the id (key) of a node.
-}
nodeId : Node comparable data -> comparable
nodeId (Node node) =
  node.key


{-| Get the node with a specific key. Returns Nothing if it doesn't exist.
-}
get : comparable -> Graph comparable data -> Maybe (Node comparable data)
get key (Graph graph) =
  Dict.get key graph.nodes


{-| Get or insert a node with a specific key.
-}
getOrInsert : comparable -> Graph comparable data -> Node comparable data
getOrInsert key graph =
  Maybe.withDefault (nodeKey key) (get key graph)


{-| Get the data associated with a specific node.
-}
getData : Node comparable data -> Maybe data
getData (Node node) =
  node.data


{-| Get the set of incoming edges to a node.
-}
incoming : Node comparable data -> Set comparable
incoming (Node node) =
  node.incoming


{-| Get the set of outgoing edges from a node.
-}
outgoing : Node comparable data -> Set comparable
outgoing (Node node) =
  node.outgoing


{-| Determine the number of nodes in the graph.
-}
size : Graph comparable data -> Int
size (Graph graph) =
  Dict.size graph.nodes


{-| Get all nodes in the graph.
-}
nodes : Graph comparable data -> List (Node comparable data)
nodes (Graph graph) =
  Dict.values graph.nodes


{-| Get all directional edges in the graph. O(V+E)
-}
edges : Graph comparable data -> List ( comparable, comparable )
edges graph =
  (nodes graph)
    |> List.concatMap
        (\node ->
          let
            outgoingEdges =
              Set.toList <| outgoing node

            outgoingSize =
              Set.size <| outgoing node
          in
            List.Extra.zip
              (List.repeat outgoingSize (nodeId node))
              outgoingEdges
        )


{-| Check if a graph is empty, i.e. contains no nodes.
-}
isEmpty : Graph comparable data -> Bool
isEmpty (Graph graph) =
  Dict.isEmpty graph.nodes


-- UPDATE


{-| Update a node if it exists, otherwise do nothing.
-}
updateNodeIfExists : (Node comparable data -> Node comparable data) -> comparable -> Graph comparable data -> Graph comparable data
updateNodeIfExists func key graph =
  -- TODO: terrible name
  -- TODO: reconsider api; this might silence errors
  -- make a new function that creates the node if it doesn't exist
  updateNode (Maybe.map func) key graph


{-| Update a node with a given key. If it doesn't exist, `func` gets `Nothing` as its argument. If `func` returns `Nothing`, the node is removed.
-}
updateNode : (Maybe (Node comparable data) -> Maybe (Node comparable data)) -> comparable -> Graph comparable data -> Graph comparable data
updateNode func key (Graph graph) =
  case func <| get key (Graph graph) of
    Nothing ->
      Graph { graph | nodes = Dict.remove key graph.nodes }

    Just node ->
      Graph { graph | nodes = Dict.insert key node graph.nodes }


{-| Apply a function to all nodes in a graph.
-}
map : (Node comparable data -> Node comparable data) -> Graph comparable data -> Graph comparable data
map func graph =
  List.map func (nodes graph)
    |> List.foldl insert empty


{-| Fold over the nodes in a graph, in order from lowest key to highest key.
-}
foldl :
  (Node comparable data -> a -> a)
  -> a
  -> Graph comparable data
  -> a
foldl func acc graph =
  List.foldl func acc <| nodes graph


{-| Fold over the nodes in a graph, in order from highest key to lowest key.
-}
foldr :
  (Node comparable data -> a -> a)
  -> a
  -> Graph comparable data
  -> a
foldr func acc graph =
  List.foldr func acc <| nodes graph


{-| Apply a function to all nodes in a graph. If that function returns Nothing, the node is removed.
-}
modify :
  (Node comparable data -> Maybe (Node comparable data))
  -> Graph comparable data
  -> Graph comparable data
modify func graph =
  List.foldl
    (\node graph ->
      case func node of
        Nothing ->
          removeNode (nodeId node) graph

        Just node ->
          insert node graph
    )
    graph
    (nodes graph)
    |> cleanup


filter : (Node comparable data -> Bool) -> Graph comparable data -> Graph comparable data
filter func graph =
  modify
    (\node ->
      if func node then
        Just node
      else
        Nothing
    )
    graph
    |> cleanup


partition : (Node comparable data -> Bool) -> Graph comparable data -> ( Graph comparable data, Graph comparable data )
partition func graph =
  let
    ( left, right ) =
      foldl
        (\node ( left, right ) ->
          if func node then
            ( insert node left, right )
          else
            ( left, insert node right )
        )
        ( empty, empty )
        graph
  in
    ( cleanup left, cleanup right )


cleanup : Graph comparable data -> Graph comparable data
cleanup graph =
  let
    -- remove edges pointing to non-existent nodes
    removeDeadEdges =
      \(Node node) ->
        Just <|
          Node
            { node
              | incoming = Set.filter (\key -> get key graph /= Nothing) (node.incoming)
              , outgoing = Set.filter (\key -> get key graph /= Nothing) (node.outgoing)
            }
  in
    modify removeDeadEdges graph


-- COMBINE


union : Graph comparable data -> Graph comparable data -> Graph comparable data
union (Graph a) (Graph b) =
  Graph
    { nodes =
        Dict.merge
          (\key node dict -> Dict.insert key node dict)
          (\key (Node n1) (Node n2) dict ->
            Dict.insert key
              (Node
                { key = key
                , data = Maybe.Extra.or n1.data n2.data
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


intersect : Graph comparable data -> Graph comparable data -> Graph comparable data
intersect (Graph a) (Graph b) =
  Graph
    { nodes =
        Dict.merge
          (\key node dict -> dict)
          (\key (Node n1) (Node n2) dict ->
            Dict.insert key
              (Node
                { key = key
                , data =
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


diff : Graph comparable data -> Graph comparable data -> Graph comparable data
diff (Graph a) (Graph b) =
  Graph
    { nodes =
        Dict.merge
          (\key node dict -> Dict.insert key node dict)
          (\key (Node n1) (Node n2) dict -> dict)
          (\key node dict -> dict)
          a.nodes
          b.nodes
          Dict.empty
    }
    |> cleanup


{-| The most general way of combining two graphs. You provide three
accumulators for when a given node appears:

  1. Only in the left graph.
  2. In both graphs.
  3. Only in the right graph.

You then traverse all the keys from lowest to highest, building up whatever
you want.
-}
merge :
  (comparable -> Node comparable data -> Dict comparable (Node comparable data) -> Dict comparable (Node comparable data))
  -> (comparable -> Node comparable data -> Node comparable data -> Dict comparable (Node comparable data) -> Dict comparable (Node comparable data))
  -> (comparable -> Node comparable data -> Dict comparable (Node comparable data) -> Dict comparable (Node comparable data))
  -> Graph comparable data
  -> Graph comparable data
  -> Dict comparable (Node comparable data)
  -> Graph comparable data
merge leftStep bothStep rightStep (Graph leftGraph) (Graph rightGraph) initialResult =
  Graph
    { nodes =
        Dict.merge
          leftStep
          bothStep
          rightStep
          leftGraph.nodes
          rightGraph.nodes
          initialResult
    }
    |> cleanup


-- OTHER


reversePostOrderKeys : List comparable -> Set comparable -> Graph comparable data -> List comparable
reversePostOrderKeys nodeKeys seenKeys graph =
  -- invariant: nodeKeys does not overlap at all with seenKeys
  if List.isEmpty nodeKeys then
    []
  else
    let
      nodes =
        List.concatMap (\key -> Maybe.Extra.toList <| get key graph) nodeKeys

      nextSeenKeys =
        List.foldl Set.insert seenKeys nodeKeys

      next =
        List.filter (\key -> not <| Set.member key nextSeenKeys || List.member key nodeKeys) <|
          List.concatMap (\node -> Set.toList <| outgoing node) nodes
    in
      nodeKeys ++ reversePostOrderKeys next nextSeenKeys graph
