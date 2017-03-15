module Graph exposing (..)

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Set exposing (Set)


type Graph comparable a
  = Graph { nodes : Dict comparable (Node comparable a) }


-- NOTE: type system doesn't help differentiate between incoming and outgoing edges
-- NODE
-- NOTE: Don't expose anything related to Node


type Node comparable data
  = Node
      { key : comparable
      , data : Maybe data
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
insert (Node node) (Graph graph) =
  Graph <| { graph | nodes = Dict.insert node.key (Node node) graph.nodes }


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


{-| Update metadata for a node. Creates the node if it does not already exist.
-}
insertNodeData : comparable -> data -> Graph comparable data -> Graph comparable data
insertNodeData key data (Graph graph) =
  Graph { graph | nodes = Dict.insert key (nodeKeyData key data) graph.nodes }


{-| Insert an edge between two nodes. If the nodes are not already in the graph, they will be inserted.
-}
insertEdge : comparable -> comparable -> Graph comparable data -> Graph comparable data
insertEdge from to (Graph graph) =
  let
    (Node fromNode) =
      getOrCreate from (Graph graph)

    (Node toNode) =
      getOrCreate to (Graph graph)
  in
    Graph
      { graph
        | nodes =
            graph.nodes
              |> Dict.insert to (Node { toNode | incoming = Set.insert to toNode.incoming })
              |> Dict.insert from (Node { fromNode | outgoing = Set.insert from fromNode.outgoing })
      }


getOrCreate key graph =
  get key graph |> Maybe.withDefault (nodeKey key)


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
      |> updateNodeIfExists updateIncoming to
      |> updateNodeIfExists updateOutgoing from


-- QUERY


get : comparable -> Graph comparable data -> Maybe (Node comparable data)
get key (Graph graph) =
  Dict.get key graph.nodes


{-| Determine the number of nodes in the graph.
-}
size : Graph comparable data -> Int
size (Graph graph) =
  Dict.size graph.nodes


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
  case get key (Graph graph) |> func of
    Nothing ->
      Graph { graph | nodes = Dict.remove key graph.nodes }

    Just node ->
      Graph { graph | nodes = Dict.insert key node graph.nodes }


{-| Keep only data that satisfy the predicate.
-}
filterData : (comparable -> Maybe data -> Bool) -> Graph comparable data -> Graph comparable data
filterData func (Graph graph) =
  Graph { graph | nodes = Dict.filter (\key (Node node) -> func key node.data) graph.nodes }


{-| Apply a function to the data associated with each node in a graph.
-}
mapData : (comparable -> Maybe data -> Maybe data) -> Graph comparable data -> Graph comparable data
mapData func (Graph graph) =
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


partition : (comparable -> Maybe data -> Bool) -> Graph comparable data -> ( Graph comparable data, Graph comparable data )
partition func (Graph graph) =
  let
    ( left, right ) =
      Dict.foldl
        (\key (Node node) ( left, right ) ->
          if func key node.data then
            ( Dict.insert key (Node node) left, right )
          else
            ( left, Dict.insert key (Node node) right )
        )
        ( Dict.empty, Dict.empty )
        graph.nodes
  in
    ( cleanup <| Graph { graph | nodes = left }, cleanup <| Graph { graph | nodes = right } )


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


-- OTHER


reversePostOrderKeys : List comparable -> Set comparable -> Graph comparable data -> List comparable
reversePostOrderKeys nodeKeys seenKeys graph =
  nodeKeys
