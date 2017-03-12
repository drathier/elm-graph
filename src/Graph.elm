module Graph exposing (..)

import Dict exposing (Dict)
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


-- QUERY


get : comparable -> Graph comparable data -> Maybe (Node comparable data)
get key (Graph graph) =
  Dict.get key graph.nodes


getOrNew : comparable -> Graph comparable data -> Node comparable data
getOrNew key graph =
  Maybe.withDefault (nodeKey key) (get key graph)


getData : Node comparable data -> Maybe data
getData (Node node) =
  node.data


incoming : Node comparable data -> Set comparable
incoming (Node node) =
  node.incoming


outgoing : Node comparable data -> Set comparable
outgoing (Node node) =
  node.outgoing


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
              getOrNew from (Graph graph)
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
                  getOrNew to (Graph graph)
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


-- REMOVE


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
      |> changeNode updateIncoming to
      |> changeNode updateOutgoing from


-- UPDATE


{-| Update a node if it exists, otherwise do nothing.
-}
changeNode : (Node comparable data -> Node comparable data) -> comparable -> Graph comparable data -> Graph comparable data
changeNode func key graph =
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
