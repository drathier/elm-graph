module Graph exposing (..)

import Dict exposing (Dict)
import Maybe.Extra
import Set exposing (Set)


type Graph comparable a
  = Graph { nodes : Dict comparable (Node comparable a) }


type Node comparable data
  = Node
      { key : comparable
      , data : Maybe data
      , incoming : IncomingKeys comparable
      , outgoing : OutgoingKeys comparable
      }


-- HELPER TYPES
-- to avoid mixing up incoming and outgoing keys


type IncomingKeys comparable
  = IncomingKeys (Set comparable)


type OutgoingKeys comparable
  = OutgoingKeys (Set comparable)


incomingKeys : Set comparable -> IncomingKeys comparable
incomingKeys set =
  IncomingKeys set


outgoingKeys : Set comparable -> OutgoingKeys comparable
outgoingKeys set =
  OutgoingKeys set


-- HELPERS


nodeKey : comparable -> Node comparable data
nodeKey key =
  Node
    { key = key
    , data = Nothing
    , incoming = incomingKeys <| Set.empty
    , outgoing = outgoingKeys <| Set.empty
    }


nodeKeyData : comparable -> data -> Node comparable data
nodeKeyData key data =
  Node
    { key = key
    , data = Just data
    , incoming = incomingKeys <| Set.empty
    , outgoing = outgoingKeys <| Set.empty
    }


-- QUERY


empty : Graph comparable data
empty =
  Graph
    { nodes = Dict.empty
    }


get : comparable -> Graph comparable data -> Maybe (Node comparable data)
get key (Graph graph) =
  Dict.get key graph.nodes


getOrNew : comparable -> Graph comparable data -> Node comparable data
getOrNew key graph =
  Maybe.withDefault (nodeKey key) (get key graph)


data : comparable -> Graph comparable data -> Maybe data
data key graph =
  get key graph |> Maybe.andThen (\(Node node) -> node.data)


-- TODO: remove graph argument to outgoing, incoming?


incoming : Node comparable data -> Graph comparable data -> Set comparable
incoming (Node node) graph =
  case node.incoming of
    IncomingKeys set ->
      set


outgoing : Node comparable data -> Graph comparable data -> Set comparable
outgoing (Node node) graph =
  case node.outgoing of
    OutgoingKeys set ->
      set


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
                  | outgoing = outgoingKeys <| Set.insert from (outgoing (Node fromNode) (Graph graph))
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
                      | incoming = incomingKeys <| Set.insert to (incoming (Node toNode) (Graph graph))
                    }
                  )
                <|
                  graph.nodes
    }
