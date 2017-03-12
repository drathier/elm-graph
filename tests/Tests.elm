module Tests exposing (..)

import Maybe.Extra
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Graph exposing (..)
import Set


-- TODO: remove expectJust in favor of Expect.equal


expectJust maybe =
  case maybe of
    Nothing ->
      Expect.fail "Expected Just _, got Nothing"

    Just _ ->
      Expect.pass


many : List Expect.Expectation -> Expect.Expectation
many expectations =
  case expectations of
    [] ->
      Expect.pass

    expectation :: expectations ->
      case Expect.getFailure expectation of
        Nothing ->
          many expectations

        Just _ ->
          expectation


todo a =
  test a <| \() -> Expect.equal "" a


all : Test
all =
  describe "Graph tests"
    [ describe "Nodes"
        [ fuzz int "Insert a node with a random int id" <|
            \key ->
              expectJust <| get key <| insertNode key empty
        , fuzz2 int int "Insert a node with a random comparable id" <|
            \key1 key2 ->
              let
                key =
                  ( key1, key2 )
              in
                expectJust <| get key <| insertNode key empty
        , fuzz int "Remove a node with a random int id" <|
            \key ->
              let
                graph =
                  insertNode key empty

                removed =
                  removeNode key graph
              in
                many
                  [ Expect.notEqual graph removed
                  , Expect.equal Nothing <|
                      get key removed
                  ]
        , describe "Node metadata"
            [ fuzz2 int string "Insert a node with non-comparable metadata" <|
                \key data ->
                  insertNodeData key { data = data } empty
                    |> get key
                    |> Maybe.map getData
                    |> Maybe.Extra.join
                    |> Expect.equal (Just { data = data })
            ]
        ]
    , describe "Edges"
        [ fuzz2 int int "Insert an edge between two existing nodes" <|
            \from to ->
              let
                graph =
                  empty
                    |> insertNodeData from { data = from }
                    |> insertNodeData to { data = to }
                    |> insertEdge from to

                maybeFromNode =
                  get from graph

                maybeToNode =
                  get to graph
              in
                case ( maybeFromNode, maybeToNode ) of
                  ( Just fromNode, Just toNode ) ->
                    many
                      [ Expect.notEqual graph empty
                      , Set.singleton from |> Expect.equal (outgoing fromNode)
                      , Set.singleton to |> Expect.equal (incoming toNode)
                      , Just { data = from } |> Expect.equal (getData fromNode)
                      , Just { data = to } |> Expect.equal (getData toNode)
                      ]

                  _ ->
                    Expect.fail "failed lookup of nodes"
        , fuzz2 int int "Insert an edge with non-existant source node" <|
            \from to ->
              let
                graph =
                  empty
                    |> insertNodeData to { data = to }
                    |> insertEdge from to

                maybeFromNode =
                  get from graph

                maybeToNode =
                  get to graph
              in
                case ( maybeFromNode, maybeToNode ) of
                  ( Just fromNode, Just toNode ) ->
                    many
                      [ Expect.notEqual graph empty
                      , Set.singleton from |> Expect.equal (outgoing fromNode)
                      , Set.singleton to |> Expect.equal (incoming toNode)
                      , Nothing |> Expect.equal (getData fromNode)
                      , Just { data = to } |> Expect.equal (getData toNode)
                      ]

                  _ ->
                    Expect.fail "failed lookup of nodes created by edge insertion"
        , fuzz2 int int "Insert an edge with non-existant target node" <|
            \from to ->
              let
                graph =
                  empty
                    |> insertNodeData from { data = from }
                    |> insertEdge from to

                maybeFromNode =
                  get from graph

                maybeToNode =
                  get to graph
              in
                case ( maybeFromNode, maybeToNode ) of
                  ( Just fromNode, Just toNode ) ->
                    many
                      [ Expect.notEqual graph empty
                      , Set.singleton from |> Expect.equal (outgoing fromNode)
                      , Set.singleton to |> Expect.equal (incoming toNode)
                      , Just { data = from } |> Expect.equal (getData fromNode)
                      , Nothing |> Expect.equal (getData toNode)
                      ]

                  _ ->
                    Expect.fail "failed lookup of nodes created by edge insertion"
        , fuzz2 int int "Insert an edge between two non-existant nodes" <|
            \from to ->
              let
                graph =
                  insertEdge from to empty

                maybeFromNode =
                  get from graph

                maybeToNode =
                  get to graph
              in
                case ( maybeFromNode, maybeToNode ) of
                  ( Just fromNode, Just toNode ) ->
                    many
                      [ Expect.notEqual graph empty
                      , Set.singleton from |> Expect.equal (outgoing fromNode)
                      , Set.singleton to |> Expect.equal (incoming toNode)
                      , Nothing |> Expect.equal (getData fromNode)
                      , Nothing |> Expect.equal (getData toNode)
                      ]

                  _ ->
                    Expect.fail "failed lookup of nodes created by edge insertion"
        ]
    ]
