module Tests exposing (..)

import Maybe.Extra
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Graph exposing (..)
import Set


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
              insertNode key empty
                |> member key
                |> Expect.true "inserted node isn't a member"
        , fuzz2 int int "Insert a node with a random comparable id" <|
            \key1 key2 ->
              let
                key =
                  ( key1, key2 )
              in
                insertNode key empty
                  |> member key
                  |> Expect.true "inserted node isn't a member"
        , fuzz int "Remove a node with a random int id" <|
            \key ->
              empty
                |> insertNode key
                |> removeNode key
                |> member key
                |> Expect.false "removed key shouldn't be present"
        , describe "Node metadata"
            [ fuzz2 int string "Node metadata doesn't have to be comparable" <|
                \key data ->
                  insertNodeData key { data = data } empty
                    |> getData key
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
              in
                many
                  [ Expect.notEqual graph empty
                  , Set.singleton to |> Expect.equal (outgoing from graph)
                  , Set.singleton from |> Expect.equal (incoming to graph)
                  , Just { data = from } |> Expect.equal (getData from graph)
                  , Just { data = to } |> Expect.equal (getData to graph)
                  ]
        , fuzz2 int int "Insert an edge with non-existant source node" <|
            \from to ->
              let
                graph =
                  empty
                    |> insertNodeData to { data = to }
                    |> insertEdge from to
              in
                many
                  [ Expect.notEqual graph empty
                  , Set.singleton to |> Expect.equal (outgoing from graph)
                  , Set.singleton from |> Expect.equal (incoming to graph)
                  , Nothing |> Expect.equal (getData from graph)
                  , Just { data = to } |> Expect.equal (getData to graph)
                  ]
        , fuzz2 int int "Insert an edge with non-existant target node" <|
            \from to ->
              let
                graph =
                  empty
                    |> insertNodeData from { data = from }
                    |> insertEdge from to
              in
                many
                  [ Expect.notEqual graph empty
                  , Set.singleton to |> Expect.equal (outgoing from graph)
                  , Set.singleton from |> Expect.equal (incoming to graph)
                  , Just { data = from } |> Expect.equal (getData from graph)
                  , Nothing |> Expect.equal (getData to graph)
                  ]
        , fuzz2 int int "Insert an edge between two non-existant nodes" <|
            \from to ->
              let
                graph =
                  insertEdge from to empty
              in
                many
                  [ Expect.notEqual graph empty
                  , Set.singleton to |> Expect.equal (outgoing from graph)
                  , Set.singleton from |> Expect.equal (incoming to graph)
                  , Nothing |> Expect.equal (getData from graph)
                  , Nothing |> Expect.equal (getData to graph)
                  ]
        ]
    ]
