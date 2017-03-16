module Tests exposing (..)

import List.Extra
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


allDifferent lst expectation =
  case lst of
    x :: xs ->
      if List.map (\a -> a == x) xs |> List.member True then
        Expect.pass
      else
        allDifferent xs expectation

    [] ->
      expectation


all : Test
all =
  describe "Graph tests"
    [ describe "insertNode"
        [ fuzz int "Insert a node with a random int id" <|
            \key ->
              empty
                |> insertNode key
                |> member key
                |> Expect.true "inserted node isn't a member"
        , fuzz2 int int "Insert a node with a random comparable id" <|
            \key1 key2 ->
              let
                key =
                  ( key1, key2 )
              in
                empty
                  |> insertNode key
                  |> member key
                  |> Expect.true "inserted node isn't a member"
        ]
    , describe "removeNode"
        [ fuzz int "Remove a node with a random int id" <|
            \key ->
              empty
                |> insertNode key
                |> removeNode key
                |> member key
                |> Expect.false "removed key shouldn't be present"
        ]
    , describe "Node metadata"
        [ fuzz2 int string "Node metadata gets stored in the graph" <|
            \key data ->
              empty
                |> insertNodeData key data
                |> getData key
                |> Expect.equal (Just data)
        , fuzz2 int string "inserting an existing node doesn't remove metadata" <|
            \key data ->
              empty
                |> insertNodeData key data
                |> insertNode key
                |> getData key
                |> Expect.equal (Just data)
        , fuzz2 int string "Node metadata doesn't have to be comparable" <|
            \key data ->
              empty
                |> insertNodeData key { data = data }
                |> getData key
                |> Expect.equal (Just { data = data })
        ]
    , describe "member"
        [ fuzz int "Member check for members returns true" <|
            \key ->
              empty
                |> insertNode key
                |> member key
                |> Expect.true "member should be present"
        , fuzz2 int int "Member check for non-existant members returns false" <|
            \memberKey nonMemberKey ->
              empty
                |> insertNode memberKey
                |> member nonMemberKey
                |> Expect.false "non-member should not be present"
        , fuzz int "Empty graph has no members" <|
            \key ->
              empty
                |> member key
                |> Expect.false "empty graph should not have any members"
        ]
    , describe "incoming"
        [ fuzz5 int int int int int "Incoming should return incoming edges" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  graph =
                    empty
                      |> insertEdge ( e, a )
                      |> insertEdge ( a, b )
                      |> insertEdge ( c, b )
                      |> insertEdge ( e, b )
                      |> insertEdge ( a, c )
                      |> insertEdge ( b, c )
                      |> insertEdge ( b, d )
                      |> insertEdge ( c, e )
                      |> insertEdge ( d, e )
                in
                  many
                    [ incoming a graph |> Expect.equal (Set.fromList [ e ])
                    , incoming b graph |> Expect.equal (Set.fromList [ a, c, e ])
                    , incoming c graph |> Expect.equal (Set.fromList [ a, b ])
                    , incoming d graph |> Expect.equal (Set.fromList [ b ])
                    , incoming e graph |> Expect.equal (Set.fromList [ c, d ])
                    ]
        ]
    , describe "outgoing"
        [ fuzz5 int int int int int "Outgoing should return outgoing edges" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  graph =
                    empty
                      |> insertEdge ( a, b )
                      |> insertEdge ( a, c )
                      |> insertEdge ( b, c )
                      |> insertEdge ( b, d )
                      |> insertEdge ( c, b )
                      |> insertEdge ( c, e )
                      |> insertEdge ( d, e )
                      |> insertEdge ( e, a )
                      |> insertEdge ( e, b )
                in
                  many
                    [ outgoing a graph |> Expect.equal (Set.fromList [ b, c ])
                    , outgoing b graph |> Expect.equal (Set.fromList [ c, d ])
                    , outgoing c graph |> Expect.equal (Set.fromList [ b, e ])
                    , outgoing d graph |> Expect.equal (Set.fromList [ e ])
                    , outgoing e graph |> Expect.equal (Set.fromList [ a, b ])
                    ]
        ]
    , describe "size"
        [ fuzz (list int) "Size equals the number of unique keys inserted " <|
            \keys ->
              List.foldl insertNode empty keys
                |> size
                |> Expect.equal (Set.fromList keys |> Set.size)
        ]
    , describe "nodes"
        [ fuzz (list int) "nodes returns all inserted keys" <|
            \keys ->
              List.foldl insertNode empty keys
                |> nodes
                |> List.map Tuple.first
                |> Set.fromList
                |> Expect.equal (Set.fromList keys)
        , fuzz (list int) "nodes returns all inserted keys, with metadata" <|
            \keys ->
              List.foldl (\key -> insertNodeData key ( key, key )) empty keys
                |> nodes
                |> List.map (\( key, data ) -> ( key, Maybe.withDefault ( key, 42 ) <| data ))
                |> Set.fromList
                |> Expect.equal (Set.fromList <| List.map (\key -> ( key, ( key, key ) )) keys)
        ]
    , describe "edges"
        [ fuzz5 int int int int int "edges returns all inserted edges" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  graphEdges =
                    [ ( a, b ), ( a, c ), ( b, c ), ( b, d ), ( c, b ), ( c, e ), ( d, e ), ( e, a ), ( e, b ) ]

                  graph =
                    List.foldl insertEdge empty graphEdges
                in
                  edges graph
                    |> Set.fromList
                    |> Expect.equal (Set.fromList graphEdges)
        ]
    , describe "insertEdge"
        [ fuzz2 int int "Insert an edge between two existing nodes" <|
            \from to ->
              let
                graph =
                  empty
                    |> insertNodeData from { data = from }
                    |> insertNodeData to { data = to }
                    |> insertEdge ( from, to )
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
                    |> insertEdge ( from, to )
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
                    |> insertEdge ( from, to )
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
                  insertEdge ( from, to ) empty
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
