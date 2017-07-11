module Tests exposing (..)

import List.Extra
import Maybe.Extra
import RandomTests exposing (randomTests)
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Graph exposing (..)
import Graph.Pair as P
import Set
import GraphFuzzer exposing (acyclicGraphFuzzer, acyclicGraphFuzzerWithSelfEdges, graphFuzzer)
import TestUtils exposing (allDifferent, checkPartialOrdering, many)


all : Test
all =
  describe "All tests for Graph"
    [ graphTests
    , randomTests
    ]


expectValid : Graph comparable data edgeData -> Expect.Expectation
expectValid graph =
  case valid graph of
    Ok () ->
      Expect.pass

    Err msg ->
      Expect.fail msg


graphTests =
  describe "Graph tests"
    [ describe "insertNode"
        [ fuzz int "Insert a node with a random int id" <|
            \key ->
              empty
                |> insert key
                |> member key
                |> Expect.true "inserted node isn't a member"
        , fuzz2 int int "Insert a node with a random comparable id" <|
            \key1 key2 ->
              let
                key =
                  ( key1, key2 )
              in
                empty
                  |> insert key
                  |> member key
                  |> Expect.true "inserted node isn't a member"
        ]
    , describe "removeNode"
        [ fuzz int "Remove a node with a random int id" <|
            \key ->
              empty
                |> insert key
                |> remove key
                |> member key
                |> Expect.false "removed key shouldn't be present"
        , fuzz2 int int "RemoveNode is a no-op if the key doesn't exist" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  graph =
                    empty
                      |> insert a
                in
                  graph
                    |> remove b
                    |> Expect.equal graph
        ]
    , describe "insertNodeData"
        [ fuzz2 int string "InsertNodeData creates the node if it doesn't exist" <|
            \key data ->
              empty
                |> insertData key data
                |> member key
                |> Expect.true "insertNodeData should've inserted a node"
        , fuzz2 int string "Node metadata gets stored in the graph" <|
            \key data ->
              empty
                |> insertData key data
                |> getData key
                |> Expect.equal (Just data)
        , fuzz2 int string "inserting an existing node doesn't remove metadata" <|
            \key data ->
              empty
                |> insertData key data
                |> insert key
                |> getData key
                |> Expect.equal (Just data)
        , fuzz2 int string "Node metadata doesn't have to be comparable" <|
            \key data ->
              empty
                |> insertData key { data = data }
                |> getData key
                |> Expect.equal (Just { data = data })
        ]
    , describe "insertEdge"
        [ fuzz int "Insert a loop" <|
            \key ->
              let
                graph =
                  empty
                    |> insertData key { data = key }
                    |> P.insertEdge ( key, key )
              in
                many
                  [ Expect.notEqual graph empty
                  , Set.singleton key |> Expect.equal (outgoing key graph)
                  , Set.singleton key |> Expect.equal (incoming key graph)
                  , Just { data = key } |> Expect.equal (getData key graph)
                  , Just { data = key } |> Expect.equal (getData key graph)
                  , expectValid graph
                  ]
        , fuzz2 int int "Insert an edge between two existing nodes" <|
            \from to ->
              allDifferent [ from, to ] <|
                let
                  graph =
                    empty
                      |> insertData from { data = from }
                      |> insertData to { data = to }
                      |> P.insertEdge ( from, to )
                in
                  many
                    [ Expect.notEqual graph empty
                    , Set.singleton to |> Expect.equal (outgoing from graph)
                    , Set.singleton from |> Expect.equal (incoming to graph)
                    , Just { data = from } |> Expect.equal (getData from graph)
                    , Just { data = to } |> Expect.equal (getData to graph)
                    , expectValid graph
                    ]
        , fuzz2 int int "Insert an edge with non-existant source node" <|
            \from to ->
              allDifferent [ from, to ] <|
                let
                  graph =
                    empty
                      |> insertData to { data = to }
                      |> P.insertEdge ( from, to )
                in
                  many
                    [ Expect.notEqual graph empty
                    , Set.singleton to |> Expect.equal (outgoing from graph)
                    , Set.singleton from |> Expect.equal (incoming to graph)
                    , Nothing |> Expect.equal (getData from graph)
                    , Just { data = to } |> Expect.equal (getData to graph)
                    , expectValid graph
                    ]
        , fuzz2 int int "Insert an edge with non-existant target node" <|
            \from to ->
              allDifferent [ from, to ] <|
                let
                  graph =
                    empty
                      |> insertData from { data = from }
                      |> P.insertEdge ( from, to )
                in
                  many
                    [ Expect.notEqual graph empty
                    , Set.singleton to |> Expect.equal (outgoing from graph)
                    , Set.singleton from |> Expect.equal (incoming to graph)
                    , Just { data = from } |> Expect.equal (getData from graph)
                    , Nothing |> Expect.equal (getData to graph)
                    , expectValid graph
                    ]
        , fuzz2 int int "Insert an edge between two non-existant nodes" <|
            \from to ->
              allDifferent [ from, to ] <|
                let
                  graph =
                    P.insertEdge ( from, to ) empty
                in
                  many
                    [ Expect.notEqual graph empty
                    , Set.singleton to |> Expect.equal (outgoing from graph)
                    , Set.singleton from |> Expect.equal (incoming to graph)
                    , Nothing |> Expect.equal (getData from graph)
                    , Nothing |> Expect.equal (getData to graph)
                    , expectValid graph
                    ]
        , fuzz (list (tuple ( int, int ))) "InsertEdge handles whatever you throw at it" <|
            \edgeList ->
              List.foldl P.insertEdge empty edgeList
                |> always Expect.pass
        ]
    , describe "member"
        [ fuzz int "Member check for members returns true" <|
            \key ->
              empty
                |> insert key
                |> member key
                |> Expect.true "member should be present"
        , fuzz2 int int "Member check for non-existant members returns false" <|
            \memberKey nonMemberKey ->
              allDifferent [ memberKey, nonMemberKey ] <|
                (empty
                  |> insert memberKey
                  |> member nonMemberKey
                  |> Expect.false "non-member should not be present"
                )
        , fuzz int "Empty graph has no members" <|
            \key ->
              empty
                |> member key
                |> Expect.false "empty graph should not have any members"
        ]
    , describe "memberEdge"
        [ fuzz2 int int "Edge membership check for present edges returns true" <|
            \a b ->
              empty
                |> P.insertEdge ( a, b )
                |> memberEdge ( a, b )
                |> Expect.true "edge should be present"
        , fuzz3 int int int "Edge membership  check for non-present members returns false" <|
            \a b nonMember ->
              allDifferent [ a, b, nonMember ] <|
                let
                  graph =
                    empty
                      |> P.insertEdge ( a, b )
                in
                  many
                    [ graph
                        |> memberEdge ( a, b )
                        |> Expect.true "Edge ( a -> b ) should be present"
                    , graph
                        |> memberEdge ( a, nonMember )
                        |> Expect.false "Edge ( a -> nonMember ) shouldn't be present"
                    , graph
                        |> memberEdge ( b, a )
                        |> Expect.false "Edge ( b -> a ) shouldn't be present"
                    , graph
                        |> memberEdge ( b, nonMember )
                        |> Expect.false "Edge ( b -> nonMember ) shouldn't be present"
                    , graph
                        |> memberEdge ( nonMember, a )
                        |> Expect.false "Edge ( nonMember -> a ) shouldn't be present"
                    , graph
                        |> memberEdge ( nonMember, b )
                        |> Expect.false "Edge ( nonMember -> b ) shouldn't be present"
                    , expectValid graph
                    ]
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
                      |> P.insertEdge ( e, a )
                      |> P.insertEdge ( a, b )
                      |> P.insertEdge ( c, b )
                      |> P.insertEdge ( e, b )
                      |> P.insertEdge ( a, c )
                      |> P.insertEdge ( b, c )
                      |> P.insertEdge ( b, d )
                      |> P.insertEdge ( c, e )
                      |> P.insertEdge ( d, e )
                in
                  many
                    [ incoming a graph |> Expect.equal (Set.fromList [ e ])
                    , incoming b graph |> Expect.equal (Set.fromList [ a, c, e ])
                    , incoming c graph |> Expect.equal (Set.fromList [ a, b ])
                    , incoming d graph |> Expect.equal (Set.fromList [ b ])
                    , incoming e graph |> Expect.equal (Set.fromList [ c, d ])
                    , expectValid graph
                    ]
        ]
    , describe "outgoing"
        [ fuzz5 int int int int int "Outgoing should return outgoing edges" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  graph =
                    empty
                      |> P.insertEdge ( a, b )
                      |> P.insertEdge ( a, c )
                      |> P.insertEdge ( b, c )
                      |> P.insertEdge ( b, d )
                      |> P.insertEdge ( c, b )
                      |> P.insertEdge ( c, e )
                      |> P.insertEdge ( d, e )
                      |> P.insertEdge ( e, a )
                      |> P.insertEdge ( e, b )
                in
                  many
                    [ outgoing a graph |> Expect.equal (Set.fromList [ b, c ])
                    , outgoing b graph |> Expect.equal (Set.fromList [ c, d ])
                    , outgoing c graph |> Expect.equal (Set.fromList [ b, e ])
                    , outgoing d graph |> Expect.equal (Set.fromList [ e ])
                    , outgoing e graph |> Expect.equal (Set.fromList [ a, b ])
                    , expectValid graph
                    ]
        ]
    , describe "removeEdge"
        [ fuzz2 int int "Remove an edge, possibly to itself" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  graph =
                    empty
                      |> insertData a { x = 1 }
                      |> insertData b { x = 2 }
                      |> P.insertEdge ( a, b )

                  removed =
                    graph
                      |> P.removeEdge ( a, b )
                in
                  many
                    [ graph |> Expect.notEqual removed
                    , removed |> getData a |> Expect.equal (Just { x = 1 })
                    , removed |> getData b |> Expect.equal (Just { x = 2 })
                    , removed |> memberEdge ( a, b ) |> Expect.false "shouldn't have an edge from a to b"
                    , removed |> memberEdge ( b, a ) |> Expect.false "shouldn't have an edge from b to a"
                    , expectValid graph
                    ]
        , fuzz2 int int "Removing a node removes all edges to it" <|
            \a b ->
              empty
                |> P.insertEdge ( a, b )
                |> remove b
                |> outgoing a
                |> Set.member b
                |> Expect.false "edge to removed node shouldn't be present"
        , fuzz2 int int "Removing a node removes all edges from it" <|
            \a b ->
              empty
                |> P.insertEdge ( a, b )
                |> remove a
                |> incoming b
                |> Set.member a
                |> Expect.false "edge from removed node shouldn't be present"
        , test "Remove an edge between non-existant nodes is a no-op" <|
            \() ->
              empty
                |> P.removeEdge ( 47, 11 )
                |> Expect.equal empty
        , fuzz2 int int "Remove an edge with non-existant source node is a no-op" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  graph =
                    empty
                      |> insert b
                in
                  graph |> P.removeEdge ( a, b ) |> Expect.equal graph
        , fuzz2 int int "Remove an edge with non-existant target node is a no-op" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  graph =
                    empty
                      |> insert a
                in
                  graph |> P.removeEdge ( a, b ) |> Expect.equal graph
        ]
    , describe "size"
        [ fuzz (list int) "Size equals the number of unique keys inserted " <|
            \keys ->
              List.foldl insert empty keys
                |> size
                |> Expect.equal (Set.fromList keys |> Set.size)
        ]
    , describe "nodes"
        [ fuzz (list int) "nodes returns all inserted keys" <|
            \keys ->
              List.foldl insert empty keys
                |> nodes
                |> List.map Tuple.first
                |> Set.fromList
                |> Expect.equal (Set.fromList keys)
        , fuzz (list int) "nodes returns all inserted keys, with metadata" <|
            \keys ->
              List.foldl (\key -> insertData key ( key, key )) empty keys
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
                    List.foldl P.insertEdge empty graphEdges
                in
                  edges graph
                    |> Set.fromList
                    |> Expect.equal (Set.fromList graphEdges)
        ]
    , describe "map"
        [ fuzz5 int int int int int "Use map to modify all data fields" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  graph =
                    empty
                      |> P.insertEdge ( a, b )
                      |> P.insertEdge ( a, c )
                      |> P.insertEdge ( b, d )
                      |> P.insertEdge ( c, d )
                      |> P.insertEdge ( d, e )
                      |> P.insertEdge ( e, a )
                      |> insertData a a
                      |> insertData b b
                      |> insertData e e

                  nodesBefore =
                    nodes graph

                  func =
                    (\key maybeData -> maybeData |> Maybe.map (\a -> a + 1))
                in
                  graph
                    |> map func
                    |> nodes
                    |> Expect.equal
                        (List.map
                          (\( a, b ) ->
                            ( a, (Maybe.map (\x -> x + 1) b) )
                          )
                         <|
                          nodes <|
                            graph
                        )
        ]
    , describe "foldl"
        [ fuzz (list int) "use foldl to sum up data in all nodes" <|
            \keys ->
              let
                graph =
                  List.foldl (\key -> insertData key key) empty keys
              in
                graph
                  |> foldl (\key data acc -> (data |> Maybe.withDefault 0) + acc) 0
                  |> Expect.equal
                      (List.sum <|
                        List.map (\( x, d ) -> d |> Maybe.withDefault 0) <|
                          nodes <|
                            graph
                      )
        , test "foldl yields keys in ascending order" <|
            \() ->
              let
                graph =
                  empty
                    |> insert 1
                    |> insert 2
              in
                graph
                  |> foldl (\key data acc -> key :: acc) []
                  |> List.reverse
                  |> Expect.equal [ 1, 2 ]
        , fuzz (list int) "use foldl to get all keys in order" <|
            \keys ->
              let
                graph =
                  List.foldl insert empty keys
              in
                graph
                  |> foldl (\key data acc -> key :: acc) []
                  |> Expect.equal
                      (List.map (\( x, d ) -> x) <|
                        nodes <|
                          graph
                      )
        , fuzz (list int) "foldl and foldr return keys in opposite order" <|
            \keys ->
              let
                graph =
                  List.foldl insert empty keys
              in
                graph
                  |> foldl (\key data acc -> key :: acc) []
                  |> List.reverse
                  |> Expect.equal
                      (foldr (\key data acc -> key :: acc) [] graph)
        ]
    , describe "partition"
        [ fuzz2 int int "Partition doesn't leave invalid edges" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  ( left, right ) =
                    empty
                      |> P.insertEdge ( a, b )
                      |> partition (\key _ -> key == a)
                in
                  many
                    [ left |> nodes |> List.map Tuple.first |> Expect.equal [ a ]
                    , left |> incoming a |> Expect.equal Set.empty
                    , left |> outgoing a |> Expect.equal Set.empty
                    , left |> edges |> List.map Tuple.first |> Expect.equal []
                    , right |> nodes |> List.map Tuple.first |> Expect.equal [ b ]
                    , right |> incoming a |> Expect.equal Set.empty
                    , right |> outgoing a |> Expect.equal Set.empty
                    , right |> edges |> List.map Tuple.first |> Expect.equal []
                    , expectValid left
                    , expectValid right
                    ]
        , fuzz5 int int int int int "partition preserves all data on edges" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  leftGraph =
                    empty
                      |> insertData a { x = a }
                      |> insertData b { x = b }
                      |> insertData c { x = c }
                      |> P.insertEdge ( a, b )
                      |> P.insertEdge ( a, c )

                  rightGraph =
                    empty
                      |> insertData d { x = d }
                      |> insertData e { x = e }
                      |> P.insertEdge ( d, e )

                  graph =
                    rightGraph
                      |> insertData a { x = a }
                      |> insertData b { x = b }
                      |> insertData c { x = c }
                      |> P.insertEdge ( a, b )
                      |> P.insertEdge ( a, c )
                      |> P.insertEdge ( c, d )

                  ( left, right ) =
                    graph
                      |> partition (\key _ -> key == a || key == b || key == c)
                in
                  many
                    [ left |> Expect.equal leftGraph
                    , right |> Expect.equal rightGraph
                    , left |> getData a |> Expect.equal (Just { x = a })
                    , left |> getData b |> Expect.equal (Just { x = b })
                    , left |> getData c |> Expect.equal (Just { x = c })
                    , right |> getData d |> Expect.equal (Just { x = d })
                    , right |> getData e |> Expect.equal (Just { x = e })
                    , expectValid leftGraph
                    , expectValid rightGraph
                    , expectValid graph
                    ]
        , fuzz5 int int int int int "partition by always returning true yields a graph equal to input" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  graph =
                    empty
                      |> insertData a { x = a }
                      |> insertData b { x = b }
                      |> insertData c { x = c }
                      |> insertData d { x = d }
                      |> insertData e { x = e }
                      |> P.insertEdge ( a, b )
                      |> P.insertEdge ( a, c )
                      |> P.insertEdge ( c, d )
                      |> P.insertEdge ( d, e )

                  ( left, right ) =
                    graph
                      |> partition (\_ _ -> True)
                in
                  many
                    [ left |> Expect.equal graph
                    , right |> Expect.equal empty
                    , expectValid left
                    , expectValid right
                    ]
        ]
    , describe "union"
        [ fuzz5 int int int int int "union of non-conflicting graphs" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  leftGraph =
                    empty
                      |> insertData a { x = a }
                      |> insertData b { x = b }
                      |> insertData c { x = c }
                      |> P.insertEdge ( a, b )
                      |> P.insertEdge ( a, c )

                  rightGraph =
                    empty
                      |> insertData d { x = d }
                      |> insertData e { x = e }
                      |> P.insertEdge ( d, e )

                  graph =
                    leftGraph
                      |> insertData d { x = d }
                      |> insertData e { x = e }
                      |> P.insertEdge ( d, e )

                  graphUnion =
                    leftGraph |> union rightGraph
                in
                  graphUnion |> Expect.equal graph
        , fuzz2 int int "union of conflicting graphs preserves node data" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  leftGraph =
                    empty
                      |> insertData a { x = a }

                  rightGraph =
                    empty
                      |> insertData b { x = b }

                  graph =
                    leftGraph
                      |> insertData a { x = a }
                      |> insertData b { x = b }
                      |> P.insertEdge ( a, b )

                  graphUnionLeft =
                    leftGraph
                      |> P.insertEdge ( a, b )
                      |> union rightGraph

                  graphUnionRight =
                    rightGraph
                      |> P.insertEdge ( a, b )
                      |> union leftGraph

                  graphUnionEdgeLast =
                    leftGraph
                      |> union rightGraph
                      |> P.insertEdge ( a, b )

                  nodesAnswer =
                    List.sortBy (\( a, _ ) -> a)
                      [ ( a, Just { x = a } )
                      , ( b, Just { x = b } )
                      ]
                in
                  many
                    [ graphUnionLeft |> Expect.equal graph
                    , graphUnionRight |> Expect.equal graph
                    , graphUnionEdgeLast |> Expect.equal graph
                    , graphUnionLeft
                        |> nodes
                        |> List.sortBy (\( a, _ ) -> a)
                        |> Expect.equal nodesAnswer
                    , graphUnionRight
                        |> nodes
                        |> List.sortBy (\( a, _ ) -> a)
                        |> Expect.equal nodesAnswer
                    , graphUnionEdgeLast
                        |> nodes
                        |> List.sortBy (\( a, _ ) -> a)
                        |> Expect.equal nodesAnswer
                    , expectValid leftGraph
                    , expectValid rightGraph
                    , expectValid graph
                    , expectValid graphUnionLeft
                    , expectValid graphUnionRight
                    , expectValid graphUnionEdgeLast
                    ]
        , fuzz2 int int "union prefers metadata from left graph" <|
            \a b ->
              let
                left =
                  empty |> insertData a a

                right =
                  empty |> insertData a b
              in
                union left right |> getData a |> Expect.equal (Just a)
        ]
    , describe "intersect"
        [ fuzz2 int int "intersect keeps only intersected nodes" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  subgraph =
                    empty |> insert a

                  graph =
                    subgraph
                      |> insert b
                in
                  intersect graph subgraph |> Expect.equal subgraph
        , fuzz2 int int "intersect keeps only equal metadata" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  subgraph =
                    empty |> insertData a { x = a }

                  graph =
                    subgraph
                      |> insertData b { x = b }
                in
                  intersect graph subgraph
                    |> nodes
                    |> Expect.equal [ ( a, Just { x = a } ) ]
        , fuzz int "intersecting nodes where one has metadata drops metadata" <|
            \a ->
              let
                left =
                  empty |> insertData a { x = a }

                right =
                  empty |> insert a
              in
                intersect left right
                  |> nodes
                  |> Expect.equal [ ( a, Nothing ) ]
        , fuzz3 int int int "intersect keeps only intersected edges" <|
            \a b c ->
              allDifferent [ a, b, c ] <|
                let
                  subgraph =
                    empty |> P.insertEdge ( a, b )

                  graph =
                    subgraph
                      |> insert c
                      |> P.insertEdge ( b, c )
                in
                  intersect graph subgraph |> edges |> Expect.equal (edges subgraph)
        , fuzz5 int int int int int "intersection of larger graphs" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  leftGraph =
                    empty
                      |> insertData a { x = a }
                      |> insertData b { x = b }
                      |> insertData c { x = c }
                      |> P.insertEdge ( a, b )
                      |> P.insertEdge ( a, c )

                  graph =
                    leftGraph
                      |> insertData d { x = d }
                      |> insertData e { x = e }
                      |> P.insertEdge ( d, e )
                in
                  many
                    [ graph |> intersect leftGraph |> Expect.equal leftGraph
                    , expectValid graph
                    , expectValid leftGraph
                    ]
        ]
    , describe "postOrder"
        [ test "postOrder handles edgeless graphs" <|
            \() ->
              let
                graph =
                  empty
                    |> insert 1
                    |> insert 2
                    |> insert 3
                    |> insert 4
              in
                postOrder graph |> Expect.equal [ 1, 2, 3, 4 ]
        , test "postOrder yields keys in post order for trees" <|
            \() ->
              let
                graph =
                  empty
                    |> P.insertEdge ( 1, 2 )
                    |> P.insertEdge ( 1, 3 )
                    |> P.insertEdge ( 2, 4 )
                    |> P.insertEdge ( 2, 5 )
              in
                postOrder graph |> Expect.equal [ 4, 5, 2, 3, 1 ]
        , test "postOrder yields keys in post order for DAG's" <|
            \() ->
              let
                graph =
                  empty
                    |> P.insertEdge ( 1, 2 )
                    |> P.insertEdge ( 1, 3 )
                    |> P.insertEdge ( 2, 4 )
                    |> P.insertEdge ( 3, 4 )
              in
                -- this is not the only valid ordering, but it's the one our implementation gives.
                postOrder graph |> Expect.equal [ 4, 2, 3, 1 ]
        , test "postOrder yields keys in post order for large DAG's" <|
            \() ->
              let
                graph =
                  empty
                    |> P.insertEdge ( 1, 2 )
                    |> P.insertEdge ( 1, 3 )
                    |> P.insertEdge ( 1, 4 )
                    |> P.insertEdge ( 2, 3 )
                    |> P.insertEdge ( 2, 5 )
                    |> P.insertEdge ( 3, 5 )
                    |> P.insertEdge ( 6, 8 )
                    |> P.insertEdge ( 7, 8 )
                    |> P.insertEdge ( 5, 9 )
                    |> P.insertEdge ( 8, 9 )
                    |> P.insertEdge ( 9, 10 )
              in
                -- this is not the only valid ordering, but it's the one our implementation gives.
                postOrder graph |> Expect.equal [ 10, 9, 5, 3, 2, 4, 1, 8, 6, 7 ]
        , test "postOrder gives a valid post order for cyclic graphs" <|
            \() ->
              let
                graph =
                  empty
                    |> P.insertEdge ( 1, 2 )
                    |> P.insertEdge ( 2, 3 )
                    |> P.insertEdge ( 3, 1 )
                    |> P.insertEdge ( 3, 4 )
              in
                -- this is far from the only possible answer, but 4 always comes before 3
                postOrder graph |> Expect.equal [ 4, 3, 2, 1 ]
        ]
    , describe "topSort"
        [ test "topSort handles empty graphs" <|
            \() -> empty |> topologicalSort |> Expect.equal (Just [])
        , fuzz acyclicGraphFuzzer "topSort doesn't violate any partial orderings" <|
            \g ->
              let
                graph =
                  g
              in
                case graph |> topologicalSort of
                  Nothing ->
                    Expect.fail "Failed to generate a valid topological sort; is input acyclic?"

                  Just order ->
                    order |> (checkPartialOrdering (edges graph))
        , test "topSort doesn't return an ordering if there are cycles" <|
            \() ->
              let
                graph =
                  empty
                    |> P.insertEdge ( 1, 2 )
                    |> P.insertEdge ( 2, 3 )
                    |> P.insertEdge ( 3, 1 )
              in
                case graph |> topologicalSort of
                  Nothing ->
                    Expect.pass

                  Just order ->
                    Expect.fail ("Got an ordering despite input being cyclical" ++ toString order)
        , test "topSort doesn't return an ordering if there are loops" <|
            \() ->
              let
                graph =
                  empty
                    |> P.insertEdge ( 1, 1 )
              in
                case graph |> topologicalSort of
                  Nothing ->
                    Expect.pass

                  Just order ->
                    Expect.fail ("Got an ordering despite input having loops" ++ toString order)
        ]
    , describe "isAcyclic"
        [ test "isAcyclic returns True for empty graph" <|
            \() ->
              empty
                |> isAcyclic
                |> Expect.true "empty graph should be acyclic"
        , test "isAcyclic returns True for single edge" <|
            \() ->
              empty
                |> P.insertEdge ( 0, 1 )
                |> isAcyclic
                |> Expect.true "directed acyclic graph should be acyclic"
        , test "isAcyclic returns True for simple tree" <|
            \() ->
              empty
                |> P.insertEdge ( 0, 1 )
                |> P.insertEdge ( 0, 2 )
                |> isAcyclic
                |> Expect.true "directed acyclic graph should be acyclic"
        , fuzz acyclicGraphFuzzer "isAcyclic returns True for DAG's" <|
            \graph ->
              (graph
                |> isAcyclic
                |> Expect.true "directed acyclic graph should be acyclic"
              )
        , fuzz acyclicGraphFuzzerWithSelfEdges "isAcyclic returns False for graphs with loops but no cycles" <|
            \graph ->
              if size graph == 0 then
                Expect.pass
              else
                -- make sure there is at least one loop in the graph
                graph
                  |> P.insertEdge ( -1, -1 )
                  |> isAcyclic
                  |> Expect.false "graph with loops is not acyclic"
        , fuzz acyclicGraphFuzzerWithSelfEdges "isAcyclic returns False for graphs with cycles but no loops" <|
            \graph ->
              if size graph == 0 then
                Expect.pass
              else
                graph
                  -- remove loops
                  |>
                    edges
                  |> List.filter (uncurry (==))
                  |> List.foldl P.removeEdge graph
                  -- make sure we have a cycle in the graph by adding negative edges
                  |>
                    P.insertEdge ( -1, -2 )
                  |> P.insertEdge ( -2, -1 )
                  |> isAcyclic
                  |> Expect.false "graph with cycles is not acyclic"
        ]
    ]
