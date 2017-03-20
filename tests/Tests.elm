module Tests exposing (..)

import List.Extra
import Maybe.Extra
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Graph exposing (..)
import Set
import GraphFuzzer exposing (acyclicGraphFuzzer, acyclicGraphFuzzerWithSelfEdges, graphFuzzer)


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


-- TopSort


checkComesBefore : a -> a -> List a -> Expect.Expectation
checkComesBefore first second list =
  if first == second then
    Expect.pass
  else
    case list of
      [] ->
        Expect.fail ("expected to find " ++ toString first)

      head :: tail ->
        if head == first then
          List.member second tail |> Expect.true ("expected " ++ toString first ++ " to come before " ++ toString second)
        else
          checkComesBefore first second tail


checkPartialOrdering : List ( a, a ) -> List a -> Expect.Expectation
checkPartialOrdering constraintList ordering =
  (constraintList
    |> List.map (\( a, b ) -> checkComesBefore a b ordering)
    |> many
  )


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
        , fuzz2 int int "RemoveNode is a no-op if the key doesn't exist" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  graph =
                    empty
                      |> insertNode a
                in
                  graph
                    |> removeNode b
                    |> Expect.equal graph
        ]
    , describe "insertNodeData"
        [ fuzz2 int string "InsertNodeData creates the node if it doesn't exist" <|
            \key data ->
              empty
                |> insertNodeData key data
                |> member key
                |> Expect.true "insertNodeData should've inserted a node"
        , fuzz2 int string "Node metadata gets stored in the graph" <|
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
    , describe "insertEdge"
        [ fuzz2 int int "Insert an edge between two existing nodes" <|
            \from to ->
              allDifferent [ from, to ] <|
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
              allDifferent [ from, to ] <|
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
              allDifferent [ from, to ] <|
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
              allDifferent [ from, to ] <|
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
    , describe "member"
        [ fuzz int "Member check for members returns true" <|
            \key ->
              empty
                |> insertNode key
                |> member key
                |> Expect.true "member should be present"
        , fuzz2 int int "Member check for non-existant members returns false" <|
            \memberKey nonMemberKey ->
              allDifferent [ memberKey, nonMemberKey ] <|
                (empty
                  |> insertNode memberKey
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
                |> insertEdge ( a, b )
                |> memberEdge ( a, b )
                |> Expect.true "edge should be present"
        , fuzz3 int int int "Edge membership  check for non-present members returns false" <|
            \a b nonMember ->
              allDifferent [ a, b, nonMember ] <|
                let
                  graph =
                    empty
                      |> insertEdge ( a, b )
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
    , describe "removeEdge"
        [ fuzz2 int int "Remove an edge, possibly to itself" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  graph =
                    empty
                      |> insertNodeData a { x = 1 }
                      |> insertNodeData b { x = 2 }
                      |> insertEdge ( a, b )

                  removed =
                    graph
                      |> removeEdge ( a, b )
                in
                  many
                    [ graph |> Expect.notEqual removed
                    , removed |> getData a |> Expect.equal (Just { x = 1 })
                    , removed |> getData b |> Expect.equal (Just { x = 2 })
                    , removed |> memberEdge ( a, b ) |> Expect.false "shouldn't have an edge from a to b"
                    , removed |> memberEdge ( b, a ) |> Expect.false "shouldn't have an edge from b to a"
                    ]
        , fuzz2 int int "Removing a node removes all edges to it" <|
            \a b ->
              empty
                |> insertEdge ( a, b )
                |> removeNode b
                |> outgoing a
                |> Set.member b
                |> Expect.false "edge to removed node shouldn't be present"
        , fuzz2 int int "Removing a node removes all edges from it" <|
            \a b ->
              empty
                |> insertEdge ( a, b )
                |> removeNode a
                |> incoming b
                |> Set.member a
                |> Expect.false "edge from removed node shouldn't be present"
        , test "Remove an edge between non-existant nodes is a no-op" <|
            \() ->
              empty |> removeEdge ( 47, 11 ) |> Expect.equal empty
        , fuzz2 int int "Remove an edge with non-existant source node is a no-op" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  graph =
                    empty
                      |> insertNode b
                in
                  graph |> removeEdge ( a, b ) |> Expect.equal graph
        , fuzz2 int int "Remove an edge with non-existant target node is a no-op" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  graph =
                    empty
                      |> insertNode a
                in
                  graph |> removeEdge ( a, b ) |> Expect.equal graph
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
    , describe "map"
        [ fuzz5 int int int int int "Use map to modify all data fields" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  graph =
                    empty
                      |> insertEdge ( a, b )
                      |> insertEdge ( a, c )
                      |> insertEdge ( b, d )
                      |> insertEdge ( c, d )
                      |> insertEdge ( d, e )
                      |> insertEdge ( e, a )
                      |> insertNodeData a a
                      |> insertNodeData b b
                      |> insertNodeData e e

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
                  List.foldl (\key -> insertNodeData key key) empty keys
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
                    |> insertNode 1
                    |> insertNode 2
              in
                graph
                  |> foldl (\key data acc -> key :: acc) []
                  |> List.reverse
                  |> Expect.equal [ 1, 2 ]
        , fuzz (list int) "use foldl to get all keys in order" <|
            \keys ->
              let
                graph =
                  List.foldl insertNode empty keys
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
                  List.foldl insertNode empty keys
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
                      |> insertEdge ( a, b )
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
                    ]
        , fuzz5 int int int int int "partition preserves all data on edges" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  leftGraph =
                    empty
                      |> insertNodeData a { x = a }
                      |> insertNodeData b { x = b }
                      |> insertNodeData c { x = c }
                      |> insertEdge ( a, b )
                      |> insertEdge ( a, c )

                  rightGraph =
                    empty
                      |> insertNodeData d { x = d }
                      |> insertNodeData e { x = e }
                      |> insertEdge ( d, e )

                  graph =
                    rightGraph
                      |> insertNodeData a { x = a }
                      |> insertNodeData b { x = b }
                      |> insertNodeData c { x = c }
                      |> insertEdge ( a, b )
                      |> insertEdge ( a, c )
                      |> insertEdge ( c, d )

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
                    ]
        , fuzz5 int int int int int "partition by always returning true yields a graph equal to input" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  graph =
                    empty
                      |> insertNodeData a { x = a }
                      |> insertNodeData b { x = b }
                      |> insertNodeData c { x = c }
                      |> insertNodeData d { x = d }
                      |> insertNodeData e { x = e }
                      |> insertEdge ( a, b )
                      |> insertEdge ( a, c )
                      |> insertEdge ( c, d )
                      |> insertEdge ( d, e )

                  ( left, right ) =
                    graph
                      |> partition (\_ _ -> True)
                in
                  many
                    [ left |> Expect.equal (graph)
                    , right |> Expect.equal (empty)
                    ]
        ]
    , describe "union"
        [ fuzz5 int int int int int "union of non-conflicting graphs" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  leftGraph =
                    empty
                      |> insertNodeData a { x = a }
                      |> insertNodeData b { x = b }
                      |> insertNodeData c { x = c }
                      |> insertEdge ( a, b )
                      |> insertEdge ( a, c )

                  rightGraph =
                    empty
                      |> insertNodeData d { x = d }
                      |> insertNodeData e { x = e }
                      |> insertEdge ( d, e )

                  graph =
                    leftGraph
                      |> insertNodeData d { x = d }
                      |> insertNodeData e { x = e }
                      |> insertEdge ( d, e )

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
                      |> insertNodeData a { x = a }

                  rightGraph =
                    empty
                      |> insertNodeData b { x = b }

                  graph =
                    leftGraph
                      |> insertNodeData a { x = a }
                      |> insertNodeData b { x = b }
                      |> insertEdge ( a, b )

                  graphUnionLeft =
                    leftGraph
                      |> insertEdge ( a, b )
                      |> union rightGraph

                  graphUnionRight =
                    rightGraph
                      |> insertEdge ( a, b )
                      |> union leftGraph

                  graphUnionEdgeLast =
                    leftGraph
                      |> union rightGraph
                      |> insertEdge ( a, b )

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
                    ]
        , fuzz2 int int "union prefers metadata from left graph" <|
            \a b ->
              let
                left =
                  empty |> insertNodeData a a

                right =
                  empty |> insertNodeData a b
              in
                union left right |> getData a |> Expect.equal (Just a)
        ]
    , describe "intersect"
        [ fuzz2 int int "intersect keeps only intersected nodes" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  subgraph =
                    empty |> insertNode a

                  graph =
                    subgraph
                      |> insertNode b
                in
                  intersect graph subgraph |> Expect.equal subgraph
        , fuzz2 int int "intersect keeps only equal metadata" <|
            \a b ->
              allDifferent [ a, b ] <|
                let
                  subgraph =
                    empty |> insertNodeData a { x = a }

                  graph =
                    subgraph
                      |> insertNodeData b { x = b }
                in
                  intersect graph subgraph
                    |> nodes
                    |> Expect.equal [ ( a, Just { x = a } ) ]
        , fuzz int "intersecting nodes where one has metadata drops metadata" <|
            \a ->
              let
                left =
                  empty |> insertNodeData a { x = a }

                right =
                  empty |> insertNode a
              in
                intersect left right
                  |> nodes
                  |> Expect.equal [ ( a, Nothing ) ]
        , fuzz3 int int int "intersect keeps only intersected edges" <|
            \a b c ->
              allDifferent [ a, b, c ] <|
                let
                  subgraph =
                    empty |> insertEdge ( a, b )

                  graph =
                    subgraph
                      |> insertNode c
                      |> insertEdge ( b, c )
                in
                  intersect graph subgraph |> edges |> Expect.equal (edges subgraph)
        , fuzz5 int int int int int "intersection of larger graphs" <|
            \a b c d e ->
              allDifferent [ a, b, c, d, e ] <|
                let
                  leftGraph =
                    empty
                      |> insertNodeData a { x = a }
                      |> insertNodeData b { x = b }
                      |> insertNodeData c { x = c }
                      |> insertEdge ( a, b )
                      |> insertEdge ( a, c )

                  graph =
                    leftGraph
                      |> insertNodeData d { x = d }
                      |> insertNodeData e { x = e }
                      |> insertEdge ( d, e )
                in
                  graph |> intersect leftGraph |> Expect.equal leftGraph
        ]
    , describe "topologicalSort"
        [ {- test "topologicalSort handles edgeless graphs" <|
                       \() ->
                         let
                           graph =
                             empty
                               |> insertNode 1
                               |> insertNode 2
                               |> insertNode 3
                               |> insertNode 4
                         in
                           topologicalSort graph |> Expect.equal [ 4, 3, 2, 1 ]
                     ,
                  test "topologicalSort yields keys in reverse post order for trees" <|
                    \() ->
                      let
                        graph =
                          empty
                            |> insertEdge ( 1, 2 )
                            |> insertEdge ( 1, 3 )
                            |> insertEdge ( 2, 4 )
                            |> insertEdge ( 2, 5 )
                      in
                        topologicalSort graph |> Expect.equal [ 1, 3, 2, 5, 4 ]
                   ,
               test "topologicalSort yields keys in reverse post order for DAG's" <|
                 \() ->
                   let
                     graph =
                       empty
                         |> insertEdge ( 1, 2 )
                         |> insertEdge ( 1, 3 )
                         |> insertEdge ( 2, 4 )
                         |> insertEdge ( 3, 4 )
                   in
                     -- this is not the only valid ordering, but it's the one our implementation gives.
                     topologicalSort graph |> Expect.equal [ 1, 3, 2, 4 ]
             ,
          -}
          test "topologicalSort yields keys in reverse post order for large DAG's" <|
            \() ->
              let
                graph =
                  empty
                    |> insertEdge ( 1, 2 )
                    |> insertEdge ( 1, 3 )
                    |> insertEdge ( 1, 4 )
                    |> insertEdge ( 2, 3 )
                    |> insertEdge ( 2, 5 )
                    |> insertEdge ( 3, 5 )
                    |> insertEdge ( 6, 8 )
                    |> insertEdge ( 7, 8 )
                    |> insertEdge ( 5, 9 )
                    |> insertEdge ( 8, 9 )
                    |> insertEdge ( 9, 10 )
              in
                -- this is not the only valid ordering, but it's the one our implementation gives.
                topologicalSort graph |> Expect.equal [ 7, 6, 8, 1, 4, 2, 3, 5, 9, 10 ]
          {- , test "topologicalSort gives a somewhat valid reverse post order for cyclic graphs that at least contains all keys, and is only unpredictable on cycles with random keys" <|
             \() ->
               let
                 graph =
                   empty
                     |> insertEdge ( 1, 2 )
                     |> insertEdge ( 2, 3 )
                     |> insertEdge ( 3, 1 )
                     |> insertEdge ( 3, 4 )
               in
                 topologicalSort graph |> Expect.equal [ 1, 2, 3, 4 ]
          -}
        ]
      {- , describe "topSort"
         [ fuzz acyclicGraphFuzzer "topSort doesn't violate any partial orderings" <|
             \graph ->
               graph |> topologicalSort |> checkPartialOrdering (edges graph)
         ]
      -}
    , describe "isAcyclic"
        [ test "isAcyclic returns True for simple DAG" <|
            \() ->
              empty
                |> insertEdge ( 0, 1 )
                |> isAcyclic
                |> Expect.true "directed acyclic graph should be acyclic"
        , fuzz acyclicGraphFuzzer "isAcyclic returns True for DAG's" <|
            \graph ->
              isAcyclic graph |> Expect.true "directed acyclic graph should be acyclic"
        , fuzz acyclicGraphFuzzerWithSelfEdges "isAcyclic returns False for graphs with loops but no cycles" <|
            \graph ->
              if size graph == 0 then
                Expect.pass
              else
                isAcyclic graph |> Expect.false "graph with loops is not acyclic"
        , fuzz acyclicGraphFuzzerWithSelfEdges "isAcyclic returns False for graphs with cycles but no loops" <|
            \graph ->
              if size graph == 0 then
                Expect.pass
              else
                isAcyclic graph |> Expect.false "graph with cycles is not acyclic"
        ]
    ]
