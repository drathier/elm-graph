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
    ]
