module RandomTests exposing (randomTests)

import Expect
import Fuzz exposing (int, list, string, tuple)
import Graph exposing (edges, empty)
import Graph.Pair exposing (insertEdge)
import Graph.Random exposing (topologicalSortFuzzer)
import Random
import Set
import Shrink
import Test exposing (..)
import TestUtils exposing (checkPartialOrdering)


randomTests : Test
randomTests =
    describe "Random tests"
        [ describe "insertNode"
            (let
                graph =
                    empty
                        |> insertEdge ( 10, 30 )
                        |> insertEdge ( 11, 30 )
                        |> insertEdge ( 12, 30 )
                        |> insertEdge ( 13, 30 )
                        |> insertEdge ( 14, 30 )
                        |> insertEdge ( 15, 30 )
                        |> insertEdge ( 16, 30 )
                        |> insertEdge ( 17, 30 )
                        |> insertEdge ( 18, 30 )
                        |> insertEdge ( 19, 30 )
                        --
                        |> insertEdge ( 30, 50 )
                        |> insertEdge ( 30, 51 )
                        |> insertEdge ( 30, 52 )
                        |> insertEdge ( 30, 53 )
                        |> insertEdge ( 30, 54 )
                        |> insertEdge ( 30, 55 )
                        |> insertEdge ( 30, 56 )
                        |> insertEdge ( 30, 57 )
                        |> insertEdge ( 30, 58 )
                        |> insertEdge ( 30, 59 )
                        --
                        |> insertEdge ( 50, 70 )
                        |> insertEdge ( 51, 70 )
                        |> insertEdge ( 52, 70 )
                        |> insertEdge ( 53, 70 )
                        |> insertEdge ( 54, 70 )
                        |> insertEdge ( 55, 70 )
                        |> insertEdge ( 56, 70 )
                        |> insertEdge ( 57, 70 )
                        |> insertEdge ( 58, 70 )
                        |> insertEdge ( 59, 70 )
                        --
                        |> insertEdge ( 70, 90 )
                        |> insertEdge ( 70, 91 )
                        |> insertEdge ( 70, 92 )
                        |> insertEdge ( 70, 93 )
                        |> insertEdge ( 70, 94 )
                        |> insertEdge ( 70, 95 )
                        |> insertEdge ( 70, 96 )
                        |> insertEdge ( 70, 97 )
                        |> insertEdge ( 70, 98 )
                        |> insertEdge ( 70, 99 )
             in
             [ fuzz2 (topologicalSortFuzzer graph) (topologicalSortFuzzer graph) "Random toposort yields different values" <|
                -- probability of collision is around 1.37e-6
                \a b ->
                    a |> Expect.notEqual b
             , fuzz (topologicalSortFuzzer graph)
                "Random toposort yields a valid toposort"
                (\order ->
                    checkPartialOrdering (edges graph) order
                )

             {- , fuzz (topologicalSortFuzzer graph)
                "Fuzzer correctly finds list [53] as minimal input to trigger error"
                (\order ->
                  Debug.log (toString ( "test", order )) <|
                    if order |> List.member 11 then
                      Expect.fail "found 11"
                    else if order |> List.member 53 |> not then
                      Expect.pass
                    else
                      Expect.fail "yay"
                )
             -}
             ]
            )
        ]
