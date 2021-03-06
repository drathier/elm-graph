module Graph.Random exposing
    ( topologicalSortFuzzer
    , randomTopologicalSort
    )

{-| Graph.Random provides graph functions that require randomness to work.


# Elm-test fuzzers

@docs topologicalSortFuzzer


# Random

@docs randomTopologicalSort

-}

import Fuzz
import Graph exposing (Graph, isAcyclic, keys, outgoing)
import Random
import Random.List
import Set exposing (Set)
import Shrink


{-| Elm-test fuzzer that generates random topological sortings of a _directed acyclic graph_. Hangs if the graph contains cycles.
-}
topologicalSortFuzzer : Graph comparable data edgeData -> Fuzz.Fuzzer (List comparable)
topologicalSortFuzzer graph =
    Fuzz.custom (randomTopologicalSort graph) topologicalSortShrinker


{-| Generate a random topological sorting of a _directed acyclic graph_. Hangs if the graph contains cycles.
-}
randomTopologicalSort : Graph comparable data edgeData -> Random.Generator (List comparable)
randomTopologicalSort graph =
    (keys graph |> Random.List.shuffle)
        |> Random.andThen (\nodeKeys -> randomReversePostOrderHelper nodeKeys [] Set.empty graph)
        |> Random.map Tuple.second


randomReversePostOrderHelper : List comparable -> List comparable -> Set comparable -> Graph comparable data edgeData -> Random.Generator ( Set comparable, List comparable )
randomReversePostOrderHelper nodeKeys keyOrder seenKeys graph =
    case nodeKeys of
        [] ->
            Random.constant ( seenKeys, keyOrder )

        key :: keys ->
            if Set.member key seenKeys then
                randomReversePostOrderHelper keys keyOrder seenKeys graph

            else
                let
                    midstep : List comparable -> Random.Generator ( Set comparable, List comparable )
                    midstep shuffledOut =
                        randomReversePostOrderHelper
                            shuffledOut
                            keyOrder
                            (Set.insert key seenKeys)
                            graph

                    lastStep : ( Set comparable, List comparable ) -> Random.Generator ( Set comparable, List comparable )
                    lastStep ( seen, order ) =
                        randomReversePostOrderHelper keys (key :: order) seen graph
                in
                (outgoing key graph |> Set.toList |> Random.List.shuffle)
                    |> Random.andThen midstep
                    |> Random.andThen lastStep


{-| Shrink a topological sorting by dropping elements.
-}
topologicalSortShrinker : Shrink.Shrinker (List a)
topologicalSortShrinker =
    -- don't reorder elements, just drop them
    Shrink.list Shrink.noShrink
