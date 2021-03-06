module GraphFuzzer exposing (acyclicGraphFuzzer, acyclicGraphFuzzerWithSelfEdges, edgeGenerator, graphFuzzer, graphGenerator, graphShrinker, product)

import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Graph as G exposing (Graph)
import Graph.Pair as GP
import List.Extra
import Random exposing (..)
import Random.Extra exposing (..)
import Set
import Shrink exposing (Shrinker)



-- Fuzzing


acyclicGraphFuzzer : Fuzzer (Graph Int data edgeData)
acyclicGraphFuzzer =
    Fuzz.custom (graphGenerator (<)) (graphShrinker (<))


acyclicGraphFuzzerWithSelfEdges : Fuzzer (Graph Int data edgeData)
acyclicGraphFuzzerWithSelfEdges =
    Fuzz.custom (graphGenerator (<=)) (graphShrinker (<=))


graphFuzzer : Fuzzer (Graph Int data edgeData)
graphFuzzer =
    Fuzz.custom (graphGenerator (\_ _ -> True)) (graphShrinker (\_ _ -> True))



-- Shrinking


graphShrinker : (Int -> Int -> Bool) -> Shrinker (Graph Int data edgeData)
graphShrinker edgePredicate =
    let
        toGraph =
            List.foldl GP.insertEdge G.empty

        fromGraph =
            G.edges

        intRange low high =
            Shrink.keepIf (\i -> low <= i && i <= high) Shrink.int
    in
    -- NOTE: only handles edges; nothing else
    Shrink.tuple ( intRange 0 20, intRange 0 20 )
        |> Shrink.keepIf (\( a, b ) -> edgePredicate a b)
        |> Shrink.list
        |> Shrink.convert toGraph fromGraph



-- Generating


graphGenerator :
    (Int -> Int -> Bool)
    -> Generator (Graph Int data edgeData)
graphGenerator fn =
    -- NOTE: the 0, 20 range is hard coded in multiple places in this file
    int 0 20
        |> andThen (\n -> list n (int 0 20))
        |> map List.Extra.unique
        |> andThen
            (\keys ->
                map
                    (\edges ->
                        List.foldl GP.insertEdge G.empty edges
                    )
                    (edgeGenerator fn keys)
            )


edgeGenerator : (Int -> Int -> Bool) -> List Int -> Generator (List ( Int, Int ))
edgeGenerator edgePredicate keys =
    list (List.length keys ^ 2) bool
        |> map
            (\lst ->
                let
                    d =
                        List.Extra.indexedFoldl (\idx v dict -> Dict.insert v idx dict) Dict.empty keys
                in
                product keys
                    |> List.Extra.zip lst
                    |> List.filter Tuple.first
                    |> List.map Tuple.second
                    |> List.filter
                        (\( a, b ) ->
                            edgePredicate
                                (Dict.get a d |> Maybe.withDefault 0)
                                (Dict.get b d |> Maybe.withDefault 0)
                        )
            )


product : List a -> List ( a, a )
product list =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) list)
        list
