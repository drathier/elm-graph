module GraphFuzzer exposing (..)

import Dict exposing (Dict)
import List.Extra
import Random.Pcg exposing (..)
import Fuzz exposing (Fuzzer)
import Graph as G exposing (Graph, empty)
import Set
import Shrink exposing (Shrinker)


-- Fuzzing


acyclicGraphFuzzer : Fuzzer (Graph Int data)
acyclicGraphFuzzer =
  Fuzz.custom (graphGenerator (edgeGenerator (<))) (graphShrinker (<))


acyclicGraphFuzzerWithSelfEdges : Fuzzer (Graph Int data)
acyclicGraphFuzzerWithSelfEdges =
  Fuzz.custom (graphGenerator (edgeGenerator (<=))) (graphShrinker (<=))


graphFuzzer : Fuzzer (Graph Int data)
graphFuzzer =
  Fuzz.custom (graphGenerator (edgeGenerator (\_ _ -> True))) (graphShrinker (\_ _ -> True))


-- Shrinking


graphShrinker : (Int -> Int -> Bool) -> Shrinker (Graph Int data)
graphShrinker edgePredicate =
  let
    toGraph =
      List.foldl G.insertEdge empty

    fromGraph =
      G.edges
  in
    -- NOTE: only handles edges; nothing else
    Shrink.convert toGraph fromGraph <|
      Shrink.list (Shrink.keepIf (uncurry edgePredicate) <| Shrink.tuple ( Shrink.int, Shrink.int ))


-- Generating


graphGenerator :
  (List Int -> Generator (List ( Int, Int )))
  -> Generator (Graph Int data)
graphGenerator edgeGenerator =
  int 0 20
    |> andThen (\n -> list n (int minInt maxInt))
    |> map List.Extra.unique
    |> andThen
        (\keys ->
          map
            (\edges ->
              List.foldl G.insertEdge empty edges
            )
            (edgeGenerator keys)
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
            (product keys)
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