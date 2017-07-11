module GraphFuzzer exposing (..)

import Dict exposing (Dict)
import List.Extra
import Random.Pcg exposing (..)
import Fuzz exposing (Fuzzer)
import Graph.Internal as G exposing (Graph, empty, setTag, valid)
import Set
import Shrink exposing (Shrinker)


emptyDag =
  case G.empty |> G.enableDagReachability of
    Just g ->
      g

    Nothing ->
      Debug.crash "how did we fail at enabling DAG reachability on an empty graph?"


-- Fuzzing


acyclicGraphFuzzer : Fuzzer (Graph Int data)
acyclicGraphFuzzer =
  Fuzz.custom (graphGenerator (setTag 47114 emptyDag) (edgeGenerator (<))) (graphShrinker (setTag 47124 emptyDag) (<))


acyclicGraphFuzzerWithSelfEdges : Fuzzer (Graph Int data)
acyclicGraphFuzzerWithSelfEdges =
  Fuzz.custom (graphGenerator (setTag 47112 empty) (edgeGenerator (<=))) (graphShrinker (setTag 47122 empty) (<=))


graphFuzzer : Fuzzer (Graph Int data)
graphFuzzer =
  Fuzz.custom (graphGenerator (setTag 47113 empty) (edgeGenerator (\_ _ -> True))) (graphShrinker (setTag 47123 empty) (\_ _ -> True))


-- Shrinking


graphShrinker : Graph Int data -> (Int -> Int -> Bool) -> Shrinker (Graph Int data)
graphShrinker empty edgePredicate =
  let
    toGraph =
      List.foldl G.insertEdge empty

    fromGraph =
      G.edges

    intRange low high =
      (Shrink.keepIf (\i -> low <= i && i <= high) Shrink.int)
  in
    -- NOTE: only handles edges; nothing else
    Shrink.tuple ( intRange 0 100, intRange 0 100 )
      |> Shrink.keepIf (uncurry edgePredicate)
      |> Shrink.list
      |> Shrink.convert toGraph fromGraph


-- Generating


graphGenerator :
  Graph Int data
  -> (List Int -> Generator (List ( Int, Int )))
  -> Generator (Graph Int data)
graphGenerator empty edgeGenerator =
  -- NOTE: the 0, 100 range is hard coded in multiple places in this file
  int 0 20
    |> andThen (\n -> list n (int 0 100))
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
