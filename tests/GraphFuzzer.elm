module GraphFuzzer exposing (..)

import List.Extra
import Random.Pcg exposing (..)
import Fuzz exposing (Fuzzer)
import Graph as G exposing (Graph, empty)
import Shrink exposing (Shrinker)


-- Fuzzing


dagFuzzerWithoutLoops : Fuzzer (Graph Int data)
dagFuzzerWithoutLoops =
  Fuzz.custom (graphGenerator (edgeGenerator (<))) (graphShrinker (<))


dagFuzzerWithLoops : Fuzzer (Graph Int data)
dagFuzzerWithLoops =
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
    |> andThen (\n -> constant (List.range 0 n))
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
          (product keys)
            |> List.Extra.zip lst
            |> List.filter Tuple.first
            |> List.map Tuple.second
            |> List.filter (uncurry edgePredicate)
        )


product : List a -> List ( a, a )
product list =
  List.concatMap
    (\x -> List.map (\y -> ( x, y )) list)
    list
