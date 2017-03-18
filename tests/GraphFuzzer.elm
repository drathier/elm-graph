module Tests exposing (..)

import List.Extra
import Maybe.Extra
import Random.Pcg exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import String
import Graph as G exposing (Graph, empty)
import Set
import Shrink exposing (Shrinker)


-- Validating


checkComesBefore : a -> a -> List a -> Bool
checkComesBefore first second list =
  case list of
    [] ->
      False

    head :: tail ->
      if head == first then
        List.member second list
      else
        checkComesBefore first second tail


checkConstraints : List ( a, a ) -> List a -> Bool
checkConstraints constraintList ordering =
  constraintList
    |> List.all (\( a, b ) -> checkComesBefore a b ordering)


-- Fuzzing


graphFuzzer : Fuzzer (Graph Int data)
graphFuzzer =
  Fuzz.custom graphGenerator graphShrinker


-- Shrinking


graphShrinker : Shrinker (Graph Int data)
graphShrinker =
  let
    toGraph =
      List.foldl G.insertEdge empty

    fromGraph =
      G.edges
  in
    -- NOTE: only handles edges; nothing else
    Shrink.convert toGraph fromGraph <|
      Shrink.list (Shrink.tuple ( Shrink.int, Shrink.int ))


-- Generating


graphGenerator : Generator (Graph Int data)
graphGenerator =
  int 0 15
    |> andThen (\n -> constant (List.range 0 n))
    |> andThen
        (\keys ->
          map
            (\edges ->
              List.foldl G.insertEdge empty edges
            )
            (edgeGenerator keys)
        )


edgeGenerator : List comparable -> Generator (List ( comparable, comparable ))
edgeGenerator keys =
  list (List.length keys ^ 2) bool
    |> map
        (\lst ->
          (product keys)
            |> List.Extra.zip lst
            |> List.filter Tuple.first
            |> List.map Tuple.second
        )


product : List a -> List ( a, a )
product list =
  List.concatMap
    (\x -> List.map (\y -> ( x, y )) list)
    list
