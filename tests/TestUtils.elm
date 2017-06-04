module TestUtils exposing (..)

import Expect
import Graph.Internal exposing (..)
import Test exposing (..)


emptyDag =
  case empty |> enableDagReachability of
    Just g ->
      g

    Nothing ->
      Debug.crash "how did we fail at enabling DAG reachability on an empty graph?"


many : List Expect.Expectation -> Expect.Expectation
many expectations =
  case expectations of
    [] ->
      Expect.pass

    expectation :: expectations ->
      if expectation == Expect.pass then
        many expectations
      else
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
