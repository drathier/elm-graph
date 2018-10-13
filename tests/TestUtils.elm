module TestUtils exposing (allDifferent, checkComesBefore, checkPartialOrdering, many, todo)

import Expect
import Graph exposing (..)
import Test exposing (..)


many : List Expect.Expectation -> Expect.Expectation
many expectations =
    case expectations of
        [] ->
            Expect.pass

        exp :: exps ->
            if exp == Expect.pass then
                many exps

            else
                exp


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
                Expect.fail "expected to find needle"

            head :: tail ->
                if head == first then
                    List.member second tail |> Expect.true "expected first argument to come before second argument in the given list"

                else
                    checkComesBefore first second tail


checkPartialOrdering : List ( a, a ) -> List a -> Expect.Expectation
checkPartialOrdering constraintList ordering =
    constraintList
        |> List.map (\( a, b ) -> checkComesBefore a b ordering)
        |> many
