module Day01.Solver exposing (..)

import Day01.Input exposing (input)


solve : String
solve =
    -- test: sum all lines
    input
        |> String.trim
        |> String.lines
        |> List.map
            (\line ->
                let
                    chars =
                        line
                            |> String.toList
                            |> List.map String.fromChar

                    firstNum =
                        chars
                            |> List.filterMap String.toInt
                            |> List.map String.fromInt
                            |> List.head
                            |> Maybe.withDefault ""

                    lastNum =
                        chars
                            |> List.filterMap String.toInt
                            |> List.map String.fromInt
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault ""

                    -- _ =
                    --     Debug.log "" ( firstNum, lastNum )
                in
                (firstNum ++ lastNum)
                    |> String.toInt
                    |> Maybe.withDefault 0
            )
        |> List.sum
        |> String.fromInt
