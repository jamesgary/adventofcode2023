module Day01.Solver exposing (..)

import Day01.Input exposing (input)


solve : String
solve =
    -- test: sum all lines
    input
        |> String.trim
        |> String.lines
        |> List.filterMap String.toInt
        |> List.sum
        |> String.fromInt
