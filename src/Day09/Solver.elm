module Day09.Solver exposing (..)

import Array exposing (Array)
import Day09.Input exposing (..)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


inputToUse =
    input


solve : String
solve =
    let
        histories =
            parseHistories

        answer =
            histories
                |> List.map getPrevVal
                |> List.sum
    in
    answer
        |> Debug.toString


getPrevVal : List Int -> Int
getPrevVal vals =
    case vals of
        [] ->
            -- should never get here!
            0

        head :: tail ->
            if tail |> List.all ((==) head) then
                -- all matching, so we're done
                -- (skipping 0 check for a bit of perf)
                head

            else
                -- they differ, so get next val for diffs and add to last val
                let
                    diffs : List Int
                    diffs =
                        vals
                            |> List.drop 1
                            |> List.Extra.zip vals
                            |> List.map (\( a, b ) -> b - a)

                    prevDiffVal =
                        getPrevVal diffs

                    firstVal =
                        vals
                            |> List.head
                            |> Maybe.withDefault -1
                in
                -- add next val to last val
                firstVal - prevDiffVal


getNextVal : List Int -> Int
getNextVal vals =
    case vals of
        [] ->
            -- should never get here!
            0

        head :: tail ->
            if tail |> List.all ((==) head) then
                -- all matching, so we're done
                -- (skipping 0 check for a bit of perf)
                head

            else
                -- they differ, so get next val for diffs and add to last val
                let
                    diffs : List Int
                    diffs =
                        vals
                            |> List.drop 1
                            |> List.Extra.zip vals
                            |> List.map (\( a, b ) -> b - a)

                    nextVal =
                        getNextVal diffs

                    lastVal =
                        vals
                            |> List.Extra.last
                            |> Maybe.withDefault -1
                in
                -- add next val to last val
                nextVal + lastVal


parseHistories : List (List Int)
parseHistories =
    inputToUse
        |> String.trim
        |> String.lines
        |> List.map
            (\line ->
                line
                    |> String.split " "
                    |> List.filterMap String.toInt
            )
