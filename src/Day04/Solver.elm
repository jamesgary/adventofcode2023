module Day04.Solver exposing (..)

import Day04.Input exposing (input)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


solve : String
solve =
    input
        |> String.trim
        |> String.lines
        |> List.map
            (\line ->
                case String.split ":" line of
                    [ cardIdStr_, cardNumsStr ] ->
                        case String.split "|" cardNumsStr of
                            [ winningNumStrs, myNumStrs ] ->
                                matchingNums (winningNumStrs |> String.trim) (myNumStrs |> String.trim)
                                    |> List.length
                                    |> (\matchCount ->
                                            if matchCount <= 0 then
                                                0

                                            else
                                                2 ^ (matchCount - 1)
                                       )

                            _ ->
                                Debug.todo ("bad nums: " ++ cardNumsStr)

                    _ ->
                        Debug.todo ("bad line: " ++ line)
            )
        |> List.sum
        |> String.fromInt


matchingNums : String -> String -> List Int
matchingNums winningNumsStr myNumsStr =
    let
        winningNums =
            winningNumsStr
                |> String.split " "
                |> List.filterMap String.toInt
                |> Set.fromList

        myNums =
            myNumsStr
                |> String.split " "
                |> List.filterMap String.toInt
                |> Set.fromList
    in
    Set.intersect winningNums myNums
        |> Set.toList
