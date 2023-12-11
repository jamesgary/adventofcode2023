module Day11.Solver exposing (..)

import Array exposing (Array)
import Day11.Input exposing (..)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


inputToUse =
    input


solve : String
solve =
    let
        charLists : List (List Char)
        charLists =
            inputToUse
                |> String.trim
                |> String.lines
                |> List.map String.toList
                -- expand universe rows
                |> List.map
                    (\charList ->
                        if List.all ((==) '.') charList then
                            [ charList, charList ]

                        else
                            [ charList ]
                    )
                |> List.concat
                -- expand universe cols (do the same thing, just transpose first)
                |> List.Extra.transpose
                |> List.map
                    (\charList ->
                        if List.all ((==) '.') charList then
                            [ charList, charList ]

                        else
                            [ charList ]
                    )
                |> List.concat
                -- un-transpose
                |> List.Extra.transpose

        starCoords : List ( Int, Int )
        starCoords =
            charLists
                |> List.indexedMap
                    (\y charList ->
                        charList
                            |> List.indexedMap
                                (\x char ->
                                    if char == '#' then
                                        Just ( x, y )

                                    else
                                        Nothing
                                )
                    )
                |> List.concat
                |> List.filterMap identity

        distSum : Int
        distSum =
            starCoords
                |> List.Extra.uniquePairs
                |> List.map
                    (\( ( x1, y1 ), ( x2, y2 ) ) ->
                        abs (x1 - x2) + abs (y1 - y2)
                    )
                |> List.sum
    in
    distSum
        -- convert to set of galaxy coordinates
        |> Debug.toString
