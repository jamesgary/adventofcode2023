module Day11.Solver exposing (..)

import Array exposing (Array)
import Day11.Input exposing (..)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


inputToUse =
    input


expansion =
    1000000


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
                            -- 'X' represents 1,000,000
                            List.repeat (List.length charList) 'X'

                        else
                            charList
                    )
                -- expand universe cols (do the same thing, just transpose first)
                |> List.Extra.transpose
                |> List.map
                    (\charList ->
                        if List.all (\c -> c == '.' || c == 'X') charList then
                            -- 'X' represents 1,000,000
                            List.repeat (List.length charList) 'X'

                        else
                            charList
                    )
                -- un-transpose
                |> List.Extra.transpose

        unexpandedStarCoords : List ( Int, Int )
        unexpandedStarCoords =
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

        unexpandedCoordToChar : Dict ( Int, Int ) Char
        unexpandedCoordToChar =
            charLists
                |> List.indexedMap
                    (\y charList ->
                        charList
                            |> List.indexedMap
                                (\x char ->
                                    ( ( x, y ), char )
                                )
                    )
                |> List.concat
                |> Dict.fromList

        expandedStarCoords : List ( Int, Int )
        expandedStarCoords =
            unexpandedStarCoords
                |> List.map
                    (\( unexpandedX, unexpandedY ) ->
                        let
                            expandedX =
                                List.range 0 unexpandedX
                                    |> List.map
                                        (\x ->
                                            case unexpandedCoordToChar |> Dict.get ( x, unexpandedY ) of
                                                Just 'X' ->
                                                    expansion

                                                _ ->
                                                    1
                                        )
                                    |> List.sum

                            expandedY =
                                List.range 0 unexpandedY
                                    |> List.map
                                        (\y ->
                                            case unexpandedCoordToChar |> Dict.get ( unexpandedX, y ) of
                                                Just 'X' ->
                                                    expansion

                                                _ ->
                                                    1
                                        )
                                    |> List.sum
                        in
                        ( expandedX, expandedY )
                    )

        distSum : Int
        distSum =
            expandedStarCoords
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
