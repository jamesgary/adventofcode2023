module Day05.Solver exposing (..)

import Day05.Input exposing (input)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


type alias Route =
    { sourceRangeStart : Int
    , destRangeStart : Int
    , rangeLength : Int
    }


solve : String
solve =
    let
        ( seeds, maps ) =
            inputToSeedsAndMaps

        lowestLocation =
            seeds
                |> List.map (seedToLocation maps)
                |> List.sort
                |> List.head
                |> Maybe.withDefault -1
    in
    lowestLocation
        |> String.fromInt


seedToLocation : List (List Route) -> Int -> Int
seedToLocation maps seed =
    maps
        |> List.foldl
            (\routes source ->
                let
                    route =
                        routes
                            -- already in descending order by sourceRangeStart
                            |> List.Extra.find
                                (\r ->
                                    r.sourceRangeStart <= source && source <= r.sourceRangeStart + r.rangeLength
                                )

                    dest =
                        case route of
                            Just r ->
                                (source - r.sourceRangeStart)
                                    + r.destRangeStart

                            Nothing ->
                                source
                in
                dest
            )
            seed


inputToSeedsAndMaps : ( List Int, List (List Route) )
inputToSeedsAndMaps =
    case input |> String.trim |> String.split "\n\n" of
        [] ->
            ( [], [] )

        seedsStr :: rest ->
            let
                seeds : List Int
                seeds =
                    seedsStr
                        |> String.split ": "
                        |> List.drop 1
                        |> List.head
                        |> Maybe.withDefault ""
                        |> String.split " "
                        |> List.filterMap String.toInt

                -- challenge b
                -- |> List.Extra.groupsOf 2
                -- |> List.map
                --     (\group ->
                --         case group of
                --             [ a, b ] ->
                --                 List.range 0 (b - 1)
                --                     |> List.map ((+) a)
                --             _ ->
                --                 Debug.todo "odd nums"
                --     )
                -- |> List.concat
                maps : List (List Route)
                maps =
                    rest
                        |> List.map
                            (\mapStr ->
                                mapStr
                                    |> String.lines
                                    |> List.drop 1
                                    |> List.map
                                        (\routeStr ->
                                            case routeStr |> String.split " " |> List.filterMap String.toInt of
                                                [ d, s, r ] ->
                                                    Route s d r

                                                _ ->
                                                    Debug.todo ("bad routeStr: " ++ routeStr)
                                        )
                                    -- sort them all by source range start descending
                                    |> List.sortBy .sourceRangeStart
                                    |> List.reverse
                            )
            in
            ( seeds, maps )
