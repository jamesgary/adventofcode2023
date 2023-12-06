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
        ( seedRanges, maps ) =
            inputToSeedRangesAndMaps

        lowestLocation =
            maps
                |> List.reverse
                |> findLowestLocation seedRanges
    in
    lowestLocation
        |> String.fromInt


findLowestLocation : List ( Int, Int ) -> List (List Route) -> Int
findLowestLocation seedRanges maps =
    case maps of
        [] ->
            Debug.todo "empty list, should never get here!"

        finalMap ->
            -- try to find seed for lowest route
            Debug.todo "wutttttt"


inputToSeedRangesAndMaps : ( List ( Int, Int ), List (List Route) )
inputToSeedRangesAndMaps =
    case input |> String.trim |> String.split "\n\n" of
        [] ->
            ( [], [] )

        seedsStr :: rest ->
            let
                seedRanges : List ( Int, Int )
                seedRanges =
                    seedsStr
                        |> String.split ": "
                        |> List.drop 1
                        |> List.head
                        |> Maybe.withDefault ""
                        |> String.split " "
                        |> List.filterMap String.toInt
                        |> List.Extra.groupsOf 2
                        |> List.map
                            (\group ->
                                case group of
                                    [ a, b ] ->
                                        ( a, b )

                                    _ ->
                                        Debug.todo "odd nums"
                            )

                maps : List (List Route)
                maps =
                    rest
                        |> List.map
                            (\mapStr ->
                                let
                                    routes =
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
                                            |> List.sortBy .sourceRangeStart

                                    zeroRoute : List Route
                                    zeroRoute =
                                        case List.head routes of
                                            Just head ->
                                                [ { sourceRangeStart = 0
                                                  , destRangeStart = 0
                                                  , rangeLength = head.sourceRangeStart
                                                  }
                                                ]

                                            Nothing ->
                                                []
                                in
                                zeroRoute ++ routes
                            )
            in
            ( seedRanges, maps )



{-
   seedToLocation_ : List (List Route) -> Int -> Int
   seedToLocation_ maps seed =
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


   inputToSeedsAndMaps_ : ( List Int, List (List Route) )
   inputToSeedsAndMaps_ =
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
-}
