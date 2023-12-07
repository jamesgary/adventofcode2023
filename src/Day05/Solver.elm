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
                |> findLowestLocation 0 seedRanges
    in
    lowestLocation
        |> String.fromInt



{- Starting from location 0, search every location to see if there's a matching seed.
   But that'll take too long! So we must skip. How can we smartly skip?
   As we traverse from location to seed, see how much remaining "range" we have for every layer.
   Take the minimum range for each traversal and skip that many seeds.
-}


findLowestLocation : Int -> List ( Int, Int ) -> List (List Route) -> Int
findLowestLocation loc seedRanges maps =
    case traverseUpToSource 9999999999999 loc seedRanges maps of
        Nothing ->
            -- success!
            loc

        Just minRange ->
            let
                _ =
                    Debug.log "MR" minRange
            in
            findLowestLocation (loc + minRange) seedRanges maps


traverseUpToSource : Int -> Int -> List ( Int, Int ) -> List (List Route) -> Maybe Int
traverseUpToSource minRange dest seedRanges maps =
    case maps of
        [] ->
            -- finished all maps, now is there a seed here?
            let
                found =
                    seedRanges
                        |> List.any
                            (\( seedStart, seedRange ) ->
                                seedStart <= dest && dest < seedStart + seedRange
                            )
            in
            if found then
                Nothing

            else
                Just minRange

        -- r.sourceRangeStart <= source && source <= r.sourceRangeStart + r.rangeLength
        map :: rest ->
            -- still searching. get source for this dest
            let
                ( source, range ) =
                    getSourceAndRangeFromDestForMap map dest
            in
            traverseUpToSource (min range minRange) source seedRanges rest


getSourceAndRangeFromDestForMap : List Route -> Int -> ( Int, Int )
getSourceAndRangeFromDestForMap map dest =
    map
        |> List.Extra.findMap
            (\r ->
                if r.destRangeStart <= dest && dest < r.destRangeStart + r.rangeLength then
                    let
                        range =
                            (r.rangeLength - (dest - r.destRangeStart))
                                |> max 1
                                |> Debug.log "mrrrrrrr"
                    in
                    ( r.sourceRangeStart
                        + (dest - r.destRangeStart)
                    , range
                    )
                        |> Just

                else
                    Nothing
            )
        -- dest must be same as source
        |> Maybe.withDefault ( dest, 9999999999999999 )


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
