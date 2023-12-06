module Day06.Solver exposing (..)

import Day06.Input exposing (input)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


solve : String
solve =
    case input |> String.trim |> String.lines of
        [ timeStr, distStr ] ->
            let
                times =
                    timeStr
                        |> String.split ":"
                        |> List.drop 1
                        |> String.concat
                        |> String.split " "
                        |> List.filterMap String.toInt

                dists =
                    distStr
                        |> String.split ":"
                        |> List.drop 1
                        |> String.concat
                        |> String.split " "
                        |> List.filterMap String.toInt

                races : List ( Int, Int )
                races =
                    List.map2 Tuple.pair times dists

                raceWays : List Int
                raceWays =
                    races
                        |> List.map raceToWay
            in
            raceWays
                |> List.product
                |> Debug.toString

        _ ->
            Debug.todo "hmm"


raceToWay : ( Int, Int ) -> Int
raceToWay ( time, dist ) =
    let
        minHold =
            List.range 1 time
                |> List.Extra.find
                    (\t ->
                        t * (time - t) > dist
                    )
                |> Maybe.withDefault -1

        maxHold =
            time - minHold

        _ =
            Debug.log "?" ( ( time, dist ), ( minHold, maxHold ), 1 + maxHold - minHold )
    in
    1 + maxHold - minHold
