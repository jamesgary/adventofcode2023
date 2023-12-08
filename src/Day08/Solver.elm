module Day08.Solver exposing (..)

import Day08.Input exposing (..)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


inputToUse =
    input_


solve : String
solve =
    let
        ( inst, nodes, ( firstNode_, goalNode_ ) ) =
            parsedInput

        -- D'oh, not actually first and last :P, can hardcode
        firstNode =
            "AAA"

        goalNode =
            "ZZZ"

        stepsCount =
            step inst nodes goalNode firstNode 1
    in
    stepsCount
        |> Debug.toString


step : List Char -> Dict String ( String, String ) -> String -> String -> Int -> Int
step inst nodes goalNode curNode stepsCount =
    let
        numInst =
            inst
                |> List.length

        instCharIndex =
            (stepsCount - 1) |> modBy numInst

        instChar =
            inst
                |> List.Extra.getAt instCharIndex
                |> Maybe.withDefault '?'

        nextNode =
            case Dict.get curNode nodes of
                Just ( l, r ) ->
                    let
                        _ =
                            Debug.log "?"

                        --( curNode, instChar )
                    in
                    case instChar of
                        'L' ->
                            l

                        'R' ->
                            r

                        _ ->
                            Debug.todo "bad inst char"

                Nothing ->
                    Debug.todo "can't find node"
    in
    if nextNode == goalNode then
        stepsCount

    else if stepsCount > 100000 then
        -1

    else
        step inst nodes goalNode nextNode (stepsCount + 1)


parsedInput : ( List Char, Dict String ( String, String ), ( String, String ) )
parsedInput =
    case inputToUse |> String.trim |> String.split "\n\n" of
        [ instStr, nodesStr ] ->
            let
                nodesList =
                    nodesStr
                        |> String.lines
                        |> List.map
                            (\line ->
                                case line |> String.split " = " of
                                    [ key, vals ] ->
                                        ( key
                                        , case vals |> String.dropLeft 1 |> String.dropRight 1 |> String.split ", " of
                                            [ l, r ] ->
                                                ( l, r )

                                            _ ->
                                                Debug.todo "bad line"
                                        )

                                    _ ->
                                        Debug.todo "bad line"
                            )
            in
            ( instStr
                |> String.trim
                |> String.toList
            , nodesList
                |> Dict.fromList
            , ( nodesList
                    |> List.head
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault "???"
              , nodesList
                    |> List.reverse
                    |> List.head
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault "???"
              )
            )

        _ ->
            Debug.todo "bad input"
