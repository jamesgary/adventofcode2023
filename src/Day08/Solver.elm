module Day08.Solver exposing (..)

import Day08.Input exposing (..)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)



-- vegeta voice: it's over 2,440,000,000!!!


inputToUse =
    input_


solve : String
solve =
    let
        ( inst, nodes ) =
            parsedInput

        startingNodes =
            nodes
                |> Dict.keys
                |> List.filter (String.endsWith "A")
                |> Debug.log "HI"

        stepsCount =
            step inst nodes startingNodes 0
    in
    stepsCount
        |> Debug.toString


isEndingNode : String -> Bool
isEndingNode node =
    node |> String.endsWith "Z"


step : List Char -> Dict String ( String, String ) -> List String -> Int -> Int
step inst nodes curNodes stepsCount =
    let
        done =
            curNodes
                |> List.all isEndingNode
    in
    --if stepsCount > 10000000 then
    if stepsCount > 1000000000000 then
        -5

    else if done then
        stepsCount

    else
        let
            _ =
                if (stepsCount |> modBy 1000000) == 0 then
                    Debug.log "" stepsCount

                else
                    stepsCount

            numInst =
                inst
                    |> List.length

            instCharIndex =
                stepsCount |> modBy numInst

            instChar =
                inst
                    |> List.Extra.getAt instCharIndex
                    |> Maybe.withDefault '?'

            nextNodes =
                curNodes
                    |> List.map
                        (\node ->
                            case Dict.get node nodes of
                                Just ( l, r ) ->
                                    case instChar of
                                        'L' ->
                                            l

                                        'R' ->
                                            r

                                        _ ->
                                            Debug.todo "bad inst char"

                                -- |> Debug.log (( instChar, node ) |> Debug.toString)
                                Nothing ->
                                    Debug.todo "can't find node"
                        )
        in
        step inst nodes nextNodes (stepsCount + 1)


parsedInput : ( List Char, Dict String ( String, String ) )
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
            )

        _ ->
            Debug.todo "bad input"
