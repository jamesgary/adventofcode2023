module Day08.Solver exposing (..)

import Array exposing (Array)
import Day08.Input exposing (..)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)



-- vegeta voice: it's over 2,440,000,000!!!
-- inst == instruction


inputToUse =
    inputB


solve : String
solve =
    let
        ( inst, nodes ) =
            parsedInput

        startingNodes =
            nodes
                |> Dict.keys
                |> List.filter (String.endsWith "A")

        routes =
            startingNodes
                |> List.map (getRoute inst nodes)
                |> Debug.log "?"

        smartRoutes : List SmartRoute
        smartRoutes =
            routes
                |> List.map
                    (\{ loopedNodes } ->
                        { loopSize = List.length loopedNodes
                        , foundIndices =
                            loopedNodes
                                |> List.indexedMap Tuple.pair
                                |> List.filterMap
                                    (\( i, node ) ->
                                        if isEndingNode node then
                                            Just i

                                        else
                                            Nothing
                                    )
                                |> Set.fromList
                        }
                    )

        -- foo =
        --     [ 18113, 13201, 20569, 16271, 22411, 18727 ]
        --         |> List.product
        -- CACHED
        -- [ { firstLoopStep = 16578, loopedNodes = [ "CLM", "GQX" ] }
        -- , { firstLoopStep = 2149, loopedNodes = [ "TDM", "VXB" ] }
        -- , { firstLoopStep = 3684, loopedNodes = [ "RXC", "MLC", "CQB", "FHG" ] }
        -- , { firstLoopStep = 8903, loopedNodes = [ "LDV", "TXQ", "KPQ" ] }
        -- , { firstLoopStep = 10438, loopedNodes = [ "DPB", "RGM", "XPQ" ] }
        -- , { firstLoopStep = 9210, loopedNodes = [ "TCB", "RSJ", "RHT", "MJS" ] }
        -- ]
    in
    smartRoutes
        |> Debug.toString


type alias Route =
    { firstLoopStep : Int
    , loopedNodes : List String
    }


type alias SmartRoute =
    { loopSize : Int
    , foundIndices : Set Int
    }


getRoute : List Char -> Dict String ( String, String ) -> String -> Route
getRoute inst nodes node =
    let
        ( firstLoopStep, loopedNodes ) =
            step inst nodes node Dict.empty
                |> Debug.log "gr"

        len =
            List.length loopedNodes

        newLoopedNodes =
            (loopedNodes
                |> List.drop (len - firstLoopStep)
            )
                ++ (loopedNodes
                        |> List.take len
                   )
    in
    { firstLoopStep = 0
    , loopedNodes = newLoopedNodes
    }


isEndingNode : String -> Bool
isEndingNode node =
    node |> String.endsWith "Z"


step : List Char -> Dict String ( String, String ) -> String -> Dict ( Int, String ) Int -> ( Int, List String )
step inst nodes curNode nodeVisitedAtInstIndexToStepNum =
    let
        stepNum =
            nodeVisitedAtInstIndexToStepNum
                |> Dict.size

        instCount =
            inst
                |> List.length

        instCharIndex =
            stepNum |> modBy instCount

        maybeFoundIndex =
            -- at the same node on the same modded index
            nodeVisitedAtInstIndexToStepNum
                |> Dict.get ( instCharIndex, curNode )
    in
    if stepNum > 100000 then
        ( -1, [] )

    else
        case maybeFoundIndex of
            Just i ->
                let
                    _ =
                        Debug.log "!" ( i, instCharIndex, curNode )
                in
                ( i
                , nodeVisitedAtInstIndexToStepNum
                    |> Dict.toList
                    -- sort by stepNum
                    |> List.sortBy Tuple.second
                    |> Debug.log "???"
                    |> List.map Tuple.first
                    |> List.drop i
                    -- drop the found one
                    |> List.reverse
                    |> List.drop 1
                    |> List.reverse
                    |> List.map Tuple.second
                    |> Debug.log "???!!"
                )

            Nothing ->
                let
                    instChar =
                        inst
                            |> List.Extra.getAt instCharIndex
                            |> Maybe.withDefault '?'

                    nextNode =
                        case Dict.get curNode nodes of
                            Just ( l, r ) ->
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
                step inst
                    nodes
                    nextNode
                    (nodeVisitedAtInstIndexToStepNum
                        |> Dict.insert
                            ( instCharIndex
                            , curNode
                            )
                            stepNum
                    )


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
