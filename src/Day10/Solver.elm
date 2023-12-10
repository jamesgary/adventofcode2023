module Day10.Solver exposing (..)

import Array exposing (Array)
import Day10.Input exposing (..)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


inputToUse =
    input


solve : String
solve =
    let
        startPos : Pos
        startPos =
            parsedTiles
                |> Dict.toList
                |> List.Extra.find (\( pos_, pipe ) -> pipe == Start)
                |> Maybe.map Tuple.first
                |> Maybe.withDefault badPos
    in
    parsedTiles
        |> getStepsToFurthestPos startPos
        |> Debug.toString


type alias Pos =
    ( Int, Int )


type Pipe
    = Vert
    | Horz
    | NE
    | NW
    | SW
    | SE
    | Start


allRelNeighborPoses : List Pos
allRelNeighborPoses =
    [ ( -1, 0 )
    , ( 0, -1 )
    , ( 0, 1 )
    , ( 1, 0 )
    ]


add : Pos -> Pos -> Pos
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2
    , y1 + y2
    )


connectedNeighborsForPos : Pos -> Dict Pos Pipe -> ( Pos, Pos )
connectedNeighborsForPos pos pipes =
    case Dict.get pos pipes of
        Just pipe ->
            let
                validRelNeighborPoses : Maybe ( Pos, Pos )
                validRelNeighborPoses =
                    case pipe of
                        Vert ->
                            Just
                                ( ( 0, 1 )
                                , ( 0, -1 )
                                )

                        Horz ->
                            Just
                                ( ( 1, 0 )
                                , ( -1, 0 )
                                )

                        NE ->
                            Just
                                ( ( 0, 1 )
                                , ( 1, 0 )
                                )

                        NW ->
                            Just
                                ( ( 0, 1 )
                                , ( -1, 0 )
                                )

                        SW ->
                            Just
                                ( ( 0, -1 )
                                , ( -1, 0 )
                                )

                        SE ->
                            Just
                                ( ( 0, -1 )
                                , ( 1, 0 )
                                )

                        Start ->
                            Nothing
                                |> Debug.log "can't get here"
            in
            validRelNeighborPoses
                |> Maybe.withDefault ( badPos, badPos )
                |> Tuple.mapBoth (add pos) (add pos)

        Nothing ->
            let
                _ =
                    Debug.log "no pipe for this pos!" pos
            in
            ( badPos, badPos )


validPipesForRelNeighborPos : Dict Pos (List Pipe)
validPipesForRelNeighborPos =
    [ ( ( 0, 1 ), [ Vert, SW, SE ] ) -- my north
    , ( ( 0, -1 ), [ Vert, NW, NE ] ) -- my south
    , ( ( 1, 0 ), [ Horz, NW, SW ] ) -- my east
    , ( ( -1, 0 ), [ Horz, NE, SE ] ) -- my west
    ]
        |> Dict.fromList


getStepsToFurthestPos : Pos -> Dict Pos Pipe -> Int
getStepsToFurthestPos startPos pipes =
    let
        nextPos =
            allRelNeighborPoses
                |> List.Extra.findMap
                    (\relNeighborPos ->
                        let
                            neighborPos =
                                add startPos relNeighborPos

                            neighborPipe =
                                pipes
                                    |> Dict.get neighborPos
                                    |> Maybe.withDefault Start

                            isValid =
                                validPipesForRelNeighborPos
                                    |> Dict.get relNeighborPos
                                    |> Maybe.map (List.member neighborPipe)
                                    |> Maybe.withDefault False
                        in
                        if isValid then
                            Just neighborPos

                        else
                            Nothing
                    )
                |> Maybe.withDefault badPos

        --|> Debug.log "nextPosForStart"
        loop =
            buildLoop pipes ( startPos, nextPos ) [ nextPos, startPos ]

        --|> Debug.log "loop"
    in
    List.length loop // 2


buildLoop : Dict Pos Pipe -> ( Pos, Pos ) -> List Pos -> List Pos
buildLoop pipes ( prevPos, curPos ) visitedPipes =
    let
        ( pos1, pos2 ) =
            connectedNeighborsForPos curPos pipes

        --|> Debug.log ("cnfsp " ++ Debug.toString curPos)
        nextPos =
            if pos1 == prevPos then
                pos2

            else
                pos1
    in
    if List.member nextPos visitedPipes || List.length visitedPipes > 100000 then
        visitedPipes

    else
        buildLoop pipes ( curPos, nextPos ) (nextPos :: visitedPipes)


parsedTiles : Dict Pos Pipe
parsedTiles =
    inputToUse
        |> String.trim
        |> String.lines
        |> List.indexedMap
            (\y line ->
                line
                    |> String.toList
                    |> List.indexedMap
                        (\x char ->
                            case charToPipe char of
                                Just pipe ->
                                    Just ( ( x, -y ), pipe )

                                Nothing ->
                                    Nothing
                        )
                    |> List.filterMap identity
            )
        |> List.concat
        |> Dict.fromList


charToPipe : Char -> Maybe Pipe
charToPipe char =
    case char of
        '|' ->
            Just Vert

        '-' ->
            Just Horz

        'L' ->
            Just NE

        'J' ->
            Just NW

        '7' ->
            Just SW

        'F' ->
            Just SE

        'S' ->
            Just Start

        _ ->
            Nothing


badPos =
    ( -1, -1 )
