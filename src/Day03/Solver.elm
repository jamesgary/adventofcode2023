module Day03.Solver exposing (..)

import Day03.Input exposing (input)
import Dict exposing (Dict)
import List.Extra


type alias Num =
    { startPos : ( Int, Int )
    , numStr : String
    }


type alias Schematic =
    Dict ( Int, Int ) Char


solve : String
solve =
    let
        schematic =
            input
                |> strToSchematic

        nums =
            schematic
                |> schematicToNums ( 0, 0 )
    in
    schematic
        |> Dict.filter (\pos val -> val == '*')
        |> Dict.keys
        |> List.map (gearPosToPower schematic nums)
        |> List.sum
        |> String.fromInt


solve_ : String
solve_ =
    let
        schematic =
            input
                |> strToSchematic

        nums =
            schematic
                |> schematicToNums ( 0, 0 )
    in
    nums
        |> List.filter (isNumTouchingSymbol schematic Nothing)
        |> List.map numToInt
        |> List.sum
        |> String.fromInt


strToSchematic : String -> Schematic
strToSchematic str =
    str
        |> String.trim
        |> String.lines
        |> List.indexedMap
            (\y line ->
                line
                    |> String.toList
                    |> List.indexedMap
                        (\x char ->
                            ( ( x, y ), char )
                        )
            )
        |> List.concat
        -- filter out dots
        -- |> List.filter (\( pos, char ) -> char == '.')
        |> Dict.fromList


schematicToNums : ( Int, Int ) -> Schematic -> List Num
schematicToNums ( x, y ) schematic =
    case Dict.get ( x, y ) schematic of
        Just _ ->
            -- OK, we're still within schematic
            case getFullNumStr ( x, y ) schematic of
                "" ->
                    -- not a num, move ahead
                    schematicToNums ( x + 1, y ) schematic

                numStr ->
                    -- nice, got a num, move ahead
                    { startPos = ( x, y )
                    , numStr = numStr
                    }
                        :: schematicToNums ( x + (String.length numStr + 1), y ) schematic

        Nothing ->
            -- end of row, try next row down
            case Dict.get ( 0, y + 1 ) schematic of
                Just _ ->
                    -- got something, keep going
                    schematicToNums ( 0, y + 1 ) schematic

                Nothing ->
                    -- done!
                    []


getFullNumStr : ( Int, Int ) -> Schematic -> String
getFullNumStr ( x, y ) schematic =
    case Dict.get ( x, y ) schematic of
        Just char ->
            if Char.isDigit char then
                String.cons char <| getFullNumStr ( x + 1, y ) schematic

            else
                ""

        Nothing ->
            ""


isNumTouchingSymbol : Schematic -> Maybe ( Int, Int ) -> Num -> Bool
isNumTouchingSymbol schematic maybeSpecificPos num =
    let
        numLength =
            num.numStr
                |> String.length

        ( x, y ) =
            num.startPos

        neighbors =
            List.range (y - 1) (y + 1)
                |> List.map
                    (\y_ ->
                        List.range (x - 1) (x + numLength)
                            |> List.map
                                (\x_ ->
                                    ( x_, y_ )
                                )
                    )
                |> List.concat
    in
    neighbors
        |> List.any
            (\neighbor ->
                schematic
                    |> Dict.get neighbor
                    |> Maybe.map
                        (\char ->
                            case maybeSpecificPos of
                                Just pos ->
                                    pos == neighbor

                                Nothing ->
                                    not (Char.isDigit char)
                                        && not (char == '.')
                        )
                    |> Maybe.withDefault False
            )


numToInt : Num -> Int
numToInt num =
    num.numStr
        |> String.toInt
        |> Maybe.withDefault -1


gearPosToPower : Schematic -> List Num -> ( Int, Int ) -> Int
gearPosToPower schematic nums pos =
    let
        touchingNums =
            nums
                |> List.filter (isNumTouchingSymbol schematic (Just pos))
                |> List.map numToInt
    in
    if List.length touchingNums == 2 then
        List.product touchingNums

    else
        0
