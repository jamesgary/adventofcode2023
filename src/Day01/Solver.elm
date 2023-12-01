module Day01.Solver exposing (..)

import Day01.Input exposing (input)
import Dict exposing (Dict)
import List.Extra


numStringDict =
    [ ( "one", 1 )
    , ( "two", 2 )
    , ( "three", 3 )
    , ( "four", 4 )
    , ( "five", 5 )
    , ( "six", 6 )
    , ( "seven", 7 )
    , ( "eight", 8 )
    , ( "nine", 9 )
    ]
        |> Dict.fromList


solve : String
solve =
    input
        |> String.trim
        |> String.lines
        |> List.map
            (\line ->
                let
                    firstNumStringWithIndex : Maybe ( Int, Int )
                    firstNumStringWithIndex =
                        numStringDict
                            |> Dict.toList
                            |> List.filterMap
                                (\( numStr, num ) ->
                                    line
                                        |> String.indexes numStr
                                        |> List.head
                                        |> Maybe.map (\index -> ( num, index ))
                                )
                            |> List.Extra.minimumBy (\( num, index ) -> index)
                            |> Debug.log "firstNumString"

                    lastNumStringWithIndex : Maybe ( Int, Int )
                    lastNumStringWithIndex =
                        numStringDict
                            |> Dict.toList
                            |> List.filterMap
                                (\( numStr, num ) ->
                                    line
                                        |> String.indexes numStr
                                        |> List.reverse
                                        |> List.head
                                        |> Maybe.map (\index -> ( num, index ))
                                )
                            |> List.Extra.maximumBy (\( num, index ) -> index)

                    chars : List String
                    chars =
                        line
                            |> String.toList
                            |> List.map String.fromChar

                    firstNumWithIndex : Maybe ( Int, Int )
                    firstNumWithIndex =
                        chars
                            |> List.indexedMap (\i char -> ( char, i ))
                            |> List.filterMap
                                (\( char, i ) ->
                                    char
                                        |> String.toInt
                                        |> Maybe.map (\int -> ( int, i ))
                                )
                            |> List.head

                    firstNum =
                        (case ( firstNumStringWithIndex, firstNumWithIndex ) of
                            ( Just ( num1, i1 ), Just ( num2, i2 ) ) ->
                                if i1 < i2 then
                                    num1

                                else
                                    num2

                            ( Just ( num, i ), Nothing ) ->
                                num

                            ( Nothing, Just ( num, i ) ) ->
                                num

                            ( Nothing, Nothing ) ->
                                0
                                    |> Debug.log "???"
                        )
                            |> String.fromInt

                    lastNumWithIndex : Maybe ( Int, Int )
                    lastNumWithIndex =
                        chars
                            |> List.indexedMap (\i char -> ( char, i ))
                            |> List.filterMap
                                (\( char, i ) ->
                                    char
                                        |> String.toInt
                                        |> Maybe.map (\int -> ( int, i ))
                                )
                            |> List.reverse
                            |> List.head

                    lastNum =
                        (case ( lastNumStringWithIndex, lastNumWithIndex ) of
                            ( Just ( num1, i1 ), Just ( num2, i2 ) ) ->
                                if i1 > i2 then
                                    num1

                                else
                                    num2

                            ( Just ( num, i ), Nothing ) ->
                                num

                            ( Nothing, Just ( num, i ) ) ->
                                num

                            ( Nothing, Nothing ) ->
                                0
                                    |> Debug.log "???!!!"
                        )
                            |> String.fromInt
                in
                (firstNum ++ lastNum)
                    |> Debug.log "OK"
                    |> String.toInt
                    |> Maybe.withDefault 0
            )
        |> List.sum
        |> String.fromInt


solve_ : String
solve_ =
    input
        |> String.trim
        |> String.lines
        |> List.map
            (\line ->
                let
                    chars =
                        line
                            |> String.toList
                            |> List.map String.fromChar

                    firstNum =
                        chars
                            |> List.filterMap String.toInt
                            |> List.map String.fromInt
                            |> List.head
                            |> Maybe.withDefault ""

                    lastNum =
                        chars
                            |> List.filterMap String.toInt
                            |> List.map String.fromInt
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault ""

                    -- _ =
                    --     Debug.log "" ( firstNum, lastNum )
                in
                (firstNum ++ lastNum)
                    |> String.toInt
                    |> Maybe.withDefault 0
            )
        |> List.sum
        |> String.fromInt
