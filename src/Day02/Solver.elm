module Day02.Solver exposing (..)

import Day02.Input exposing (input)
import Dict exposing (Dict)
import List.Extra


actualDice : Dict String Int
actualDice =
    [ ( "red", 12 )
    , ( "green", 13 )
    , ( "blue", 14 )
    ]
        |> Dict.fromList


emptyRoll : Dict String Int
emptyRoll =
    [ ( "red", 0 )
    , ( "green", 0 )
    , ( "blue", 0 )
    ]
        |> Dict.fromList


type alias Game =
    { id : Int
    , rolls : List Roll
    }


type alias Roll =
    Dict String Int


solve : String
solve =
    input
        |> String.trim
        |> String.lines
        |> List.map lineToGame
        |> List.map minDice
        |> List.map diceToPower
        |> List.sum
        |> String.fromInt


solve_ : String
solve_ =
    input
        |> String.trim
        |> String.lines
        |> List.map lineToGame
        |> List.filter isGameValid
        |> List.map .id
        |> List.sum
        |> String.fromInt


lineToGame : String -> Game
lineToGame str =
    case String.split ":" str of
        [ idStr, gamesStr ] ->
            { id =
                idStr
                    -- drop "Game "
                    |> String.dropLeft 5
                    |> String.toInt
                    |> Maybe.withDefault -1
            , rolls =
                gamesStr
                    |> String.trim
                    |> strToRolls
            }

        _ ->
            Debug.todo ("line bad: " ++ str)


strToRolls : String -> List Roll
strToRolls str =
    str
        |> Debug.log "?"
        |> String.split ";"
        |> List.map String.trim
        |> List.map
            (\rollStr ->
                rollStr
                    |> String.split ", "
                    |> List.foldl
                        (\numDiceStr roll ->
                            case String.split " " numDiceStr of
                                [ numStr, color ] ->
                                    let
                                        num =
                                            String.toInt numStr
                                                |> Maybe.withDefault -1
                                    in
                                    roll
                                        |> Dict.update color (Maybe.map (\orig -> orig + num))

                                _ ->
                                    Debug.todo ("bad numDiceStr: " ++ numDiceStr)
                        )
                        emptyRoll
            )


isGameValid : Game -> Bool
isGameValid game =
    game.rolls
        |> List.all
            (\roll ->
                roll
                    |> Dict.toList
                    |> List.all
                        (\( col, num ) ->
                            Dict.get col actualDice
                                |> Maybe.withDefault 0
                                |> (\n -> num <= n)
                        )
            )


minDice : Game -> Roll
minDice game =
    game.rolls
        |> List.foldl
            (\roll result ->
                Dict.merge
                    -- only empty
                    (\col num -> Dict.insert col num)
                    -- both
                    (\col num1 num2 dict ->
                        if num1 > num2 then
                            dict |> Dict.insert col num1

                        else
                            dict |> Dict.insert col num2
                    )
                    -- only right (impossible)
                    (\col num -> Dict.insert col num)
                    result
                    roll
                    Dict.empty
            )
            emptyRoll


diceToPower : Roll -> Int
diceToPower roll =
    roll
        |> Dict.values
        |> List.product
