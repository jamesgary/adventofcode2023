module Day04.Solver exposing (..)

import Day04.Input exposing (input)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


solve : String
solve =
    let
        cardsDict : Dict Int String
        cardsDict =
            input
                |> String.trim
                |> String.lines
                |> List.indexedMap (\i l -> ( i + 1, l ))
                |> Dict.fromList

        initCardsOwned : List Int
        initCardsOwned =
            List.range 1 (Dict.size cardsDict)
    in
    cardsDict
        |> Dict.toList
        |> List.foldl
            (\( cardId, cardStr ) cardsOwned ->
                case String.split ":" cardStr of
                    [ cardIdStr_, cardNumsStr ] ->
                        case String.split "|" cardNumsStr of
                            [ winningNumStrs, myNumStrs ] ->
                                let
                                    numMatchingNums =
                                        matchingNums (winningNumStrs |> String.trim) (myNumStrs |> String.trim)
                                            |> List.length

                                    numCardsOwnedOfId =
                                        cardsOwned
                                            |> List.Extra.count ((==) cardId)

                                    newCards =
                                        if numMatchingNums > 0 then
                                            List.range 1 numMatchingNums
                                                |> List.map ((+) cardId)
                                                |> List.repeat numCardsOwnedOfId
                                                |> List.concat

                                        else
                                            []

                                    _ =
                                        Debug.log ("Card " ++ String.fromInt cardId ++ " had " ++ String.fromInt numMatchingNums ++ " matching nums, and had " ++ String.fromInt numCardsOwnedOfId ++ " copies, so generated the new cards " ++ Debug.toString newCards)

                                    -- ()
                                in
                                cardsOwned ++ newCards

                            _ ->
                                Debug.todo ("bad nums: " ++ cardNumsStr)

                    _ ->
                        Debug.todo ("bad cardStr: " ++ cardStr)
            )
            initCardsOwned
        |> List.length
        |> String.fromInt


matchingNums : String -> String -> List Int
matchingNums winningNumsStr myNumsStr =
    let
        winningNums =
            winningNumsStr
                |> String.split " "
                |> List.filterMap String.toInt
                |> Set.fromList

        myNums =
            myNumsStr
                |> String.split " "
                |> List.filterMap String.toInt
                |> Set.fromList
    in
    Set.intersect winningNums myNums
        |> Set.toList



------------


solve_ : String
solve_ =
    input
        |> String.trim
        |> String.lines
        |> List.map
            (\line ->
                case String.split ":" line of
                    [ cardIdStr_, cardNumsStr ] ->
                        case String.split "|" cardNumsStr of
                            [ winningNumStrs, myNumStrs ] ->
                                matchingNums (winningNumStrs |> String.trim) (myNumStrs |> String.trim)
                                    |> List.length
                                    |> (\matchCount ->
                                            if matchCount <= 0 then
                                                0

                                            else
                                                2 ^ (matchCount - 1)
                                       )

                            _ ->
                                Debug.todo ("bad nums: " ++ cardNumsStr)

                    _ ->
                        Debug.todo ("bad line: " ++ line)
            )
        |> List.sum
        |> String.fromInt
