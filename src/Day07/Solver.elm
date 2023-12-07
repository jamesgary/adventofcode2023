module Day07.Solver exposing (..)

import Day07.Input exposing (input)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


ranks : List Char
ranks =
    [ 'A'
    , 'K'
    , 'Q'
    , 'T'
    , '9'
    , '8'
    , '7'
    , '6'
    , '5'
    , '4'
    , '3'
    , '2'
    , 'J' --joker
    ]


rankToComparable : Char -> Int
rankToComparable c =
    List.Extra.elemIndex c ranks
        |> Maybe.withDefault 999


type Kind
    = FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard


allKinds : List Kind
allKinds =
    [ FiveOfAKind
    , FourOfAKind
    , FullHouse
    , ThreeOfAKind
    , TwoPair
    , OnePair
    , HighCard
    ]


kindToComparable : Kind -> Int
kindToComparable kind =
    List.Extra.elemIndex kind allKinds
        |> Maybe.withDefault 999


solve : String
solve =
    handsAndBids
        |> List.map (\( h, b ) -> ( ( h, b ), handToComparable h ))
        |> List.sortBy Tuple.second
        |> Debug.log ""
        |> List.map Tuple.first
        |> List.reverse
        |> List.indexedMap
            (\i ( h, b ) ->
                b * (i + 1)
            )
        |> List.sum
        |> Debug.toString


handsAndBids : List ( List Char, Int )
handsAndBids =
    input
        |> String.trim
        |> String.lines
        |> List.map
            (\line ->
                case line |> String.split " " of
                    [ handStr, bidStr ] ->
                        let
                            hand =
                                handStr
                                    |> String.toList

                            bid =
                                bidStr
                                    |> String.toInt
                                    |> Maybe.withDefault -1
                        in
                        ( hand, bid )

                    _ ->
                        Debug.todo ("bad line: " ++ line)
            )


handToKind : List Char -> Kind
handToKind hand =
    allKinds
        |> List.Extra.find
            (\kind ->
                isHand kind hand
            )
        |> Maybe.withDefault HighCard


isHand : Kind -> List Char -> Bool
isHand kind hand_ =
    let
        hand =
            hand_
                |> List.sort

        groupedHand =
            hand
                |> List.Extra.group
                |> List.map
                    (\( c, cs ) ->
                        c :: cs
                    )

        jokerless =
            groupedHand
                |> List.filter
                    (\cs ->
                        List.all ((/=) 'J') cs
                    )

        sortedJokerlessGroupSizes =
            jokerless
                |> List.map List.length
                |> List.sort
    in
    case kind of
        FiveOfAKind ->
            (List.length jokerless == 1)
                || (List.length jokerless == 0)

        FourOfAKind ->
            case sortedJokerlessGroupSizes of
                [ 1, _ ] ->
                    True

                _ ->
                    False

        FullHouse ->
            case sortedJokerlessGroupSizes of
                [ 2, 3 ] ->
                    True

                [ 2, _ ] ->
                    True

                _ ->
                    False

        ThreeOfAKind ->
            case sortedJokerlessGroupSizes of
                [ 1, 1, _ ] ->
                    True

                _ ->
                    False

        TwoPair ->
            case sortedJokerlessGroupSizes of
                [ 1, 2, 2 ] ->
                    True

                _ ->
                    False

        OnePair ->
            (groupedHand
                |> List.filter
                    (\cs -> List.length cs == 2)
                |> List.length
                |> (==) 1
            )
                || (hand |> List.member 'J')

        HighCard ->
            True


handToComparable : List Char -> List Int
handToComparable hand =
    let
        kind =
            hand
                |> handToKind
                --|> Debug.log (Debug.toString hand)
                |> kindToComparable

        hand_ =
            hand
                |> List.map rankToComparable
    in
    kind :: hand_
