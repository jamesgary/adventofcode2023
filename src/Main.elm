module Main exposing (..)

import Browser
import Day01.Solver
import Day02.Solver
import Day03.Solver
import Day04.Solver
import Day05.Solver
import Day06.Solver
import Day07.Solver
import Html exposing (Html)
import Html.Attributes as Attr exposing (style)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { output : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { output =
            -- Update this when starting new puzzle!
            Day07.Solver.solve
      }
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Html.div
        [ style "margin" "30px"
        , style "color" "white"
        , style "font-family" "monospace"
        , style "font-size" "64px"
        ]
        [ Html.text model.output ]
