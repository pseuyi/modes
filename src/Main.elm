port module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Encode as E


port synth : String -> Cmd msg


port triggerAttack : String -> Cmd msg


port triggerRelease : String -> Cmd msg


type Msg
    = TriggerAttack String
    | TriggerRelease String
    | IncrementOctave
    | DecrementOctave


type alias Model =
    { octave : Int }


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { octave = 4 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TriggerAttack note ->
            ( model, triggerAttack note )

        TriggerRelease note ->
            ( model, triggerRelease note )

        IncrementOctave ->
            ( { model
                | octave =
                    if model.octave < 8 then
                        model.octave + 1

                    else
                        model.octave
              }
            , Cmd.none
            )

        DecrementOctave ->
            ( { model
                | octave =
                    if model.octave > 0 then
                        model.octave - 1

                    else
                        model.octave
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ style "margin" "10em auto"
        , style "padding" "1em"
        , style "border" "solid 1px darkgrey"
        , style "border-radius" "4px"
        , style "width" "848px"
        ]
        [ div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(12, 40px)"
            , style "grid-gap" "0.4em 2.2em"
            ]
            (showKeys model.octave)
        , button [ onClick IncrementOctave ] [ text "+" ]
        , button [ onClick DecrementOctave ] [ text "-" ]
        ]


notes : List String
notes =
    [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]


showKeys : Int -> List (Html Msg)
showKeys octave =
    List.map (\n -> key n) (List.map (\n -> n ++ String.fromInt octave) notes)


key : String -> Html Msg
key note =
    div
        [ style "width" "40px"
        , style "height" "40px"
        , style "background-color" "darkgrey"
        , style "color" "white"
        , style "text-align" "center"
        , style "padding" "1em"
        , onMouseDown (TriggerAttack note)
        , onMouseUp (TriggerRelease note)
        ]
        [ text note ]
