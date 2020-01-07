port module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import List.Extra exposing (dropWhile, elemIndex, takeWhile)


port synth : String -> Cmd msg


port triggerAttack : Float -> Cmd msg


port triggerRelease : Float -> Cmd msg


type Msg
    = TriggerAttack Float
    | TriggerRelease Float
    | IncrementOctave
    | DecrementOctave
    | ChangeMode String
    | ChangeKey String


type alias Model =
    { octave : Int, mode : String, key : String }


type Status
    = Active
    | Inactive


type alias Mode =
    { name : String, offset : Int }


type alias Note =
    { name : String, meta : String }


type alias Notes =
    List Note


type alias Modes =
    List Mode


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { octave = 4, mode = "ionian", key = "C" }, Cmd.none )


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

        ChangeMode id ->
            ( { model | mode = id }, Cmd.none )

        ChangeKey id ->
            ( { model | key = id }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ style "margin" "10em auto"
        , style "padding" "1em"
        , style "border-radius" "4px"
        , style "width" "848px"
        ]
        [ div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(12, 40px)"
            , style "grid-gap" "0.4em 2.2em"
            ]
            (showKeys model.key model.mode model.octave)
        , button [ onClick IncrementOctave ] [ text "+" ]
        , button [ onClick DecrementOctave ] [ text "-" ]
        , select [ onInput selectMode ] (createOptions modes modeOption)
        , text (model.mode ++ " mode")
        , select [ onInput selectKey ] (createOptions noteMap keyOption)
        , text (model.key ++ " scale")
        ]



-- constants


noteMap =
    [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]


base : Float
base =
    16.35


majorIntervals =
    [ 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1 ]


modes : Modes
modes =
    [ Mode "ionian" 0, Mode "dorian" 2, Mode "phrygian" 4, Mode "lydian" 5, Mode "mixolydian" 6, Mode "aeolian" 8, Mode "locrian" 10 ]


selectKey : String -> Msg
selectKey id =
    ChangeKey id


keyOption : String -> Html Msg
keyOption t =
    option [ value t ] [ text t ]


getModeByName : String -> Mode
getModeByName name =
    let
        match =
            List.filter (\n -> n.name == name) modes
    in
    case match of
        head :: rest ->
            Mode head.name head.offset

        [] ->
            Mode "ionian" 0


selectMode : String -> Msg
selectMode id =
    ChangeMode id


modeOption : Mode -> Html Msg
modeOption m =
    option [ value m.name ] [ text m.name ]


createOptions : List a -> (a -> Html Msg) -> List (Html Msg)
createOptions ls createOption =
    List.map createOption ls


rotate : Int -> List a -> List a
rotate n list =
    List.drop n list ++ List.take n list


showKeys : String -> String -> Int -> List (Html Msg)
showKeys key mode octave =
    generateFrequencies key octave
        |> List.map createKey


generateFrequencies : String -> Int -> List Float
generateFrequencies key octave =
    let
        start =
            convertKeyToIndex key
    in
    List.map (generatePitch octave) (List.range start (start + 11))


generatePitch : Int -> Int -> Float
generatePitch octave =
    \n -> base * 2 ^ (toFloat octave + toFloat n / 12)


convertKeyToIndex : String -> Int
convertKeyToIndex t =
    let
        result =
            elemIndex t noteMap
    in
    case result of
        Just y ->
            y

        Nothing ->
            0


createKey : Float -> Html Msg
createKey n =
    div
        [ style "width" "40px"
        , style "height" "40px"
        , style "background-color" "darkgrey"
        , style "text-align" "center"
        , style "padding" "1em"
        , style "color" "white"
        , onMouseDown (TriggerAttack n)
        , onMouseUp (TriggerRelease n)
        ]
        [ text (String.fromFloat n) ]
