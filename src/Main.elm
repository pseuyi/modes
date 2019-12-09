port module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D


port synth : String -> Cmd msg


port triggerAttack : String -> Cmd msg


port triggerRelease : String -> Cmd msg


type Msg
    = TriggerAttack String
    | TriggerRelease String
    | IncrementOctave
    | DecrementOctave
    | ChangeMode String
    | ChangeTonic String


type alias Model =
    { octave : Int, mode : String, tonic : String }


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
    ( { octave = 4, mode = "ionian", tonic = "C" }, Cmd.none )


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

        ChangeTonic id ->
            ( { model | tonic = id }, Cmd.none )


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
            (showKeys model.tonic model.mode model.octave)
        , button [ onClick IncrementOctave ] [ text "+" ]
        , button [ onClick DecrementOctave ] [ text "-" ]
        , select [ onInput selectMode ] (createOptions modes modeOption)
        , text (model.mode ++ " mode")
        , select [ onInput selectTonic ] (createOptions tonics tonicOption)
        , text (model.tonic ++ " scale")
        ]



-- constants


notes : Notes
notes =
    [ Note "C" "B#", Note "C#" "Db", Note "D" "", Note "D#" "Eb", Note "E" "Fb", Note "F" "E#", Note "F#" "Gb", Note "G" "", Note "G#" "Ab", Note "A" "", Note "A#" "Bb", Note "B" "Cb" ]


majorIntervals =
    [ 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1 ]


modes : Modes
modes =
    [ Mode "ionian" 0, Mode "dorian" 1, Mode "phrygian" 2, Mode "lydian" 3, Mode "mixolydian" 4, Mode "aeolian" 5, Mode "locrian" 6 ]


tonics =
    List.map .name notes


selectTonic : String -> Msg
selectTonic id =
    ChangeTonic id


tonicOption : String -> Html Msg
tonicOption t =
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



-- the step pattern for a mode


modeIntervals : String -> List Int
modeIntervals m =
    let
        mode =
            getModeByName m
    in
    rotate (mode.offset * 2) majorIntervals



-- notes starting at given tonic


tonicNotes : String -> Notes -> Notes
tonicNotes t ns =
    dropWhile (\n -> n.name /= t) ns ++ takeWhile (\n -> n.name /= t) ns


rotate : Int -> List a -> List a
rotate n list =
    List.drop n list ++ List.take n list


showKeys : String -> String -> Int -> List (Html Msg)
showKeys tonic mode octave =
    List.map2 activeNotes (modeIntervals mode) (tonicNotes tonic notes)
        |> List.map (addOctave octave)
        |> List.map createKey


activeNotes : Int -> Note -> Note
activeNotes i n =
    if i == 1 then
        n

    else
        -- TODO get the correct name from meta before reassigning...
        Note n.name "inactive"


addOctave : Int -> Note -> Note
addOctave octave =
    \n -> Note (n.name ++ String.fromInt octave) n.meta


createKey : Note -> Html Msg
createKey n =
    let
        note =
            n.name

        color =
            if n.meta == "inactive" then
                "grey"

            else
                "white"
    in
    div
        [ style "width" "40px"
        , style "height" "40px"
        , style "background-color" "darkgrey"
        , style "text-align" "center"
        , style "padding" "1em"
        , style "color" color
        , onMouseDown (TriggerAttack note)
        , onMouseUp (TriggerRelease note)
        ]
        [ text note ]


{-| Take elements in order as long as the predicate evaluates to `True`
-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                x :: takeWhile predicate xs

            else
                []


{-| Drop elements in order as long as the predicate evaluates to `True`
-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs

            else
                list
