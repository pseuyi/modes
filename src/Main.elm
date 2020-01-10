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
    | IncrementOctave Int
    | DecrementOctave Int
    | ChangeMode String
    | ChangeKey String


type alias Model =
    { scales : List Scale }


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


type alias Key =
    String


type alias Scale =
    { id : Int
    , octave : Octave
    , mode : String
    , key : String
    }


type alias Octave =
    Int


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { scales = default }, Cmd.none )


default =
    [ { id = 1, octave = 4, mode = "ionian", key = "C" }, { id = 2, octave = 4, mode = "dorian", key = "D" }, { id = 3, octave = 4, mode = "phrygian", key = "E" }, { id = 4, octave = 4, mode = "lydian", key = "F" } ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TriggerAttack note ->
            ( model, triggerAttack note )

        TriggerRelease note ->
            ( model, triggerRelease note )

        IncrementOctave id ->
            ( let
                newScales =
                    List.map (updateScale id) model.scales
              in
              { model
                | scales = newScales
              }
            , Cmd.none
            )

        DecrementOctave id ->
            ( let
                newScales =
                    List.map (updateScale id) model.scales
              in
              { model
                | scales = newScales
              }
            , Cmd.none
            )

        ChangeMode id ->
            ( model, Cmd.none )

        ChangeKey id ->
            ( model, Cmd.none )


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
        (showScales model.scales)


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


showScales : List Scale -> List (Html Msg)
showScales scales =
    List.map createScale scales


createScale : Scale -> Html Msg
createScale scale =
    div []
        [ div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(12, 40px)"
            , style "grid-gap" "0.4em 2.2em"
            ]
            (showKeys scale.key scale.mode scale.octave)
        , button [ onClick (IncrementOctave scale.id) ] [ text "+" ]
        , button [ onClick (DecrementOctave scale.id) ] [ text "-" ]
        , select [ onInput selectMode ] (createOptions modes modeOption)
        , text (scale.mode ++ " mode")
        , select [ onInput selectKey ] (createOptions noteMap keyOption)
        , text (scale.key ++ " scale")
        ]


updateScale : Int -> Scale -> Scale
updateScale id scale =
    if scale.id == id then
        { scale | octave = incrementOctave scale.octave }

    else
        scale


incrementOctave : Octave -> Octave
incrementOctave octave =
    if octave < 8 then
        octave + 1

    else
        octave



-- constants


temperament : Float
temperament =
    1


tones : Float
tones =
    12


noteMap : List String
noteMap =
    [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]


base : Float
base =
    16.35



-- TODO: each step is equal but this may change


step : Float
step =
    temperament


halfStep : Float
halfStep =
    step


wholeStep : Float
wholeStep =
    halfStep + halfStep


major =
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
    generateFrequencies key octave (notesByMode mode)
        |> List.map createKey



-- ea note in a mode is represented by number of steps


notesByMode : String -> List Int
notesByMode m =
    let
        mode =
            getModeByName m

        modeIntervals =
            rotate (mode.offset * 2) major

        peek =
            case modeIntervals of
                x :: xs ->
                    [ x ]

                [] ->
                    []
    in
    List.append modeIntervals peek
        |> List.indexedMap Tuple.pair
        |> List.filter (\t -> Tuple.second t == 1)
        |> List.map (\t -> Tuple.first t)


generateFrequencies : String -> Int -> List Int -> List Float
generateFrequencies key octave notes =
    let
        notesInMode =
            shiftByKey key notes
    in
    List.map (generatePitch octave) notesInMode



-- shift the steps in the mode by the key


shiftByKey : Key -> List Int -> List Int
shiftByKey key notes =
    let
        keyStep =
            convertKeyToIndex key
    in
    List.map (\n -> n + keyStep) notes



-- generate pitches based on equal temperament and 12 tones by default


generatePitch : Int -> Int -> Float
generatePitch octave =
    \steps -> base * 2 ^ (toFloat octave + toFloat steps * temperament / tones)


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
