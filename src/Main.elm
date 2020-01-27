port module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Css exposing (px, rem)
import Html as RootHtml
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D
import List.Extra exposing (dropWhile, elemIndex, last, takeWhile)


port synth : String -> Cmd msg


port triggerAttack : Float -> Cmd msg


port triggerRelease : Float -> Cmd msg


type Msg
    = TriggerAttack Float
    | TriggerRelease Float
    | IncrementOctave Id
    | DecrementOctave Id
    | ChangeMode Id ModeName
    | ChangeKey Id Key
    | KeyDown Keyboard
    | KeyUp Keyboard
    | CreateScale
    | DeleteScale Id


type alias Model =
    { scales : List Scale }


type alias Mode =
    { name : String, offset : Int }


type alias Note =
    { name : String, meta : String }


type alias Notes =
    List Note


type alias Interval =
    List Int


type alias Modes =
    List Mode


type alias Id =
    Int


type alias Octave =
    Int


type alias ModeName =
    String


type alias Key =
    String


type alias Scale =
    { id : Id
    , octave : Octave
    , mode : ModeName
    , key : Key
    }


type Status
    = Active
    | Inactive


type Keyboard
    = Character Char
    | Control String


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
    [ { id = 1, octave = 4, mode = "ionian", key = "C" } ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TriggerAttack note ->
            ( model, triggerAttack note )

        TriggerRelease note ->
            ( model, triggerRelease note )

        IncrementOctave id ->
            ( { model
                | scales = updateScales id incrementOctave model.scales
              }
            , Cmd.none
            )

        DecrementOctave id ->
            ( { model
                | scales = updateScales id decrementOctave model.scales
              }
            , Cmd.none
            )

        ChangeMode id modeName ->
            ( { model | scales = updateScales id (changeMode modeName) model.scales }, Cmd.none )

        ChangeKey id key ->
            ( { model | scales = updateScales id (changeKey key) model.scales }, Cmd.none )

        KeyDown keycode ->
            ( model, triggerAttack 261.6 )

        KeyUp keycode ->
            ( model, triggerRelease 261.6 )

        CreateScale ->
            ( { model | scales = model.scales ++ List.map (incrementId model.scales) default }, Cmd.none )

        DeleteScale id ->
            ( { model | scales = List.filter (\s -> s.id /= id) model.scales }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (D.map KeyDown keyDecoder)
        , onKeyUp (D.map KeyUp keyDecoder)
        ]


view : Model -> RootHtml.Html Msg
view model =
    toUnstyled <|
        div
            [ css
                [ Css.margin2 (rem 10) Css.auto
                , Css.padding (rem 1)
                , Css.borderRadius (px 4)
                , Css.width (px 848)
                ]
            ]
            [ div
                []
                (showScales model.scales)
            , div
                [ css
                    [ Css.property "border" "solid 1px orange"
                    , Css.margin3 (rem 5) (rem 0) Css.auto
                    ]
                ]
                [ button [ onClick CreateScale ] [ text "new" ] ]
            ]



-- ui helpers


createKey : Float -> Html Msg
createKey n =
    div
        [ css
            [ Css.width (px 40)
            , Css.height (px 40)
            , Css.backgroundColor (Css.hex "A9A9A9")
            , Css.textAlign Css.center
            , Css.padding (rem 1)
            , Css.color (Css.hex "FFF")
            ]
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
            [ css
                [ Css.property "display" "grid"
                , Css.property "grid-template-columns" "repeat(12, 40px)"
                , Css.property "grid-gap" "0.4em 2.2em"
                ]
            ]
            (showKeys scale.key scale.mode scale.octave)
        , text (String.fromInt scale.id)
        , button [ onClick (IncrementOctave scale.id) ] [ text "+" ]
        , button [ onClick (DecrementOctave scale.id) ] [ text "-" ]
        , select [ onInput (selectMode scale.id) ] (createOptions modes modeOption)
        , text (scale.mode ++ " mode")
        , select [ onInput (selectKey scale.id) ] (createOptions noteMap keyOption)
        , text (scale.key ++ " scale")
        , button [ onClick (DeleteScale scale.id) ] [ text "del" ]
        ]


createOptions : List a -> (a -> Html Msg) -> List (Html Msg)
createOptions ls createOption =
    List.map createOption ls


keyOption : Key -> Html Msg
keyOption t =
    option [ value t ] [ text t ]


modeOption : Mode -> Html Msg
modeOption m =
    option [ value m.name ] [ text m.name ]


showKeys : Key -> ModeName -> Octave -> List (Html Msg)
showKeys key mode octave =
    let
        interval =
            notesByMode mode
    in
    generateFrequencies key octave interval
        |> List.map createKey



-- input handler


keyDecoder : D.Decoder Keyboard
keyDecoder =
    D.map toKey (D.field "key" D.string)


toKey : String -> Keyboard
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string



-- updaters


updateScales : Id -> (Scale -> Scale) -> List Scale -> List Scale
updateScales id updater scales =
    List.map (updateScale id updater) scales


updateScale : Id -> (Scale -> Scale) -> Scale -> Scale
updateScale id updater scale =
    let
        newScale =
            updater scale
    in
    if scale.id == id then
        newScale

    else
        scale


incrementOctave : Scale -> Scale
incrementOctave scale =
    if scale.octave < 8 then
        { scale | octave = scale.octave + 1 }

    else
        scale


decrementOctave : Scale -> Scale
decrementOctave scale =
    if scale.octave > 0 then
        { scale | octave = scale.octave - 1 }

    else
        scale


changeMode : ModeName -> Scale -> Scale
changeMode modeName =
    \scale -> { scale | mode = modeName }


changeKey : Key -> Scale -> Scale
changeKey key =
    \scale -> { scale | key = key }


incrementId : List Scale -> Scale -> Scale
incrementId scales =
    case last scales of
        Nothing ->
            \scale -> scale

        Just last ->
            \scale -> { scale | id = last.id + 1 }



-- constants


temperament : Float
temperament =
    1


tones : Float
tones =
    12


noteMap : List Key
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


major : Interval
major =
    [ 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1 ]


modes : Modes
modes =
    [ Mode "ionian" 0, Mode "dorian" 2, Mode "phrygian" 4, Mode "lydian" 5, Mode "mixolydian" 6, Mode "aeolian" 8, Mode "locrian" 10 ]


selectKey : Id -> Key -> Msg
selectKey id =
    \key -> ChangeKey id key


getModeByName : ModeName -> Mode
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


selectMode : Id -> ModeName -> Msg
selectMode id =
    \modeName -> ChangeMode id modeName


rotate : Int -> List a -> List a
rotate n list =
    List.drop n list ++ List.take n list



-- ea note in a mode is represented by number of steps


notesByMode : ModeName -> Interval
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


generateFrequencies : Key -> Octave -> Interval -> List Float
generateFrequencies key octave notes =
    let
        notesInMode =
            shiftByKey key notes
    in
    List.map (generatePitch octave) notesInMode



-- shift the steps in the mode by the key


shiftByKey : Key -> Interval -> Interval
shiftByKey key notes =
    let
        keyStep =
            convertKeyToIndex key
    in
    List.map (\n -> n + keyStep) notes



-- generate pitches based on equal temperament and 12 tones by default


generatePitch : Octave -> Int -> Float
generatePitch octave =
    \steps -> base * 2 ^ (toFloat octave + toFloat steps * temperament / tones)


convertKeyToIndex : Key -> Int
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
