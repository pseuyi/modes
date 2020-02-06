port module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Css exposing (pct, px, rem)
import Html as RootHtml
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D
import List.Extra exposing (dropWhile, elemIndex, last, takeWhile)



-- port synth : String -> Cmd msg


port connectMIDI : (String -> msg) -> Sub msg


port receiveMIDINoteOn : (Float -> msg) -> Sub msg


port receiveMIDINoteOff : (Float -> msg) -> Sub msg


port triggerAttack : Float -> Cmd msg


port triggerRelease : Float -> Cmd msg


type Msg
    = TriggerAttack Float
    | TriggerRelease Float
    | IncrementOctave Id
    | DecrementOctave Id
    | ChangeMode Id ModeName
    | ChangeKey Id Key
    | ChangeTones Id Tones
    | KeyDown Keyboard
    | KeyUp Keyboard
    | CreateScale
    | DeleteScale Id
    | ConnectDevice String
    | ReceiveMIDINoteOn Float
    | ReceiveMIDINoteOff Float


type alias Model =
    { scales : List Scale, connectedDevice : String }


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


type alias Tones =
    Float


type alias Scale =
    { id : Id
    , octave : Octave
    , mode : ModeName
    , key : Key
    , tones : Tones
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
    ( { scales = default, connectedDevice = "" }, Cmd.none )


default =
    [ { id = 1, octave = 4, mode = "ionian", key = "C", tones = 12 } ]


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

        ChangeTones id tones ->
            ( { model | scales = updateScales id (\scale -> { scale | tones = tones }) model.scales }, Cmd.none )

        KeyDown keycode ->
            ( model, triggerAttack 261.6 )

        KeyUp keycode ->
            ( model, triggerRelease 261.6 )

        CreateScale ->
            ( { model | scales = model.scales ++ List.map (incrementId model.scales) default }, Cmd.none )

        DeleteScale id ->
            ( { model | scales = List.filter (except id) model.scales }, Cmd.none )

        ConnectDevice name ->
            ( { model | connectedDevice = name }, Cmd.none )

        ReceiveMIDINoteOn note ->
            ( model, triggerAttack note )

        ReceiveMIDINoteOff note ->
            ( model, triggerRelease note )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (D.map KeyDown keyDecoder)
        , onKeyUp (D.map KeyUp keyDecoder)
        , connectMIDI (\n -> ConnectDevice n)
        , receiveMIDINoteOn (\n -> ReceiveMIDINoteOn n)
        , receiveMIDINoteOff (\n -> ReceiveMIDINoteOff n)
        ]


view : Model -> RootHtml.Html Msg
view model =
    toUnstyled <|
        div
            [ css
                [ Css.margin2 (rem 10) Css.auto
                , Css.padding (rem 1)
                , Css.borderRadius (px 4)
                , Css.width (pct 90)
                , Css.textAlign Css.center
                ]
            ]
            [ div []
                [ text
                    (if model.connectedDevice /= "" then
                        "currently connect to: " ++ model.connectedDevice

                     else
                        ""
                    )
                ]
            , ul
                [ css [ Css.padding (px 0), Css.listStyle Css.none ] ]
                (showScales model.scales)
            , div
                [ css
                    [ Css.margin3 (rem 5) (rem 0) Css.auto
                    ]
                ]
                [ button [ onClick CreateScale ] [ text "new" ] ]
            ]



-- ui helpers


createKey : Float -> Html Msg
createKey n =
    li
        [ css
            [ Css.backgroundColor (Css.hex "A9A9A9")
            , Css.textAlign Css.center
            , Css.padding (rem 0.5)
            , Css.color (Css.hex "FFF")
            , Css.margin (px 2)
            , Css.property "flex-grow" "1"
            ]
        , onMouseDown (TriggerAttack n)
        , onMouseUp (TriggerRelease n)
        ]
        [ text (String.fromInt (round n)) ]


showScales : List Scale -> List (Html Msg)
showScales scales =
    List.map createScale scales


createScale : Scale -> Html Msg
createScale scale =
    li [ css [ Css.property "display" "block" ] ]
        [ ul
            [ css
                [ Css.property "display" "flex"
                , Css.padding (px 0)
                , Css.listStyle Css.none
                , Css.property "justify-content" "center"
                ]
            ]
            (showKeys scale.key scale.mode scale.octave scale.tones)
        , text (String.fromInt scale.id)
        , button [ onClick (IncrementOctave scale.id) ] [ text "+" ]
        , button [ onClick (DecrementOctave scale.id) ] [ text "-" ]
        , select [ onInput (selectMode scale.id) ] (createOptions modes modeOption)
        , text (scale.mode ++ " mode")
        , select [ onInput (selectKey scale.id) ] (createOptions noteMap keyOption)
        , text (scale.key ++ " scale")
        , input [ onInput (changeTones scale.id), placeholder "# of tones" ] []
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
    option [ selected ("ionian" == m.name), value m.name ] [ text m.name ]


showKeys : Key -> ModeName -> Octave -> Tones -> List (Html Msg)
showKeys key mode octave tones =
    let
        interval =
            notesByMode mode
    in
    generateFrequencies key octave interval tones
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


except : Id -> Scale -> Bool
except id =
    \scale -> scale.id /= id || scale.id == 1



-- constants


temperament : Float
temperament =
    1


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



-- input handlers


selectKey : Id -> Key -> Msg
selectKey id =
    \key -> ChangeKey id key


selectMode : Id -> ModeName -> Msg
selectMode id =
    \modeName -> ChangeMode id modeName


changeTones : Id -> String -> Msg
changeTones id =
    \tones ->
        ChangeTones id
            (Maybe.withDefault 12 (String.toFloat tones))


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


generateFrequencies : Key -> Octave -> Interval -> Tones -> List Float
generateFrequencies key octave notes tones =
    let
        notesInMode =
            if tones == 12 then
                shiftByKey key notes

            else
                -- steps for a multitonal 'mode'
                List.range 0 (round tones)
    in
    List.map (generatePitch octave tones) notesInMode



-- shift the steps in the mode by the key


shiftByKey : Key -> Interval -> Interval
shiftByKey key notes =
    let
        keyStep =
            convertKeyToIndex key
    in
    List.map (\n -> n + keyStep) notes



-- generate pitches based on equal temperament and 12 tones by default


generatePitch : Octave -> Tones -> Int -> Float
generatePitch octave tones =
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
