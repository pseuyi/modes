module Main exposing (Freq(..), Interval(..), Mode, Scale, halfStep, major, middleA, middleC, shift, viewNote, wholeStep)


type Freq
    = Freq Float


type Interval
    = Interval Float


middleA : Freq
middleA =
    44100



-- khz


halfStep : Interval
halfStep =
    500



--
-- init/update kinda stuff


middleC : Freq
middleC =
    shift wholeStep middleA
        |> shift halfStep


wholeStep : Interval
wholeStep =
    halfStep + halfStep


shift : Freq -> Interval -> Freq
shift (Freq f) (Interval i) =
    Freq (f + i)


type alias Scale =
    List Interval


major : Scale
major =
    [ wholeStep, wholeStep, halfStep, wholeStep, wholeStep, wholeStep, halfStep ]


type alias Mode =
    { root : Freq
    , scale : Scale
    }



-- view stuff


viewNote : Freq -> Scale -> Html msg
viewNote (Freq f) scale =
    if f == middleC then
        text "C"

    else if f == middleA then
        text "A"

    else
        text (String.fromFloat f ++ "kHz")
