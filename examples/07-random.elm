module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewImg)

import Browser
import Html exposing (..)
import Html.Attributes exposing (alt, src)
import Html.Events exposing (..)
import Process
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dieFace1 : Maybe Face
    , dieFace2 : Maybe Face
    , rolls : Int
    }


type Face
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Nothing 0
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace ( Face, Face )


pair : Random.Generator ( Face, Face )
pair =
    Random.pair
        (Random.weighted
            ( 1, One )
            [ ( 1, Two )
            , ( 1, Three )
            , ( 1, Four )
            , ( 1, Five )
            , ( 1, Six )
            ]
        )
        (Random.weighted
            ( 1, One )
            [ ( 1, Two )
            , ( 1, Three )
            , ( 1, Four )
            , ( 1, Five )
            , ( 6, Six )
            ]
        )


faces : Cmd Msg
faces =
    Random.generate NewFace pair


count : Model -> Int -> ( Model, Cmd Msg )
count model n =
    if model.rolls == n then
        ( { model | rolls = 0 }
        , Cmd.none
        )

    else
        ( { model | rolls = model.rolls + 1 }
        , Process.sleep 50
            |> Task.perform (\_ -> Roll)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , faces
            )

        NewFace ( newFace1, newFace2 ) ->
            let
                m =
                    { model
                        | dieFace1 = Just newFace1
                        , dieFace2 = Just newFace2
                    }
            in
            count m 10



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ viewImg model ]
        , button [ onClick Roll ] [ Html.text "Roll" ]
        ]


viewImg : Model -> Html Msg
viewImg model =
    span []
        [ makeDie model.dieFace1
        , makeDie model.dieFace2
        ]


makeDie : Maybe Face -> Svg Msg
makeDie n =
    let
        f n_ =
            case n_ of
                Just One ->
                    one

                Just Two ->
                    two

                Just Three ->
                    three

                Just Four ->
                    four

                Just Five ->
                    five

                Just Six ->
                    six

                Nothing ->
                    zero
    in
    svg
        [ width "120"
        , height "120"
        , viewBox "0 0 120 120"
        ]
        (f n)


zero =
    [ squared
    ]


one =
    [ squared
    , dot 60 60
    ]


two =
    [ squared
    , dot 30 30
    , dot 90 90
    ]


three =
    [ squared
    , dot 30 30
    , dot 60 60
    , dot 90 90
    ]


four =
    [ squared
    , dot 30 30
    , dot 30 90
    , dot 90 30
    , dot 90 90
    ]


five =
    [ squared
    , dot 30 30
    , dot 30 90
    , dot 60 60
    , dot 90 30
    , dot 90 90
    ]


six =
    [ squared
    , dot 30 30
    , dot 30 90
    , dot 30 60
    , dot 90 60
    , dot 90 30
    , dot 90 90
    ]


dot : Int -> Int -> Svg Msg
dot x y =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r "10"
        ]
        []


squared : Svg Msg
squared =
    rect
        [ x "10"
        , y "10"
        , width "100"
        , height "100"
        , rx "15"
        , ry "15"
        , fill "white"
        , stroke "black"
        , strokeWidth "4"
        ]
        []
