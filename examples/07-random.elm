module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewImg)

import Browser
import Html exposing (..)
import Html.Attributes exposing (alt, src)
import Html.Events exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)



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
    { dieFace1 : Int
    , dieFace2 : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 0
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace ( Int, Int )


pair : Random.Generator ( Int, Int )
pair =
    Random.pair
        (Random.int 1 6)
        (Random.int 1 6)


faces : Cmd Msg
faces =
    Random.generate NewFace pair


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , faces
            )

        NewFace ( newFace1, newFace2 ) ->
            ( Model newFace1 newFace2
            , Cmd.none
            )



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


makeDie : Int -> Svg Msg
makeDie n =
    let
        f n_ =
            case n_ of
                1 ->
                    one

                2 ->
                    two

                3 ->
                    three

                4 ->
                    four

                5 ->
                    five

                6 ->
                    six

                _ ->
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
        , r "5"
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
