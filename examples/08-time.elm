module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as Svg
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) False
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            if not model.paused then
                ( { model | time = newTime }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = not model.paused }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


zeroPrefix : String -> String
zeroPrefix s =
    if String.length s == 1 then
        "0" ++ s

    else
        s


view : Model -> Html Msg
view model =
    let
        hour =
            zeroPrefix <| String.fromInt (Time.toHour model.zone model.time)

        minute =
            zeroPrefix <| String.fromInt (Time.toMinute model.zone model.time)

        second =
            zeroPrefix <| String.fromInt (Time.toSecond model.zone model.time)
    in
    div []
        [ h1
            [ style "text-align" "center"
            , style "left" "50%"
            , font
            ]
            [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , button
            [ style "display" "block"
            , style "margin" "auto"
            , font
            , onClick Pause
            ]
            [ Html.text "pause" ]
        , clock model
        ]


font =
    style "font-family" "'Lucida Console',Monaco,monospace"


timeToAngle : Int -> Int -> Float
timeToAngle time unit =
    turns <| toFloat time / toFloat unit + radians (pi / 4.2)


hand : Int -> Float -> Float -> String -> Svg.Svg Msg
hand frameSide x y colour =
    Svg.line
        [ Svg.x1 <| String.fromInt (frameSide // 2)
        , Svg.y1 <| String.fromInt (frameSide // 2)
        , Svg.x2 <| String.fromFloat x
        , Svg.y2 <| String.fromFloat y
        , Svg.strokeWidth "3"
        , Svg.stroke colour
        ]
        []


clock : Model -> Html Msg
clock m =
    let
        frameSide =
            120

        radius =
            50

        angleSec =
            timeToAngle (Time.toSecond m.zone m.time) 60

        angleMin =
            timeToAngle (Time.toMinute m.zone m.time) 60

        xSec =
            frameSide / 2 + radius * cos angleSec

        ySec =
            frameSide / 2 + radius * sin angleSec

        xMin =
            frameSide / 2 + (radius - 10) * cos angleMin

        yMin =
            frameSide / 2 + (radius - 10) * sin angleMin
    in
    div
        [ style "text-align" "center"
        ]
        [ Svg.svg
            [ Svg.width <| String.fromInt frameSide
            , Svg.height <| String.fromInt frameSide
            , Svg.viewBox <| "0 0 " ++ String.fromInt frameSide ++ String.fromInt frameSide
            ]
            [ Svg.circle
                [ Svg.cx "50%"
                , Svg.cy "50%"
                , Svg.r <| String.fromInt radius
                , Svg.strokeWidth "3"
                , Svg.stroke "black"
                , Svg.fill "white"
                ]
                []
            , hand frameSide xSec ySec "red"
            , hand frameSide xMin yMin "black"
            ]
        ]
