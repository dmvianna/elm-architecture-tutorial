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


clock : Model -> Html Msg
clock m =
    span
        [ style "margin" "3px"
        ]
        [ Svg.svg
            [ Svg.width "120"
            , Svg.height "120"
            , Svg.viewBox "0 0 120 120"
            ]
            [ Svg.circle
                [ Svg.cx "50"
                , Svg.cy "50"
                , Svg.r "50"
                , Svg.strokeWidth "3"
                , Svg.stroke "black"
                , Svg.fill "white"
                ]
                []
            ]
        ]
