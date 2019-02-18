import Browser
import Html exposing (Html, Attribute, div, span, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { celsius : String
  , fahren : String
  }


init : Model
init =
  { celsius = ""
  , fahren = "" }


-- UPDATE


type Msg
    = Celsius String
    | Fahren String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Celsius t ->
        { model | celsius = t }
    Fahren t ->
        { model | fahren = t }


-- VIEW


type alias Color = String
type alias Border = String
type alias ToUnit = String
type alias FromUnit = String
type alias UserInput = String
type alias EquivalentTemp = String


viewMaybe : UserInput -> (Float -> Float) -> Html Msg
viewMaybe userInput f =
    case String.toFloat userInput of
        Just value ->
            viewTo "blue" "none"
                (String.fromFloat (f value))
        Nothing ->
            viewTo "red" "solid" "???"


viewTo : Color -> Border -> EquivalentTemp -> Html Msg
viewTo color border equivalentTemp =
    span [ style "color" color
         , style "border-color" color
         , style "border-style" border ] [ text equivalentTemp ]


viewFrom : UserInput -> (String -> Msg) -> Html Msg
viewFrom userInput msg =
    input [ value userInput, onInput msg, style "width" "40px" ] []


celsiusToFahren : Float -> Float
celsiusToFahren t =
    t * 1.8 + 32


fahrenToCelsius : Float -> Float
fahrenToCelsius t =
    (t - 32) / 1.8


view : Model -> Html Msg
view model =
    span []
        [ div []
             [ viewFrom model.celsius Celsius
             , text "°C = "
             , viewMaybe model.celsius celsiusToFahren
             , text "°F"
             ]
        , div []
             [ viewFrom model.fahren Fahren
             , text "°F = "
             , viewMaybe model.fahren fahrenToCelsius
             , text "°C"
             ]
        ]
