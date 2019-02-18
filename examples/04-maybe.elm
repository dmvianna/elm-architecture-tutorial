import Browser
import Html exposing (Html, Attribute, span, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { celsiusInput : String
  , fahrenInput : String
  }


init : Model
init =
  { celsiusInput = ""
  , fahrenInput = "" }



-- UPDATE

type Msg
    = Celsius String
    | Fahren String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Celsius newInput ->
        { model | celsiusInput = newInput }
    Fahren newInput ->
        { model | fahrenInput = newInput }



-- VIEW


view : Model -> Html Msg
view model =
  case String.toFloat model.celsiusInput of

      Just celsius ->
          viewConverter model.celsiusInput "blue" "none" (String.fromFloat (celsius * 1.8 + 32))

      Nothing ->
          viewConverter model.celsiusInput "red" "solid" "???"


viewConverter : UserInput -> Color -> Border -> EquivalentTemp -> Html Msg
viewConverter userInput color border equivalentTemp =
    span []
        [ viewMaker userInput equivalentTemp Celsius "°C = " "°F" color border
        ]

type alias Color = String
type alias Border = String
type alias ToUnit = String
type alias FromUnit = String
type alias UserInput = String
type alias EquivalentTemp = String
        
viewMaker : UserInput -> EquivalentTemp -> (String -> Msg) -> FromUnit -> ToUnit -> Color -> Border -> Html Msg
viewMaker userInput equivalentTemp unit fromUnit toUnit color border =
  span []
    [ input [ value userInput, onInput unit, style "width" "40px" ] []
    , text fromUnit
    , span [ style "color" color
           , style "border-color" color
           , style "border-style" border ] [ text equivalentTemp ]
    , text toUnit
    ]
    
