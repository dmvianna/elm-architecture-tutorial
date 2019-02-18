import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , validate : Bool
  }


init : Model
init =
  Model "" "" "" "" False



-- UPDATE

type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Validate


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
        { model | name = name, validate = False }

    Password password ->
        { model | password = password, validate = False }

    PasswordAgain password ->
        { model | passwordAgain = password, validate = False }

    Age age ->
        { model | age = age, validate = False }
    Validate ->
        { model | validate = True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewInput "age" "Age" model.age Age
        , button [ onClick Validate ] [ text "validate" ]
        , viewValidate model
        ]

        
viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidate : Model -> Html msg
viewValidate model =
    if model.validate == True
    then viewValidation model
    else div [] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.password /= model.passwordAgain then
        viewDivError "Passwords do not match!"
    else if String.length model.password < 8 then
        viewDivError "Password is too short!"
    else if not <| hasUpperLowerDigit model.password then
        viewDivError "Password must have uppercase, lowercase, and digit characters!"
    else if not <| allChar Char.isDigit model.age then
        viewDivError "Age must only contain digits!"
    else
        div [ style "color" "green" ] [ text "OK" ]


viewDivError : String -> Html msg
viewDivError str =
    div [ style "color" "red" ] [ text str ]


hasChar : (Char -> Bool) -> String -> Bool
hasChar f s =
    String.length (String.filter f s) > 0


allChar : (Char -> Bool) -> String -> Bool
allChar f s =
    String.length (String.filter f s) == String.length s


hasUpper : String -> Bool
hasUpper = hasChar Char.isUpper


hasLower : String -> Bool
hasLower = hasChar Char.isLower


hasDigit : String -> Bool
hasDigit = hasChar Char.isDigit


hasUpperLowerDigit : String -> Bool
hasUpperLowerDigit s =
    hasUpper s && hasLower s && hasDigit s
    
