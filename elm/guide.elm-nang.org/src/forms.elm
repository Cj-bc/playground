import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (length, any, all)
import Char exposing (isUpper, isLower, isDigit)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view}


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  }

init : Model
init = Model "" "" "" ""


-- UPDATE

type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewInput"text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-entetr Password" model.passwordAgain PasswordAgain
    , viewInput "text" "Age" model.age Age
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []



viewValidation : Model -> Html msg
viewValidation model =
   if length model.password < 8 then
    viewError "Passwords should be more than 8 characters"

  else if not (any isUpper model.password) then
    viewError "Password should contain upper case, lower case, and numeric characters."

  else if not (any isLower model.password) then
    viewError "Password should contain upper case, lower case, and numeric characters."

  else if not (any isDigit model.password) then
    viewError "Password should contain upper case, lower case, and numeric characters."

  else if model.password /= model.passwordAgain then
    viewError "Passwords do not match!"

  else if not (all isDigit model.age) then
    viewError "Age should be number"
  else
    div [ style "color" "green" ] [ text "OK" ]


viewError : String -> Html msg
viewError error_str =
  div [ style "color" "red" ] [text error_str ]

isContainsNum : String -> Bool
isContainsNum str = any isDigit str
