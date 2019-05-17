import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (length)


-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view}


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }

init : Model
init = Model "" "" ""


-- UPDATE

type Msg
  = Name String
  | Password String
  | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewInput"text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-entetr Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if length model.password < 8 then
    viewError "Passwords should be more than 8 characters"
  else if model.password /= model.passwordAgain then
    viewError "Passwords do not match!"
  else
    div [ style "color" "green" ] [ text "OK" ]


viewError : String -> Html msg
viewError error_str =
  div [ style "color" "red" ] [text error_str ]

