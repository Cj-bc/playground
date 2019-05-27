port module Main exposing (..)

import Browser
import Html exposing (Html, div, text, textarea, br, button)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick)
import Json.Encode as E

port cache : E.Value -> Cmd msg

-- MAIN

main = Browser.element
        {init =init
        ,view = view
        ,update = update
        , subscriptions = subscriptions
        }


-- MODEL

type alias Model = {text : String}

init : String -> (Model, Cmd Msg)
init text =
  (Model text
  ,Cmd.none)


-- UPDATE

type Msg
  = WriteNewText String
  | CacheData

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WriteNewText text ->
      (Model text, Cmd.none)
    CacheData ->
      (model, cache (E.string model.text))


-- VIEW

view : Model -> Html Msg
view model =
  div [] [text model.text
         ,br [] []
         ,textarea [onInput WriteNewText] []
         ,br[] []
         ,button [onClick CacheData] [text "Cache it"]
         ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
