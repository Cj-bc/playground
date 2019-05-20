import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN

main =
  Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

-- MODEL

type Model
  = Loading
  | Failure
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading , getRandomCatGif)

-- UPDATE

type Msg
  = MorePlease
  | GotGif (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getRandomCatGif)
    GotGif result ->
      case result of
        Ok gifURL ->
          (Success gifURL, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  case model of
    Loading ->
      text "Loading"
    Failure ->
      div []
      [ text "Unable to get cat gif..."
      , button [ onClick MorePlease ] [ text "MorePlease" ]
      ]
    Success url ->
      div []
      [ button [ onClick MorePlease ] [ text "MorePlease" ]
      , img [ src url ] []
      ]


-- HTTP

getRandomCatGif : Cmd Msg
getRandomCatGif =
  Http.get
  { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
  , expect = Http.expectJson GotGif gifDecorder }


gifDecorder : Decoder String
gifDecorder =
  field "data" (field "image_url" string)
