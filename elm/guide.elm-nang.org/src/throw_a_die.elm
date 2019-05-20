import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random


-- MAIN

main =
  Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }


-- Model

type alias Model =
  { dieFace : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 1, Cmd.none)


-- UPDATE

type Msg
  = Roll
  | NewFace Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace (Random.int 1 6)
      )
    NewFace newface ->
      ( Model newface
      , Cmd.none
      )


-- SUBSCRIBES

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
      h1 [] [text (String.fromInt model.dieFace)],
      button [ onClick Roll ] [ text "Re throw die!" ]
    ]

