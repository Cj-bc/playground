import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random
import String


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
    dieFaceSvg model.dieFace
    , button [ onClick Roll ] [ Html.text "Re throw die!" ]
    ]


dieFaceSvg : Int -> Html Msg
dieFaceSvg die =
  case die of
    1 ->
      svg [ width "120"
          , height "120"
          , viewBox "0 0 120 120"
          ]
          [ rect
            [ x "0"
            , y "0"
            , width "120"
            , height "120"
            , stroke "black"
            , fill "none"
            ] []
          , circle
            [ cx "60"
            , cy "60"
            , r "10"
            ] []
          ]
    2 ->
      svg [ width "120"
          , height "120"
          , viewBox "0 0 120 120"
          ]
          [ rect
          [ x "0"
          , y "0"
          , height "120"
          , width "120"
          , stroke "black"
          , fill "none"
          ] []
          , circle
            [ cx "100"
            , cy "20"
            , r "10"
            ] []
          , circle
            [ cx "20"
            , cy "100"
            , r "10"
            ] []
          ]
    3 ->
      svg [ width "120"
          , height "120"
          , viewBox "0 0 120 120"
          ]
          [ rect
            [ x "0"
            , y "0"
            , height "120"
            , width "120"
            , stroke "black"
            , fill "none"
            ] []
          , circle
            [ cy "100"
            , cx "20"
            , r "10"
            ] []
          , circle
            [ cy "20"
            , cx "100"
            , r "10"
            ] []
          , circle
            [ cy "60"
            , cx "60"
            , r "10"
            ] []
          ]
    4 ->
      svg [ width "120"
          , height "120"
          , viewBox "0 0 120 120"
          ]
          [ rect
            [ x "0"
            , y "0"
            , height "120"
            , width "120"
            , stroke "black"
            , fill "none"
            ] []
          , circle
            [ cx "20"
            , cy "20"
            , r "10"
            ] []
          , circle
            [ cx "100"
            , cy "20"
            , r "10"
            ] []
          , circle
            [ cx "20"
            , cy "100"
            , r "10"
            ] []
          , circle
            [ cx "100"
            , cy "100"
            , r "10"
            ] []
          ]
    5 ->
      svg [ width "120"
          , height "120"
          , viewBox "0 0 120 120"
          ]
          [ rect
            [ x "0"
            , y "0"
            , height "120"
            , width "120"
            , stroke "black"
            , fill "none"
            ] []
          , circle
            [ cx "20"
            , cy "20"
            , r "10"
            ] []
          , circle
            [ cx "100"
            , cy "20"
            , r "10"
            ] []
          , circle
            [ cx "20"
            , cy "100"
            , r "10"
            ] []
          , circle
            [ cx "100"
            , cy "100"
            , r "10"
            ] []
          , circle
            [ cx "60"
            , cy "60"
            , r "10"
            ] []
          ]
    6 ->
      svg [ width "120"
          , height "120"
          , viewBox "0 0 120 120"
          ]
          [ rect
            [ x "0"
            , y "0"
            , height "120"
            , width "120"
            , stroke "black"
            , fill "none"
            ] []
          , circle
            [ cx "20"
            , cy "20"
            , r "10"
            ] []
          , circle
            [ cx "100"
            , cy "20"
            , r "10"
            ] []
          , circle
            [ cx "20"
            , cy "100"
            , r "10"
            ] []
          , circle
            [ cx "100"
            , cy "100"
            , r "10"
            ] []
          , circle
            [ cx "100"
            , cy "60"
            , r "10"
            ] []
          , circle
            [ cx "20"
            , cy "60"
            , r "10"
            ] []
          ]
    _ -> h1 [] [Html.text (String.fromInt die)]
