import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time

-- MAIN
main =
  Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }


-- MODEL

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , pause : Bool
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) False
  , Task.perform AdjustTimeZone Time.here)

-- UPDATE

type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | TogglePause

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | time = newTime }
      , Cmd.none)
    AdjustTimeZone timeZone ->
      ({ model | zone = timeZone }
      , Cmd.none)
    TogglePause ->
      ({model | pause = if model.pause == True then False else True}
      , Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  if model.pause then
    Sub.none
  else
    Time.every 1000 Tick


-- VIEW
view : Model -> Html Msg
view model =
    let hour = String.fromInt (Time.toHour model.zone model.time)
        minute = String.fromInt (Time.toMinute model.zone model.time)
        second = String.fromInt (Time.toSecond model.zone model.time)
    in div [] [text (hour ++ " : " ++ minute ++ " : " ++ second)
              ,button [ onClick TogglePause ][ text "pause"]]

