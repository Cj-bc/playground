import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time
import Svg exposing (line,circle,svg)
import Svg.Attributes exposing (cy, cx, r
                               ,x1, y1, x2, y2
                               ,x, y, width, height
                               , fill, stroke, strokeWidth
                               )

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
    in div [] [svgClock model
              ,text (hour ++ " : " ++ minute ++ " : " ++ second)
              ,button [ onClick TogglePause ][ text "pause"]]



type alias Coordinate = {x : Float
                        ,y : Float
                        }

type ClockHand = HourHand
               | MinuteHand
               | SecondHand

svgClock : Model -> Html Msg
svgClock model =
  let hourHandAngle   = calcHandAngle HourHand model
      minuteHandAngle = calcHandAngle MinuteHand model
      secondHandAngle = calcHandAngle SecondHand model
      hourHand_ext    = Coordinate (40 * (cos hourHandAngle)) (-1 * 40 * (sin hourHandAngle))
      minuteHand_ext  = Coordinate (80 * (cos minuteHandAngle)) (-1 * 80 * (sin minuteHandAngle))
      secondHand_ext  = Coordinate (70 * (cos secondHandAngle)) (-1 * 70 * (sin secondHandAngle))
      clockBase   = Coordinate 100 100
      clockFrame  = circle [ cx (String.fromFloat clockBase.x)
                           , cy (String.fromFloat clockBase.y)
                           , r "100"
                           , fill "none"
                           , stroke "black"] []
      hourHand    = line [ x1 (String.fromFloat clockBase.x)
                         , y1 (String.fromFloat clockBase.y)
                         , x2 (String.fromFloat (clockBase.x + hourHand_ext.x))
                         , y2 (String.fromFloat (clockBase.y + hourHand_ext.y))
                         , stroke "red"
                         , strokeWidth "5"] []
      minuteHand  = line [ x1 (String.fromFloat clockBase.x)
                         , y1 (String.fromFloat clockBase.y)
                         , x2 (String.fromFloat (clockBase.x + minuteHand_ext.x))
                         , y2 (String.fromFloat (clockBase.y + minuteHand_ext.y))
                         , stroke "black"
                         , strokeWidth "3"] []
      secondHand  = line [ x1 (String.fromFloat clockBase.x)
                         , y1 (String.fromFloat clockBase.y)
                         , x2 (String.fromFloat (clockBase.x + secondHand_ext.x))
                         , y2 (String.fromFloat (clockBase.y + secondHand_ext.y))
                         , stroke "black"
                         , strokeWidth "1"] []
  in svg  [x "0"
          ,y "0"
          ,width "200"
          ,height "200"
          ]
          [ clockFrame
          , hourHand
          , minuteHand
          , secondHand
          ]



-- I'm using two different coordinate system.
-- 1. The same thing used in clock (Top is 0, increase as clock goes. 0~360)
-- 2. System used with trigonometric functions (Top is 90, Bottom is -90.-180~180)
-- From now, I call 1. as 'clock c.s.' and 2. as 'trig c.s.'
-- I should convert one to another to integrate with two systems.
--
-- First, I use 'clock c.s.' to imagine how clock looks like.
-- Then, I should convert it into 'trig c.s.'
--       to integrate with trigonometric functions(cos, sin)
--
-- How to convert clock c.s. into trig c.s.
-- ```haskell
--  convertClockCStoTrigCS : Float -> Float
--  convertClockCStoTrigCS clockcs  | clockcs <= 90         = 90 - clockcs
--                                  | 90 < clockcs <= 180   = -(clockcs - 90)     -- = 90 - clockcs
--                                  | 180 < clockcs <= 270  = -(clockcs - 90)     -- = 90 - clockcs
--                                  | 270 < clockcs         = (360 - clockcs) + 90
-- ```
calcHandAngle : ClockHand -> Model -> Float
calcHandAngle hand model =
  let hour   = toFloat (Time.toHour model.zone model.time)
      minute = toFloat (Time.toMinute model.zone model.time)
      second = toFloat (Time.toSecond model.zone model.time)
      clockAngle = if hand == HourHand
                   then (360 / 24) * hour
                   else if hand == MinuteHand
                      then (360 / 60) * minute
                      else (360 / 60) * second
  in
    if 270 < clockAngle
    then (360 - clockAngle) + 90
    else 90 - clockAngle
