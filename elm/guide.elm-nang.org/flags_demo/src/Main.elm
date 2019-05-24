import Browser
import Html exposing (Html, div, text)
import String

-- MAIN
main = Browser.element
        {init = init
        ,view = view
        ,update = update
        ,subscriptions = subscriptions}


-- MODEL

type alias Model = {time : Int}

init : Int -> (Model, Cmd Msg)
init time
  = (Model time
    ,Cmd.none)


-- UPDATE

type Msg
  = GotTime


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model
  ,Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  div [] [text (String.fromInt model.time)]


subscriptions : Model -> Sub Msg
subscriptions _
  = Sub.none
