module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text)
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type Model
    = Model Int


init : flag -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "test title"
    , body = [ text "hello" ]
    }
