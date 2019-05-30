module Main exposing (main)

import Browser
import Entry exposing (Entry)
import Html exposing (Html, h1, input, text, textarea)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import List


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { entries : List Entry.Entry
    , title_field : String
    , detail_field : String
    , uid : Int
    }


emptyModel : Model
emptyModel =
    Model [] "" "" 0


init : flag -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, Cmd.none )



-- Update


type Msg
    = NoOp
    | Add
    | MakeDone Int
    | EditTitle String
    | EditDetail String
    | Remove Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add ->
            ( { model
                | entries = model.entries ++ [ Entry.Entry model.uid model.title_field model.detail_field False ]
                , uid = model.uid + 1
                , title_field = ""
                , detail_field = ""
              }
            , Cmd.none
            )

        MakeDone id ->
            ( { model
                | entries =
                    List.filter (\e -> e.id /= id) model.entries
                        ++ List.map (\e -> Entry e.id e.title e.detail True) (List.filter (\e -> e.id == id) model.entries)
              }
            , Cmd.none
            )

        EditTitle title ->
            ( { model | title_field = title }
            , Cmd.none
            )

        EditDetail detail ->
            ( { model | detail_field = detail }
            , Cmd.none
            )

        Remove id ->
            ( { model | entries = List.filter (\e -> e.id /= id) model.entries }
            , Cmd.none
            )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "TODO"
    , body =
        [ h1 [] [ text "Todo List" ]
        , input [ placeholder "title", onInput EditTitle ] []
        , textarea [ onInput EditDetail ] []
        ]
    }
