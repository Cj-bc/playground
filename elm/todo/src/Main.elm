port module Main exposing (Model, Msg(..), emptyModel, init, main, subscriptions, update, view, viewEntry)

import Browser
import Entry exposing (Entry, entryEncoder)
import Html exposing (Html, button, div, h1, input, p, text, textarea)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as E
import List


main : Program (Maybe (List Entry)) Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port storeTodo : E.Value -> Cmd msg



-- Model


type alias Model =
    { entries : List Entry
    , title_field : String
    , detail_field : String
    , uid : Int
    }


emptyModel : Model
emptyModel =
    Model [] "" "" 0


init : Maybe (List Entry) -> ( Model, Cmd Msg )
init d =
    let
        last list =
            List.head (List.reverse list)

        n_uid entries =
            case last entries of
                Nothing ->
                    0

                Just e ->
                    e.id + 1
    in
    case d of
        Nothing ->
            ( emptyModel, Cmd.none )

        Just entries ->
            ( Model entries "" "" (n_uid entries), Cmd.none )



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
            let
                new_entry =
                    Entry.Entry model.uid model.title_field model.detail_field False

                new_model =
                    { model
                        | entries = model.entries ++ [ new_entry ]
                        , uid = model.uid + 1
                        , title_field = ""
                        , detail_field = ""
                    }
            in
            ( new_model
            , storeTodo (E.list entryEncoder new_model.entries)
            )

        MakeDone id ->
            let
                new_model =
                    { model
                        | entries =
                            List.filter (\e -> e.id /= id) model.entries
                                ++ List.map (\e -> Entry e.id e.title e.detail True) (List.filter (\e -> e.id == id) model.entries)
                    }
            in
            ( new_model
            , storeTodo (E.list entryEncoder new_model.entries)
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
            let
                new_model =
                    { model | entries = List.filter (\e -> e.id /= id) model.entries }
            in
            ( new_model
            , storeTodo (E.list entryEncoder new_model.entries)
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
        , input [ placeholder "title", onInput EditTitle, value model.title_field ] []
        , textarea [ onInput EditDetail, placeholder "details", value model.detail_field ] []
        , button [ onClick Add ] [ text "Add todo" ]
        , p []
            [ text "Left todoes:"
            , div [] (List.map viewEntry (List.filter (\e -> not e.done) model.entries))
            ]
        , p []
            [ text "Done todoes:"
            , div [] (List.map viewEntry (List.filter (\e -> e.done) model.entries))
            ]
        ]
    }


viewEntry : Entry -> Html Msg
viewEntry e =
    let
        title =
            "title: " ++ e.title ++ "\n"

        detail =
            "detail: " ++ e.detail ++ "\n"
    in
    p []
        [ text title
        , text detail
        , if e.done then
            text "done"

          else
            button [ onClick (MakeDone e.id) ] [ text "Mark as done" ]
        , button [ onClick (Remove e.id) ] [ text "remove this" ]
        ]
