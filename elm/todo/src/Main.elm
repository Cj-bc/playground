port module Main exposing (Model, Msg(..), emptyModel, init, main, subscriptions, update, view, viewEntry)

import Browser
import Entry exposing (Entry, entryEncoder)
import Html exposing (Html, b, button, details, div, h1, input, li, p, summary, text, textarea, ul)
import Html.Attributes exposing (class, placeholder, style, type_, value)
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
        , viewNewEntry model
        , viewActiveTodo model
        , viewDoneTodo model
        ]
    }


viewNewEntry : Model -> Html Msg
viewNewEntry model =
    div [ class "todo-new" ]
        [ input [ placeholder "title", onInput EditTitle, value model.title_field, class "title" ]
            []
        , textarea
            [ onInput EditDetail, placeholder "details", value model.detail_field, class "details" ]
            []
        , button
            [ onClick Add, class "submit" ]
            [ text "Add todo" ]
        ]


viewDoneTodo : Model -> Html Msg
viewDoneTodo model =
    div [ class "todo-done" ]
        [ text "Done todoes:"
        , ul [ class "entries-container" ] (List.map viewEntry (List.filter (\e -> e.done) model.entries))
        ]


viewActiveTodo : Model -> Html Msg
viewActiveTodo model =
    div [ class "todo-active" ]
        [ text "Active todoes:"
        , ul [ class "entries-container" ] (List.map viewEntry (List.filter (\e -> not e.done) model.entries))
        ]


viewEntry : Entry -> Html Msg
viewEntry e =
    div [ class "entry" ]
        [ div [ class "entry-title" ] [ b [] [ text ("title: " ++ e.title) ] ]
        , div [ class "entry-detail" ] [ text e.detail ]
        , if e.done then
            div [ class "entry-done-button" ] [ text "done" ]

          else
            button [ class "entry-done-button", onClick (MakeDone e.id) ] [ text "Mark as done" ]
        , button [ class "entry-remove-button", onClick (Remove e.id) ] [ text "remove this" ]
        ]
