module Main exposing (..)
import Browser
import Http
import Html.Attributes as Attr
import Html.Events as AttrEvent

import Html exposing (Html, div, text, ul, li, input, form)
import Json.Decode as D
import Json.Encode as E

backendUrlBase="http://localhost:3000"

type alias Todo =
    { id : Int
    , title : String
    , isDone : Bool
    }

type alias Model =
    {todoes : List Todo
    , newTodoField : String
    , error : Maybe Http.Error
    }

main = Browser.element
       { init = init
       , update = update
       , view = view
       , subscriptions = always Sub.none
       }
               
type Msg = CallListTodoes
         | CallUpdateTodo Todo
         | CallCreateTodo String
         | CallDeleteTodo Int
         | ToggleDone Todo Bool
         | UpdateNewTodo String
         | GotListTodoes (Result Http.Error (List Todo))
         | GotUpdateTodo (Result Http.Error ())
         | GotCreateTodo (Result Http.Error ())
         | GotDeleteTodo (Result Http.Error ())

init : () -> (Model, Cmd Msg)
init _ = (Model [] "" Nothing, listTodoes)
       
todoListDecoder : D.Decoder (List Todo)
todoListDecoder =
    D.field "Todoes" (
                      D.list (D.map3 Todo
                                  (D.field "id"     D.int)
                                  (D.field "title"  D.string)
                                  (D.field "isDone" D.bool)))

todoEncode : Todo -> E.Value
todoEncode todo =
    E.object
        [ ("id", E.int todo.id)
        , ("title", E.string todo.title)
        , ("isDone", E.bool todo.isDone)
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CallListTodoes -> (model, listTodoes)
        CallUpdateTodo todo -> (model, patchTodo todo)
        CallCreateTodo title -> (model, createNewTodo title)
        CallDeleteTodo id -> (model, deleteTodo id)
        ToggleDone todo isDoneState -> (model, patchTodo { todo | isDone = isDoneState })
        UpdateNewTodo title -> ({model | newTodoField = title}, Cmd.none)
        GotListTodoes ts -> case ts of
                                Ok todoes -> ({model | todoes = todoes, error = Nothing }, Cmd.none) 
                                Err err -> ({ model | error = Just err }, Cmd.none)
        GotUpdateTodo res -> case res of
                                 Ok _ -> (model, listTodoes)
                                 Err err -> ({model | error = Just err }, Cmd.none)
        GotCreateTodo res -> case res of
                                 Ok _ -> ({model | newTodoField = "" }, listTodoes)
                                 Err err -> ({model | error = Just err }, Cmd.none)
        GotDeleteTodo res -> case res of
                                 Ok _ -> (model, listTodoes)
                                 Err err -> ({model | error = Just err }, Cmd.none)

-- Commands
patchTodo : Todo -> Cmd Msg
patchTodo todo = Http.request
                 { method = "PATCH"
                 , headers = []
                 , url = backendUrlBase++"/todo/"++String.fromInt(todo.id)
                 , body = Http.jsonBody (todoEncode todo)
                 , expect = Http.expectWhatever GotUpdateTodo
                 , timeout = Nothing
                 , tracker = Nothing
                 }

listTodoes : Cmd Msg
listTodoes = Http.get { url = backendUrlBase++"/todoes"
                      , expect = Http.expectJson GotListTodoes todoListDecoder
                      }

createNewTodo : String -> Cmd Msg
createNewTodo title = Http.post { url = backendUrlBase++"/todo"
                                , body = Http.jsonBody (E.object [("title", E.string title)])
                                , expect = Http.expectWhatever GotCreateTodo
                                }
deleteTodo : Int -> Cmd Msg
deleteTodo id = Http.request
                { method = "DELETE"
                , headers = []
                , url = backendUrlBase++"/todo/"++String.fromInt(id)
                , body = Http.emptyBody
                , expect = Http.expectWhatever GotDeleteTodo
                , timeout = Nothing
                , tracker = Nothing
                }

-- View
view : Model -> Html Msg
view model =
    div []  [text "Todo list:"
            , ul [Attr.class "todo-list"] (List.map todoView model.todoes)
            , errorToast model.error
            , newTodoView model
            ]

todoView : Todo -> Html Msg
todoView todo =
    let doneBtn = input [Attr.type_ "checkbox"
                        , Attr.checked todo.isDone
                        , AttrEvent.onCheck (ToggleDone todo)] []
        deleteBtn = input [Attr.type_ "button"
                          , AttrEvent.onClick (CallDeleteTodo todo.id)
                          , Attr.value "削除"] []
    in li [Attr.class "todo-item"] [doneBtn, text todo.title, deleteBtn]

newTodoView : Model -> Html Msg
newTodoView model =
    form [AttrEvent.onSubmit (CallCreateTodo model.newTodoField)]
        [input [Attr.type_ "input"
               , AttrEvent.onInput UpdateNewTodo
               , Attr.value model.newTodoField
               ] []]

errorToast : Maybe Http.Error -> Html msg
errorToast err = case err of
                     Nothing -> div [] []
                     Just (Http.BadUrl str) -> div [] [text str]
                     Just Http.Timeout -> div [] [text "Request timeouted"]
                     Just Http.NetworkError -> div [] [text "Something went wrong with network"]
                     Just (Http.BadStatus status) -> div [] [text ("bad status code: "++String.fromInt(status))]
                     Just (Http.BadBody status) -> div [] [text ("bad body: "++status)]

