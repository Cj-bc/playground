module Main exposing (..)
import Browser
import Http

import Html exposing (Html, div, text, ul, li)
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
               
type Msg = ListTodoes
         | CallUpdateTodo Todo
         | CallCreateTodo Todo
         | GotListTodoes (Result Http.Error (List Todo))
         | GotUpdateTodo (Result Http.Error ())
         | GotCreateTodo (Result Http.Error ())

init : () -> (Model, Cmd Msg)
init _ = (Model [Todo 1 "AAA" False] "" Nothing, listTodoes)
       
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
        ListTodoes -> (model, listTodoes)
        CallUpdateTodo todo -> (model, patchTodo todo)
        CallCreateTodo todo -> Debug.todo "it's in todo"
        GotListTodoes ts -> case ts of
                                Ok todoes -> ({model | todoes = todoes, error = Nothing }, Cmd.none) 
                                Err err -> ({ model | error = Just err }, Cmd.none)
        GotUpdateTodo res -> (model, listTodoes)
        GotCreateTodo _ -> Debug.todo "It's in todo"

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

view : Model -> Html Msg
view model =
    div []  [text "Todo list:"
            , ul [] (List.map todoView model.todoes)
            , errorToast model.error
            ]

todoView : Todo -> Html msg
todoView todo = li [] [text (String.fromInt todo.id), text todo.title]

errorToast : Maybe Http.Error -> Html msg
errorToast err = case err of
                     Nothing -> div [] []
                     Just (Http.BadUrl str) -> div [] [text str]
                     Just Http.Timeout -> div [] [text "Request timeouted"]
                     Just Http.NetworkError -> div [] [text "Something went wrong with network"]
                     Just (Http.BadStatus status) -> div [] [text ("bad status code: "++String.fromInt(status))]
                     Just (Http.BadBody status) -> div [] [text ("bad body: "++status)]

