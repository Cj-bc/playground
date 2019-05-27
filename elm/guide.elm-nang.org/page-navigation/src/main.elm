import Browser
import Browser.Navigation as Nav
import Html exposing (Html,button, text, li, a, ul, b)
import Html.Attributes exposing (href)
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

type alias Model =
  { key : Nav.Key
  , url : Url.Url}


init : flags -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  (Model key url
  ,Cmd.none)


-- UPDATE

type Msg = UrlChanged Url.Url
         | LinkClicked Browser.UrlRequest


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UrlChanged url ->
      ({model | url = url}
      ,Cmd.none
      )

    LinkClicked request ->
      case request of
        Browser.Internal url ->
          (model, Nav.pushUrl model.key (Url.toString url))
        Browser.External href ->
          (model, Nav.load href)


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


view : Model -> Browser.Document Msg
view model =
  { title = "Url Interceptor"
  , body =
      [ text "The current URL is: "
      , b [] [ text (Url.toString model.url)]
      , ul []
          [ viewLink "/home"
          , viewLink "/profile"
          ]
      ]
  }

viewLink : String -> Html msg
viewLink path =
  li [] [ a [href path] [text path]]

