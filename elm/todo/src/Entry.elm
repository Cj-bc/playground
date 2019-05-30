module Entry exposing (Entry)


type alias Entry =
    { id : Int
    , title : String
    , detail : String
    , done : Bool
    }
