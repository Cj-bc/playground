module Entry exposing (Entry, entryEncoder)

import Json.Encode as E


type alias Entry =
    { id : Int
    , title : String
    , detail : String
    , done : Bool
    }


entryEncoder : Entry -> E.Value
entryEncoder e =
    E.object
        [ ( "id", E.int e.id )
        , ( "title", E.string e.title )
        , ( "detail", E.string e.detail )
        , ( "done", E.bool e.done )
        ]
