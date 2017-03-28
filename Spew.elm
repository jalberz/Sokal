module Spew exposing (..)

import HTTP
import List
import Html exposing (..)

-- MODEL

type alias Model =
    { urls : List String
    }

--INIT

init : (Model, Cmd.msg)
init = (Model [], Cmd.none)


-- UPDATE

type Msg = Suck | Spew

update : Msg -> Model -> (Model, Cmd.Msg)
update msg model =
  case msg of
    Suck -> (model, msg)
    Spew -> (model, msg)


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [text model.topic]
        , img [src model.gifUrl] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        ]


{-
executeSuck opens a file, takes the urls from it and siphons the text, and 
subsequently removes HTML tags from it. It calls upon makeModel to make the
text into a frequency sorted mapping that is written to sokal.model
-}