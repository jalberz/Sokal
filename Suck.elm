module Suck exposing (..)

import HTTP
import Json.Decode
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

type Msg = Suck (Result Http.Error String) | Spew

update : Msg -> Model -> (Model, Cmd.Msg)
update msg model =
  case msg of
    Suck -> (model, getURLs "urls.txt")
    Spew -> (model, msg)

getURLs : String -> Cmd Msg
getURLs file =
  let
    urls =
      "http://cmsc-22311.cs.uchicago.edu/2015/Labs/Lab-01/urls.txt"

    request =
      Http.get url decodeUrl
  in
    Http.send Suck request

decodeUrl : Decode.Decoder String
decodeUrl =
  Decode.at ["data", "url"] Decode.string

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [text model.topic]
        , img [src model.gifUrl] []
        , button [ onClick MorePlease ] [ text "Harvest" ]
        ]


{-
executeSuck opens a file, takes the urls from it and siphons the text, and 
subsequently removes HTML tags from it. It calls upon makeModel to make the
text into a frequency sorted mapping that is written to sokal.model
-}