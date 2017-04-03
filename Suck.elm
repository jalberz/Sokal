module Suck exposing (..)

import HTTP
import Debug
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
    Click -> (model, getURLs)

    Suck (Ok text) -> (model, harvest text)

    Suck (Err _) -> Debug.crash "Error: HTTP get request" 
    Spew -> (model, msg)

getURLs : Cmd Msg
getURLs =
  let
    src =
      "http://cmsc-22311.cs.uchicago.edu/2015/Labs/Lab-01/urls.txt"

    text =
      Http.getString src

  in
    Http.send Suck text

decodeUrl : Decode.Decoder String
decodeUrl =
  Decode.at ["data", "url"] Decode.string

harvest : String -> String
harvest text =
    let
        urls = lines text
        requests = map Http.getString urls
    in
        map Http.send requests
--TODO: mapping an http request across many requests


{-
//change haskell function to one that converts html to text
harvest ::  String -> String
harvest s =
    let
        tags = parseTags s
        paragraphs = map clean $ partitions (~== TagOpen "p" [])
            $ takeWhile (~/= TagOpen "div" [("id","back")])
            $ dropWhile (~/= TagOpen "div" [("id","body")]) tags
    in
        unlines paragraphs where
            clean :: [Tag String] -> String
            clean = unwords
                . words
                . filter isAscii
                . innerText
                . filter isTagText
-}

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [text model.topic]
        , button [ onClick Suck ] [ text "Harvest" ]
        ]


{-
executeSuck opens a file, takes the urls from it and siphons the text, and 
subsequently removes HTML tags from it. It calls upon makeModel to make the
text into a frequency sorted mapping that is written to sokal.model
-}