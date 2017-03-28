module Suck exposing (..)

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


{-
executeSuck opens a file, takes the urls from it and siphons the text, and 
subsequently removes HTML tags from it. It calls upon makeModel to make the
text into a frequency sorted mapping that is written to sokal.model
-}

executeSuck : 