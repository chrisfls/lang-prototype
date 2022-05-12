module Main exposing (..)

import Browser
import Html exposing (text)
import Infer

main =
  Browser.sandbox { init = (), update = update, view = view }

update _ _ =
    ()

view _ =
    text "a"