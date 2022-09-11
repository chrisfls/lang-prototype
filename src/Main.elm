module Main exposing (..)

import Browser
import Html exposing (text)



-- TODO: deal with import requests and report errors


main =
    Browser.sandbox { init = (), update = always (always ()), view = always (text "") }
