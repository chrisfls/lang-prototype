module Infer.Model.Bindings exposing (Bindings, empty, get, insert, remove)

import Dict exposing (Dict)
import IR.Spec exposing (Spec)



-- TODO: start using IntDict


type alias Bindings =
    Dict String Spec


empty : Bindings
empty =
    Dict.empty


insert : String -> Spec -> Bindings -> Bindings
insert =
    Dict.insert


remove : String -> Bindings -> Bindings
remove =
    Dict.remove


get : String -> Bindings -> Maybe Spec
get =
    Dict.get
