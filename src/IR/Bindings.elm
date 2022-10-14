module IR.Bindings exposing (Bindings, empty, get, insert, remove)

import Dict
import IR.Spec as Spec exposing (Spec)



-- TODO: start using IntDict


type alias Bindings =
    Spec.Bindings


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
