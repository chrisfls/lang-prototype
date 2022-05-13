module Lang.Context exposing (..)

import Dict exposing (Dict)
import Lang.Type as Type exposing (Type(..))
import Lang.Scheme as Scheme exposing (Scheme(..))
import Set 

type Context
    = Context InternalContext

type alias InternalContext = Dict String Scheme

unwrap : Context -> InternalContext
unwrap (Context context) =
    context

empty : Context
empty =
    Context Dict.empty

insert : String -> Scheme -> Context -> Context
insert name scheme =
    unwrap
        >> Dict.insert name scheme
        >> Context

generalize : Type -> Context -> Scheme
generalize t =
    unwrap
        >> Dict.values
        >> List.map Scheme.freeVariables
        >> List.foldl Set.union Set.empty
        >> Set.diff (Type.variables t) 
        >> Set.toList
        >> (\g -> Scheme.create g t)
