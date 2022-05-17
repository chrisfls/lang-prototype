module Lang.Constraint exposing (..)

import Lang.Syntax.Type exposing (Type)
import Lang.Scheme exposing (Environment)
import State.Result exposing (StateResult)

type alias Constraint
    = ( Type, Type )

type alias Unifier = ( Type, List Constraint )