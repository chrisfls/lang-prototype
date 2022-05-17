module Lang2.Constraint exposing (..)

import Lang2.Syntax.Type exposing (Type)
import Lang2.Scheme exposing (Environment)
import State.Result exposing (StateResult)

type alias Constraint
    = ( Type, Type )

type alias Unifier = ( Type, List Constraint )