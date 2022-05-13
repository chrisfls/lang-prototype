module Lang.Scheme exposing (..)

import Lang.Type as Type  exposing (Type(..))
import Set exposing (Set)

{-| A scheme represents a type that is safe to instantiate
-}

type Scheme
    = Scheme (List Int) Type


fromType : Type -> Scheme
fromType =
    Scheme []


create : (List Int) -> Type -> Scheme
create =
    Scheme

freeVariables : Scheme -> Set Int
freeVariables (Scheme generic t) =
    Set.diff (Type.variables t) (Set.fromList generic)
