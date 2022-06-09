module Lang.Canonical.Modl exposing (..)

import Lang.Canonical.Expr exposing (Expr)
import Lang.Canonical.Name exposing (ForName, ModName, QuaName, TypName, VarName)
import Lang.Canonical.Type exposing (Type)


type Modl
    = Mod Export Import Body
    | Arg ModName (Modl -> Modl)
    | Ann Type Modl


type alias Export =
    { default : Maybe EType
    , entries : Entries
    }


type alias Import =
    { from : QuaName
    , name : Maybe ModName
    , entries : Entries
    }


type Entries
    = All
    | Some (List Enum)


type Enum
    = EValue VarName
    | EType EType


type EType
    = EDef TypName
    | EDec TypName


type Body
    = Def TypName Forall Type Body
    | Dec TypName Forall Union Body
    | Let VarName Expr Body
    | End


type alias Forall =
    List ForName


type alias Union =
    List Tag


type Tag
    = Tag TypName Type
