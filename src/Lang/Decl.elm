module Lang.Decl exposing (..)

import Lang.Expr exposing (Expr)
-- TODO: Imp | Def | Rec | Export


type Decl
    = Let String Expr Expr

