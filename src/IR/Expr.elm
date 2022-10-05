module IR.Expr exposing (..)

import IR.Spec as Spec exposing (Spec)


-- TODO: perhaps decorate every tag with a spec

type Expr
    = Variable String
    | Lambda Linear String Expr
    | Apply Expr Expr
    | Free String Expr
    | Annotation Skip Spec Expr


type alias Linear =
    Maybe Bool


{-| Avoids double checking previously infered annotations.
-}
type alias Skip =
    Bool


toString : Expr -> String
toString expr =
    case expr of
        Variable name ->
            name

        Lambda (Just True) name body ->
            "(*" ++ name ++ " -> " ++ toString body ++ ")"

        Lambda _ name body ->
            "(" ++ name ++ " -> " ++ toString body ++ ")"

        Apply function argument ->
            "(" ++ toString function ++ " " ++ toString argument ++ ")"

        Free name body ->
            "free " ++ name ++ " in " ++ toString body

        Annotation _ spec (Lambda _ name body) ->
            -- WTF is this? I don't remember
            "(" ++ name ++ " [" ++ Spec.toString spec ++ "] -> " ++ toString body ++ ")"

        Annotation _ spec expr_ ->
            "<" ++ Spec.toString spec ++ ">" ++ toString expr_
