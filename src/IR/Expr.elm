module IR.Expr exposing (..)

import IR.Spec as Spec exposing (Spec)



-- TODO: perhaps decorate every tag with a spec


type Expr
    = Variable String
    | Lambda Bool Bool String Expr
    | Apply Expr Expr
    | Unborrow String Expr
    | Annotation Spec Expr


toString : Expr -> String
toString expr =
    case expr of
        Variable name ->
            name

        Lambda True True name body ->
            "(*" ++ name ++ " => " ++ toString body ++ ")"

        Lambda True False name body ->
            "(" ++ name ++ " => " ++ toString body ++ ")"

        Lambda False True name body ->
            -- invalid case but gotta cover it
            "(*" ++ name ++ " -> " ++ toString body ++ ")"

        Lambda False _ name body ->
            "(" ++ name ++ " -> " ++ toString body ++ ")"

        Apply function argument ->
            "(" ++ toString function ++ " " ++ toString argument ++ ")"

        Unborrow name body ->
            "unborrow " ++ name ++ " in " ++ toString body

        Annotation spec expr_ ->
            "<" ++ Spec.toString spec ++ ">" ++ toString expr_
