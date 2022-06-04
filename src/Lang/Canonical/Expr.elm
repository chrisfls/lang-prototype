module Lang.Canonical.Expr exposing (Expr(..), toString)

import Lang.Canonical.Type as Type exposing (Name, Type)


type Expr
    = Var Name
    | Lam Name (Expr -> Expr)
    | App Expr Expr
    | Ann Type Expr


toString : Expr -> String
toString exp =
    case exp of
        Var name ->
            name

        Lam name body ->
            "(" ++ name ++ " -> " ++ toString (body (Var name)) ++ ")"

        App func argm ->
            "(" ++ toString func ++ " " ++ toString argm ++ ")"

        Ann thisT (Lam name body) ->
            "(" ++ name ++ " [" ++ Type.toString thisT ++ "] -> " ++ toString (body (Var name)) ++ ")"

        Ann thisT exp_ ->
            "[" ++ Type.toString thisT ++ "]" ++ toString exp_
