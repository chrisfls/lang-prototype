module Lang.Canonical.Expr exposing (Expr(..), toString)

import Dict exposing (Dict)
import Lang.Canonical.Type as Type exposing (Type)


type Expr
    = Var String
    | Tup (List Expr)
    | Rec (Dict String Expr)
    | Upd Expr (Dict String Expr)
      -- I might have to replace HOAS with FOAS eventually
    | Lam String (Expr -> Expr)
    | App Expr Expr
    | Ann Type Expr


toString : Expr -> String
toString exp =
    case exp of
        Var name ->
            name

        Tup list ->
            "( " ++ String.join ", " (List.map toString list) ++ " )"

        Rec fields ->
            "{ "
                ++ (Dict.toList fields
                        |> List.map (\( name, exp_ ) -> name ++ " : " ++ toString exp_)
                        |> String.join ", "
                   )
                ++ " }"

        Upd name fields ->
            "{ " ++ toString name
                ++ " | "
                ++ (Dict.toList fields
                        |> List.map (\( name_, exp_ ) -> name_ ++ " : " ++ toString exp_)
                        |> String.join ", "
                   )
                ++ " }"

        Lam name body ->
            "(" ++ name ++ " -> " ++ toString (body (Var name)) ++ ")"

        App func argm ->
            "(" ++ toString func ++ " " ++ toString argm ++ ")"

        Ann t (Lam name body) ->
            "(" ++ name ++ " [" ++ Type.toString t ++ "] -> " ++ toString (body (Var name)) ++ ")"

        Ann t exp_ ->
            "<" ++ Type.toString t ++ ">" ++ toString exp_
