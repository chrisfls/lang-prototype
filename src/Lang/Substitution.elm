module Lang.Substitution exposing (..)

import Dict exposing (Dict)
import Lang.Canonical.Type as Type exposing (Type(..))
import Set


type Substitution
    = Substitution (Dict Int Type)


empty =
    Substitution Dict.empty


fromList : List ( Int, Type ) -> Substitution
fromList =
    Dict.fromList >> Substitution


insert : Int -> Type -> Substitution -> Substitution
insert a b (Substitution c) =
    Substitution (Dict.insert a b c)


substitute : Substitution -> Type -> Type
substitute substitution t =
    case t of
        TVar x ->
            let
                (Substitution s) =
                    substitution
            in
            Dict.get x s
                |> Maybe.withDefault (TVar x)

        TArr h t_ ->
            TArr (substitute substitution h) (substitute substitution t_)

        TCon name types ->
            TCon name <| List.map (substitute substitution) types

        TTuple types ->
            TTuple <| List.map (substitute substitution) types


app : Substitution -> Substitution -> Substitution
app a (Substitution b) =
    let
        (Substitution a_) =
            a
    in
    Substitution (Dict.union (Dict.map (always <| substitute a) b) a_)


toString : Type -> String
toString t =
    case t of
        TCon name args ->
            name
                :: List.map toString args
                |> String.join " "
                |> brace

        TArr l r ->
            toString l ++ " -> " ++ toString r

        TVar x ->
            String.fromInt x

        TTuple types ->
            List.map toString types
                |> String.join ","
                |> brace


brace : String -> String
brace x =
    "(" ++ x ++ ")"


unify : Type -> Type -> Result String Substitution
unify context content =
    case ( context, content ) of
        ( TCon a at, TCon b bt ) ->
            if a == b then
                unifyMany at bt

            else
                mismatch a b

        ( TArr head1 tail1, TArr head2 tail2 ) ->
            unify head1 head2
                |> Result.andThen
                    (\sub1 ->
                        unify (substitute sub1 tail1) (substitute sub1 tail2)
                            |> Result.map (\sub2 -> app sub2 sub1)
                    )

        ( TVar id, x ) ->
            bind id x

        ( x, TVar id ) ->
            bind id x

        ( x, y ) ->
            mismatch (toString x) (toString y)


unifyMany : List Type -> List Type -> Result String Substitution
unifyMany context content =
    List.map2 Tuple.pair context content
        |> List.foldl
            (\( a, b ) ->
                Result.andThen
                    (\s ->
                        unify (substitute s a) (substitute s b)
                            |> Result.map (\res -> app res s)
                    )
            )
            (Ok empty)


bind : Int -> Type -> Result String Substitution
bind id x =
    if x == TVar id then
        Ok empty

    else if Set.member id (Type.variables x) then
        Err ("recursive type " ++ String.fromInt id ++ " " ++ toString x)

    else
        Ok <| Substitution <| Dict.singleton id x


mismatch : String -> String -> Result String a
mismatch a b =
    Err <| "Mismatch: " ++ a ++ " and " ++ b
