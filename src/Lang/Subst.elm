module Lang.Subst exposing (..)

import Dict exposing (Dict)
import Lang.Canonical.Type.Internal as Type exposing (Type(..))
import Lang.Error.Internal as Error exposing (Error)
import Set


type Subst
    = Subst (Dict Int Type)


empty : Subst
empty =
    Subst Dict.empty


fromList : List ( Int, Type ) -> Subst
fromList =
    Dict.fromList >> Subst


insert : Int -> Type -> Subst -> Subst
insert a b (Subst c) =
    Subst (Dict.insert a b c)


substitute : Subst -> Type -> Type
substitute substitution t =
    case t of
        TVar x ->
            let
                (Subst s) =
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

        TRecord fields ->
            TRecord <| Dict.map (always <| substitute substitution) fields


app : Subst -> Subst -> Subst
app a (Subst b) =
    let
        (Subst a_) =
            a
    in
    Subst (Dict.union (Dict.map (always <| substitute a) b) a_)


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

        TRecord d ->
            Dict.toList d
                |> List.map (\( n, t_ ) -> n ++ " : " ++ toString t_)
                |> String.join ", "
                |> (\x -> "{" ++ x ++ "}")


brace : String -> String
brace x =
    "(" ++ x ++ ")"


unify : Type -> Type -> Result Error Subst
unify context content =
    case ( context, content ) of
        ( TCon a at, TCon b bt ) ->
            if a == b then
                unifyMany at bt

            else
                Err <| Error.Mismatch context content

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
            Err <| Error.Mismatch x y


unifyMany : List Type -> List Type -> Result Error Subst
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


bind : Int -> Type -> Result Error Subst
bind id x =
    if x == TVar id then
        Ok empty

    else if Set.member id (Type.variables x) then
        Err (Error.Recursion id x)

    else
        Ok <| Subst <| Dict.singleton id x
