module Lang2.Syntax.Type exposing
    ( Type(..)
    , string, char, bool, int, float
    , toString
    , Substitution, substitute
    , unify, union
    , variables
    , app
    )

{-|


#

@docs Type


# Constructors for common primitive types

@docs string, char, bool, int, float

@docs toString

@docs Substitution, substitute, ($)

@docs unify, union

@docs variables

-}

import Dict exposing (Dict)
import Set exposing (Set)


{-| Represents Elm types. TAny is for type variables.
-}
type Type
    = TArr Type Type
    | TCon String (List Type)
    | TVar Int
    | TRecord (Dict String Type)
    | TTuple (List Type)



-- TODO:
-- * rework record
-- * rework tuple
-- * rework TCon


{-| String
-}
string : Type
string =
    TCon ".String" []


{-| Char
-}
char : Type
char =
    TCon ".Char" []


{-| Bool
-}
bool : Type
bool =
    TCon ".Bool" []


{-| Int
-}
int : Type
int =
    TCon ".Int" []


{-| Float
-}
float : Type
float =
    TCon ".Float" []


{-| Textual representation of a type
-}
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

        TRecord d ->
            Dict.toList d
                |> List.map (\( n, t_ ) -> n ++ " : " ++ toString t_)
                |> String.join ", "
                |> (\x -> "{" ++ x ++ "}")

        TTuple types ->
            List.map toString types
                |> String.join ","
                |> brace


brace : String -> String
brace x =
    "(" ++ x ++ ")"


{-| Returns the (free) type variables of the type.
-}
variables : Type -> Set Int
variables t =
    case t of
        TVar x ->
            Set.singleton x

        TArr l r ->
            Set.union (variables l) (variables r)

        TRecord d ->
            Dict.values d
                |> variablesFromList

        TTuple types ->
            variablesFromList types

        TCon _ args ->
            variablesFromList args


variablesFromList : List Type -> Set Int
variablesFromList =
    List.map variables
        >> List.foldl Set.union Set.empty


{-| Returns the substitutions necessary to transform either type
into their lowest common denominator.
Returns an error if not possible.
-}
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
            (Ok Dict.empty)


bind : Int -> Type -> Result String (Dict Int Type)
bind id x =
    if x == TVar id then
        Ok Dict.empty

    else if Set.member id (variables x) then
        Err ("recursive type " ++ String.fromInt id ++ " " ++ toString x)

    else
        Ok <| Dict.singleton id x


mismatch : String -> String -> Result String a
mismatch a b =
    Err <| "Mismatch: " ++ a ++ " and " ++ b


{-| Applies one substitution to another
-}
app : Substitution -> Substitution -> Substitution
app a b =
    Dict.union (Dict.map (always <| substitute a) b) a


{-| Returns a type that conforms to both supplied types
-}
union : Type -> Type -> Result String Type
union a b =
    unify a b
        |> Result.map (\r -> substitute r a)


{-| Tells what values type variables get.
-}
type alias Substitution =
    Dict Int Type


{-| Swap out type variables according to substitution
-}
substitute : Substitution -> Type -> Type
substitute substitution t =
    case t of
        TVar x ->
            Dict.get x substitution
                |> Maybe.withDefault (TVar x)

        TArr h t_ ->
            TArr (substitute substitution h) (substitute substitution t_)

        TCon name types ->
            TCon name <| List.map (substitute substitution) types

        TTuple types ->
            TTuple <| List.map (substitute substitution) types

        TRecord fields ->
            TRecord <| Dict.map (always <| substitute substitution) fields
