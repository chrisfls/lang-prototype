module Lang.Canonical.Type.Env exposing (..)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Lang.Canonical.Expr exposing (Name)
import Lang.Canonical.Type.Internal as Type exposing (Type(..))
import Lang.Infer.Error.Internal as Error exposing (Error)
import Lang.Canonical.Type.Subst as Substitution
import Set exposing (Set)
import StateResult exposing (StateResult)


type TypeEnv
    = TypeEnv (Dict Name Scheme)


type Scheme
    = Scheme (List Int) Type


empty : TypeEnv
empty =
    TypeEnv Dict.empty


variable : Name -> TypeEnv -> StateResult Error Type Int
variable name (TypeEnv env) =
    case Dict.get name env of
        Just t ->
            StateResult.empty t
                |> StateResult.andThen instantiate

        Nothing ->
            StateResult.error (Error.NotFound name)


instantiate : Scheme -> StateResult error Type Int
instantiate (Scheme vars t) =
    vars
        |> List.map instantiateHelp1
        |> StateResult.sequence
        |> StateResult.map (\a -> Substitution.substitute (Substitution.fromList a) t)


instantiateHelp1 : comparable -> StateResult error ( comparable, Type ) Int
instantiateHelp1 =
    Tuple.pair >> flip StateResult.map Type.freshTVar


extend : Name -> Type -> TypeEnv -> TypeEnv
extend name t (TypeEnv env) =
    TypeEnv (Dict.insert name (Scheme [] t) env)


generalize : Type -> TypeEnv -> Scheme
generalize t (TypeEnv env) =
    Dict.values env
        |> List.map freeVariables
        |> List.foldl Set.union Set.empty
        |> Set.diff (Type.variables t)
        |> Set.toList
        |> (\generic -> Scheme generic t)


freeVariables : Scheme -> Set Int
freeVariables (Scheme generic t) =
    Set.diff (Type.variables t) (Set.fromList generic)
