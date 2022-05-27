module Lang.Infer.Env exposing (..)

-- TODO: review

import Basics.Extra exposing (flip)
import Lang.Infer.NameDict as NameDict exposing (NameDict)
import Lang.Canonical.Name exposing (Name)
import Lang.Canonical.Type.Internal as Type exposing (Type(..))
import Lang.Infer.Error.Internal as Error exposing (Error)
import Lang.Infer.State as State exposing (State)
import Lang.Infer.Subst as Substitution
import Set exposing (Set)


type Env
    = TypeEnv (NameDict Scheme)


type Scheme
    = Scheme (List Int) Type


freshTVar : State error Type Int
freshTVar state =
    ( Ok (TVar state), state + 1 )


empty : Env
empty =
    TypeEnv NameDict.empty


variable : Name -> Env -> State Error Type Int
variable name (TypeEnv env) =
    case NameDict.get name env of
        Just t ->
            State.empty t
                |> State.andThen instantiate

        Nothing ->
            State.error (Error.NotFound name)


instantiate : Scheme -> State error Type Int
instantiate (Scheme vars t) =
    vars
        |> List.map instantiateHelp1
        |> State.sequence
        |> State.map (\a -> Substitution.substitute (Substitution.fromList a) t)


instantiateHelp1 : comparable -> State error ( comparable, Type ) Int
instantiateHelp1 =
    Tuple.pair >> flip State.map freshTVar


extend : Name -> Type -> Env -> Env
extend name t (TypeEnv env) =
    TypeEnv (NameDict.insert name (Scheme [] t) env)


generalize : Type -> Env -> Scheme
generalize t (TypeEnv env) =
    NameDict.values env
        |> List.map freeVariables
        |> List.foldl Set.union Set.empty
        |> Set.diff (Type.variables t)
        |> Set.toList
        |> (\generic -> Scheme generic t)


freeVariables : Scheme -> Set Int
freeVariables (Scheme generic t) =
    Set.diff (Type.variables t) (Set.fromList generic)
