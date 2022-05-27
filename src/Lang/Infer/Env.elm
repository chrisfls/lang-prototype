module Lang.Infer.Env exposing (..)


import Dict exposing (Dict)
import Lang.Canonical.Name as Name exposing (Name)
import Lang.Canonical.Type.Internal as Type exposing (Type(..))
import Lang.Infer.Error.Internal as Error exposing (Error)
import Lang.Infer.State as State exposing (State)
import Lang.Infer.Subst as Substitution
import Set exposing (Set)


type Env
    = TypeEnv (Dict String Scheme)


type Scheme
    = Scheme (List Int) Type


freshTVar : State error Type Int
freshTVar count =
    ( Ok (TVar count), count + 1 )


empty : Env
empty =
    TypeEnv Dict.empty


variable : Name -> Env -> State Error Type Int
variable name (TypeEnv env) =
    case Dict.get (Name.toString name) env of
        Just t ->
            State.empty t
                |> State.andThen instantiate

        Nothing ->
            State.error (Error.NotFound name)


instantiate : Scheme -> State error Type Int
instantiate (Scheme vars t) =
    vars
        |> List.map instantiateHelp
        |> State.sequence
        |> State.map (\a -> Substitution.substitute (Substitution.fromList a) t)


instantiateHelp : Int -> State error ( Int, Type ) Int
instantiateHelp index =
    State.map (\count -> ( index, count )) freshTVar


extend : Name -> Type -> Env -> Env
extend name t (TypeEnv env) =
    TypeEnv (Dict.insert (Name.toString name) (Scheme [] t) env)


generalize : Type -> Env -> Scheme
generalize t (TypeEnv env) =
    let
        generic =
            Dict.values env
                |> List.map freeVariables
                |> List.foldl Set.union Set.empty
                |> Set.diff (Type.variables t)
                |> Set.toList
    in
    Scheme generic t


freeVariables : Scheme -> Set Int
freeVariables (Scheme generic t) =
    Set.diff (Type.variables t) (Set.fromList generic)
