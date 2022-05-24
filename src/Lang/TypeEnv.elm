module Lang.TypeEnv exposing (Scheme, TypeEnv, empty, extend, freeVariables, generalize, variable)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Lang.Canonical.Type as Type exposing (Type(..))
import Lang.Substitution as Substitution
import Set exposing (Set)
import State exposing (State)
import StateResult exposing (StateResult)


type Scheme
    = Scheme (List Int) Type


type TypeEnv
    = TypeEnv (Dict String Scheme)


empty : TypeEnv
empty =
    TypeEnv Dict.empty


variable : String -> TypeEnv -> StateResult String Type Int
variable name (TypeEnv env) =
    case Dict.get name env of
        Just t ->
            State.empty t
                |> State.andThen instantiate
                |> StateResult.fromState

        Nothing ->
            StateResult.error ("variable " ++ name ++ " not found")


instantiate : Scheme -> State Type Int
instantiate (Scheme vars t) =
    vars
        |> List.map instantiateHelp1
        |> State.sequence
        |> State.map (\a -> Substitution.substitute (Substitution.fromList a) t)


instantiateHelp1 : comparable -> State ( comparable, Type ) Int
instantiateHelp1 =
    Tuple.pair >> flip State.map Type.freshTVar


extend : String -> Type -> TypeEnv -> TypeEnv
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
