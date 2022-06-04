module Lang.Infer exposing (Return(..), infer)

import Lang.Canonical.Expr exposing (Expr(..))
import Lang.Canonical.Type exposing (Type(..))
import Lang.Infer.Error exposing (Error(..))
import Lang.Infer.State as State exposing (State)



-- TODO: extend return to also return Expr
-- TODO: unshit some bits of code


type Return
    = Throw Error
    | Return Type State


return : ( Type, State ) -> Return
return ( thisT, state ) =
    Return thisT state


infer : Expr -> State -> Return
infer expr state =
    case infer2 expr state of
        Return thisT newState ->
            let
                ( someT, finalState ) =
                    State.unify thisT newState state
            in
            Return someT finalState

        throw ->
            throw

infer2 : Expr -> State -> Return
infer2 expr state =
    case expr of
        Var _ ->
            Debug.todo "TODO: proper unbound var error messages"

        Lam name body ->
            let
                ( argmT, newState ) =
                    State.nextTVar state
            in
            case infer2 (body (Ann argmT (Var name))) newState of
                Return bodyT finalState ->
                    Return (TArr argmT bodyT) finalState

                throw ->
                    throw

        App func argm ->
            case infer2 argm state of
                Return argmT newState ->
                    case infer2 func newState of
                        Return funcT finalState ->
                            apply funcT argmT finalState

                        throw ->
                            throw

                throw ->
                    throw

        Ann thisT (Var _) ->
            Return thisT state

        Ann thisT expr0 ->
            case infer2 expr0 state of
                Return someT _ ->
                    if someT == thisT then
                        Return thisT state

                    else
                        Throw (Error "TODO: proper type mismatch messages")

                throw ->
                    throw


apply : Type -> Type -> State -> Return
apply funcT argmT state =
    case funcT of
        TVar index ->
            case State.get index state of
                Just withT ->
                    contraintWith withT argmT state

                Nothing ->
                    constraint index argmT state

        withT ->
            contraintWith withT argmT state


constraint : Int -> Type -> State -> Return
constraint index argmT state =
    let
        ( tvar, state_ ) =
            State.nextTVar state
    in
    Return tvar (State.insert index (TArr argmT tvar) state_ )


contraintWith : Type -> Type -> State -> Return
contraintWith withT thisT state =
    -- TODO: test this function
    case withT of
        TArr funcT bodyT ->
            case thisT of
                TVar index ->
                    Return bodyT (State.insert index funcT state)

                _ ->
                    Throw (Error "TODO: try or elaborate why you can't constrain thisT to withT")

        _ ->
            Throw (Error "TODO: try or elaborate why you can't constrain thisT to withT when withT is not an arrow")
