module Lang.Infer exposing (Return(..), infer)

import Lang.Canonical.Expr exposing (Expr(..))
import Lang.Canonical.Type exposing (Type(..))
import Lang.Infer.Error exposing (Error(..))
import Lang.Infer.State as State exposing (State)


type Return
    = Throw Error
    | Return Type State


infer : Expr -> State -> Return
infer expr state =
    case expr of
        Var _ ->
            Debug.todo "TODO: proper unbound var error messages"

        Lam name body ->
            let
                ( argmT, newState1 ) =
                    State.nextTVar state
            in
            case infer (body (Ann argmT (Var name))) newState1 of
                Return bodyT newState2 ->
                    let
                        ( bodyT_, finalState ) =
                            State.unify (TArr argmT bodyT) newState2 state
                    in
                    Return bodyT_ finalState

                throw ->
                    throw

        App func argm ->
            case infer argm state of
                Return argmT newState ->
                    case infer func newState of
                        Return funcT finalState ->
                            apply funcT argmT finalState

                        throw ->
                            throw

                throw ->
                    throw

        Ann thisT (Var _) ->
            Return thisT state

        Ann thisT expr0 ->
            case infer expr0 state of
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
                    contrainWith withT argmT state

                Nothing ->
                    constrain index argmT state

        withT ->
            contrainWith withT argmT state


constrain : Int -> Type -> State -> Return
constrain index argmT state =
    let
        ( tvar, state_ ) =
            State.nextTVar state
    in
    Return tvar (State.insert index (TArr argmT tvar) state_)


contrainWith : Type -> Type -> State -> Return
contrainWith withT thisT state =
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
