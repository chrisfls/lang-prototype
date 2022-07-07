module Lang.Infer.Expr exposing (infer)

import Lang.Canonical.Expr as Expr exposing (Expr)
import Lang.Canonical.Type as Type exposing (Type)
import Lang.Infer.Error exposing (Error(..))
import Lang.Infer.Return exposing (Return(..))
import Lang.Infer.State as State exposing (State)



-- maybe move this into Lang.Canonical.Expr


infer : Expr -> State -> Return
infer expr state =
    case expr of
        Expr.Var _ ->
            Debug.todo "TODO: proper unbound var error messages"

        Expr.Tup _ ->
            Debug.todo "TODO: support tuples"

        Expr.Lam name body ->
            let
                ( argmT, newState ) =
                    State.nextTVar state
            in
            case infer (body (Expr.Ann argmT (Expr.Var name))) newState of
                Return bodyT finalState ->
                    Return (State.unwrap (Type.Arr argmT bodyT) finalState) finalState

                throw ->
                    throw

        Expr.App func argm ->
            case infer argm state of
                Return argmT newState ->
                    case infer func newState of
                        Return funcT finalState ->
                            apply funcT argmT finalState

                        throw ->
                            throw

                throw ->
                    throw

        Expr.Ann t (Expr.Var _) ->
            Return t state

        Expr.Ann t expr0 ->
            case infer expr0 state of
                Return someT _ ->
                    if someT == t then
                        Return t state

                    else
                        Throw (Error "TODO: proper type mismatch messages")

                throw ->
                    throw


apply : Type -> Type -> State -> Return
apply funcT argmT state =
    case funcT of
        Type.Var index ->
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
    Return tvar (State.insert index (Type.Arr argmT tvar) state_)


contrainWith : Type -> Type -> State -> Return
contrainWith withT t state =
    -- TODO: test this function
    case withT of
        Type.Arr argmT bodyT ->
            case t of
                Type.Var index ->
                    Return bodyT (State.insert index argmT state)

                _ ->
                    Throw (Error "TODO: try or elaborate why you can't constrain t to withT")

        _ ->
            Throw (Error "TODO: try or elaborate why you can't constrain t to withT when withT is not an arrow")
