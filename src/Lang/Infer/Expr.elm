module Lang.Infer.Expr exposing (infer)

import Dict
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

        Expr.Tup types ->
            inferTup types [] state

        Expr.Rec types ->
            inferRec Nothing (Dict.toList types) [] state

        Expr.Fil name field ->
            case infer name state of
                Return nameT newState1 ->
                    Debug.todo "inferFil"

                -- let
                --     ( fieldT, newState2 ) =
                --         State.nextTVar newState1
                -- in
                -- inferRec (Just nameT) [] [(field, fieldT)] newState2
                throw ->
                    throw

        Expr.Upd name types ->
            case infer name state of
                Return nameT finalState ->
                    inferRec (Just nameT) (Dict.toList types) [] finalState

                throw ->
                    throw

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
                Return t_ _ ->
                    if t_ == t then
                        Return t state

                    else
                        Throw (Error "TODO: proper type mismatch messages")

                throw ->
                    throw

        Expr.Bul builtin ->
            case builtin of
                Expr.UnitVal ->
                    Return Type.unit state

                Expr.TrueVal ->
                    Return Type.bool state

                Expr.FalseVal ->
                    Return Type.bool state

                Expr.IntVal _ ->
                    Return Type.int state

                Expr.FloatVal _ ->
                    Return Type.float state

                Expr.StringVal _ ->
                    Return Type.string state


inferTup : List Expr -> List Type -> State -> Return
inferTup types xs state =
    case types of
        [] ->
            Return (Type.Tup (List.reverse xs)) state

        head :: tail ->
            case infer head state of
                Return t nextState ->
                    inferTup tail (t :: xs) nextState

                throw ->
                    throw


inferRec : Maybe Type -> List ( String, Expr ) -> List ( String, Type ) -> State -> Return
inferRec ext types xs state =
    case types of
        [] ->
            Return (Type.Rec ext (Dict.fromList xs)) state

        ( name, head ) :: tail ->
            case infer head state of
                Return t nextState ->
                    inferRec ext tail (( name, t ) :: xs) nextState

                throw ->
                    throw


apply : Type -> Type -> State -> Return
apply funcT argmT state =
    case funcT of
        Type.Var index ->
            case State.get index state of
                Just t ->
                    contrainWith t argmT state

                Nothing ->
                    constrain index argmT state

        t ->
            contrainWith t argmT state


constrain : Int -> Type -> State -> Return
constrain index t state =
    let
        ( tvar, state_ ) =
            State.nextTVar state
    in
    Return tvar (State.insert index (Type.Arr t tvar) state_)


contrainWith : Type -> Type -> State -> Return
contrainWith tA tB state =
    -- TODO: test this function
    case tA of
        Type.Arr argmT bodyT ->
            case tB of
                Type.Var index ->
                    Return bodyT (State.insert index argmT state)

                _ ->
                    Throw (Error "TODO: try or elaborate why you can't constrain tB to tA")

        _ ->
            Throw (Error "TODO: try or elaborate why you can't constrain tB to tA when tA is not an arrow")