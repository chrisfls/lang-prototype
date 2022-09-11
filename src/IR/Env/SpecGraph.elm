module IR.Env.SpecGraph exposing (SpecGraph, empty, get, insert, nextTVar, unwrap)

import IR.Spec exposing (Spec)
import IntDict exposing (IntDict)


type alias SpecGraph a =
    { a | graph : IntDict Spec, count : Int }


empty : SpecGraph {}
empty =
    { graph = IntDict.empty
    , count = 0
    }


insert : Int -> Spec -> SpecGraph a -> SpecGraph a
insert index t graph =
    -- case getLastTVar t graph.env of
    --     (Var _) as t_ ->
    --         insertHelp index t_ state
    --     Arr left right ->
    --         insertHelp index
    --             (Arr (getLastTVar left graph.env) (getLastTVar right graph.env))
    --             state
    --     Tup types ->
    --         insertHelp index
    --             (Tup (List.map (\t_ -> getLastTVar t_ graph.env) types))
    --             state
    --     Rec maybeE fields ->
    --         insertHelp index
    --             (Rec
    --                 (Maybe.map (\t_ -> getLastTVar t_ graph.env) maybeE)
    --                 (Dict.map (\_ t_ -> getLastTVar t_ graph.env) fields)
    --             )
    --             state
    --     (Bul _) as t_ ->
    --         insertHelp index t_ state
    --     (Adt _ _) as t_ ->
    --         insertHelp index t_ state
    Debug.todo "a"


nextTVar : SpecGraph a -> ( Spec, SpecGraph a )
nextTVar state =
    let
        ( index, state_ ) =
            nextTVarHelp state
    in
    -- ( Var index, state_ )
    Debug.todo "a"


get : Int -> SpecGraph a -> Maybe Spec
get index graph =
    getHelp index graph.graph


unwrap : Spec -> SpecGraph a -> Spec
unwrap t graph =
    -- case t of
    --     Var i ->
    --         case get i graph of
    --             Just t_ ->
    --                 if t == t_ then
    --                     t
    --                 else
    --                     unwrap t_ graph
    --             Nothing ->
    --                 t
    --     Arr l r ->
    --         Arr (unwrap l graph) (unwrap r graph)
    --     Tup xs ->
    --         Tup (List.map (\t_ -> unwrap t_ graph) xs)
    --     Rec mx xs ->
    --         Rec (Maybe.map (\t_ -> unwrap t_ graph) mx)
    --             (Dict.map (\_ t_ -> unwrap t_ graph) xs)
    --     Bul _ ->
    --         t
    --     Adt _ _ ->
    --         t
    Debug.todo "a"



-- -- internals


insertHelp : Int -> Spec -> SpecGraph a -> SpecGraph a
insertHelp index t graph =
    { graph | graph = IntDict.insert index t graph.graph }


nextTVarHelp : SpecGraph a -> ( Int, SpecGraph a )
nextTVarHelp graph =
    ( graph.count, { graph | count = graph.count + 1 } )


getHelp : Int -> IntDict Spec -> Maybe Spec
getHelp index dict =
    case IntDict.get index dict of
        (Just t) as just ->
            -- case t of
            --     Var index_ ->
            --         case getHelp index_ dict of
            --             (Just _) as just_ ->
            --                 just_
            --             Nothing ->
            --                 just
            --     _ ->
            --         just
            Debug.todo "a"

        nothing ->
            nothing


getLastTVar : Spec -> IntDict Spec -> Spec
getLastTVar t dict =
    -- case t of
    --     Var index ->
    --         getLastTVarHelp index t dict
    --     _ ->
    --         t
    Debug.todo "a"


getLastTVarHelp : Int -> Spec -> SpecGraph a -> Spec
getLastTVarHelp index t graph =
    case IntDict.get index graph.graph of
        Just t_ ->
            -- case t_ of
            --     Var index_ ->
            --         getLastTVarHelp index_ t_ graph
            --     _ ->
            --         t
            Debug.todo "a"

        Nothing ->
            t
