module Infer.State exposing (State, empty, getByAddress, getByName, insertAtAddress, insertAtName, nextFreeAddress, removeAtName, unwrap, getFrees, insertBorrow, removeBorrow)


import Dict exposing (Dict)
import IR.Spec exposing (Address, Spec(..))
import IntDict exposing (IntDict)
import Set exposing (Set)


-- TODO: make free work for tracking free vars and used vars



type alias State =
    { graph : Graph
    , count : Int
    , scope : Scope
    , borrow : Set String
    }


type alias Graph =
    IntDict Spec



-- TODO: use IntDict


type alias Scope =
    Dict String Spec


empty : State
empty =
    State IntDict.empty 0 Dict.empty Set.empty


insertAtAddress : Address -> Spec -> State -> State
insertAtAddress address spec state =
    { state | graph = IntDict.insert address spec state.graph }


insertAtName : String -> Spec -> State -> State
insertAtName name spec state =
    { state | scope = Dict.insert name spec state.scope }


insertBorrow : String -> State -> State
insertBorrow name state =
    { state | borrow = Set.insert name state.borrow }

removeAtName : String -> State -> State
removeAtName name state =
    { state | scope = Dict.remove name state.scope }


removeBorrow : String -> State -> State
removeBorrow name state =
    { state | borrow = Set.remove name state.borrow }


nextFreeAddress : State -> ( Address, State )
nextFreeAddress ({ count } as state) =
    ( count, { state | count = count + 1 } )


getByAddress : Address -> State -> Maybe Spec
getByAddress index state =
    getHelp index state.graph

getFrees : State -> List String
getFrees state =
    -- TODO: optimize
    Dict.keys state.scope
        |> List.filter (\name -> not <| Set.member name state.borrow )



getByName : String -> State -> Maybe Spec
getByName name state =
    -- TODO: maybe unwrap
    case Dict.get name state.scope of
        (Just spec) as justSpec ->
            case spec of
                Reference address ->
                    case getByAddress address state of
                        (Just _) as justSpec_ ->
                            justSpec_

                        Nothing ->
                            justSpec

                _ ->
                    justSpec

        _ ->
            Nothing


unwrap : Spec -> State -> Spec
unwrap spec state =
    case spec of
        Reference address ->
            case getByAddress address state of
                Just nextSpec ->
                    if spec == nextSpec then
                        spec

                    else
                        unwrap nextSpec state

                Nothing ->
                    spec

        Arrow name func argm ->
            Arrow name (unwrap func state) (unwrap argm state)

        Linear subSpec ->
            Linear (unwrap subSpec state)

        Free name subSpec ->
            Free name (unwrap subSpec state)




-- internals


getHelp : Address -> Graph -> Maybe Spec
getHelp address graph =
    case IntDict.get address graph of
        (Just spec) as justSpec ->
            case spec of
                Reference nextAddress ->
                    case getHelp nextAddress graph of
                        (Just _) as justSpec_ ->
                            justSpec_

                        Nothing ->
                            justSpec

                _ ->
                    justSpec

        nothing ->
            nothing
