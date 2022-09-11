module IR.Env exposing
    ( Env
    , Name
    , empty
    )

import Dict exposing (Dict)
import IR.Env.SpecGraph as SpecGraph
import IR.Expr exposing (Expr)
import IR.Spec exposing (Spec)
import IntDict exposing (IntDict)


type alias Env =
    { exprs : Dict Name Expr
    , specs : Dict Name Spec
    , graph : IntDict Spec
    , count : Int
    }


type alias Name =
    String


empty : Env
empty =
    { exprs = Dict.empty
    , specs = Dict.empty
    , graph = SpecGraph.empty.graph
    , count = SpecGraph.empty.count
    }



-- insert : Name -> a -> Environment a -> Environment a
-- insert =
--     Dict.insert
-- remove : Name -> Environment a -> Environment a
-- remove =
--     Dict.remove
-- get : Name -> Environment a -> Maybe a
-- get =
--     Dict.get
