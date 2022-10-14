module IR.Members exposing (Members, empty, insertExpr, insertSpec)

import IR.Bindings as Bindings
import IR.Spec as Spec exposing (Spec)


type alias Members =
    Spec.Members


empty : Members
empty =
    { exprs = Bindings.empty, specs = Bindings.empty }


insertExpr : String -> Spec -> Members -> Members
insertExpr name spec model =
    { model | exprs = Bindings.insert name spec model.exprs }


insertSpec : String -> Spec -> Members -> Members
insertSpec name spec model =
    { model | specs = Bindings.insert name spec model.specs }
