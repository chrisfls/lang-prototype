module IR.Module exposing (Module(..), ModuleBody(..), ModuleExpr(..))

import IR.Annotation exposing (Annotation)
import IR.Expr exposing (Expr)


type Module
    = Param String Annotation Module
    | Let String ModuleExpr Module
    | ModuleBody ModuleBody


type ModuleExpr
    = Variable String
    | Apply ModuleExpr ModuleExpr
    | Import String


type ModuleBody
    = ReturnModule
    | DefSpec String Annotation ModuleBody
    | DefExpr String Expr ModuleBody
