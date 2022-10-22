module IR.Module2 exposing (Module(..), ModuleBody(..), ModuleExpr(..))

import IR.Annotation exposing (Annotation)
import IR.Expr exposing (Expr)


type Module
    = Param String Annotation Module
    | DefModule String ModuleExpr
    | ModuleBody ModuleBody


type ModuleExpr
    = Import String
    | Variable String
    | Apply ModuleExpr ModuleExpr


type ModuleBody
    = ReturnModule
    | DefSpec String Annotation ModuleBody
    | DefExpr String Expr ModuleBody
