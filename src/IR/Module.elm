module IR.Module exposing (Module(..), ModuleBody(..), Privacy(..))

import IR.Annotation exposing (Annotation)
import IR.Expr exposing (Expr)


type Module
    = Variable String
    | Lambda String Module
    | Apply Module Module
    | Annotation Annotation
    | IfEquals String String Module Module
    | Module ModuleBody


type ModuleBody
    = CloseModule
    | DefSpec Privacy String Annotation ModuleBody
    | DefExpr Privacy String Expr ModuleBody


type Privacy
    = Private
    | Public
