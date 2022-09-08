module IR.Spec exposing
    ( Spec(..)
    , Address(..)
    , ModuleDefs, SpecDict
    , SpecName(..), SpecNameString
    )

{-|

@docs Spec
@docs Address
@docs ModuleDefs, SpecDict
@docs SpecName, SpecNameString

-}

import Dict exposing (Dict)
import IR.ModuleName exposing (ModuleName)


{-| Type specifications.

Simply typed lambda calculus:

  - Reference
  - Arrow

Module system extension:

  - Module
  - SpecAt
  - ModuleSpecAt

-}
type Spec
    = Reference Address
    | Arrow Spec Spec
    | Module ModuleDefs
    | SpecAt SpecName
    | ModuleSpecAt ModuleName SpecName


type Address
    = Address Int


type alias ModuleDefs =
    { specs : SpecDict
    , exprs : SpecDict
    }


type alias SpecDict =
    Dict SpecNameString Spec


type SpecName
    = SpecName SpecNameString


type alias SpecNameString =
    String
