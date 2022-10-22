```elm

import a from "xyz"
import b from "xyz"
import c from "xyz"

param a : Annotation

# params can have annotation
param b
param c

apply a = Dict A

ifEq a b then
  ifEq

module where

# single line comment

###
multiple line comment
###

# basic function
identity = \a ->
  a

# type annotated function
always : a -> b -> a
always = \a b ->
  a

# linear function
discard : a -> *b -> a
discard = \a *b ->
  a

# type alias 
type Address =
  Int

# side effecting function
logAddress : Address -> *IO -> *IO
logAddress = \addr *io ->
  IO.log (String.fromInt addr) io

# union type
type Maybe x =
  | Some x
  | None

# opaque type
type Id =
  | Id String




```