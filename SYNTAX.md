```elm

module Name
  (X : Annotation)
  (Y : b)
  (Z : c)
expose (..)

import X from "xyz"
import Y from "xyz"
import Z from "xyz"


# uppercase = without types affect modules
A = Dict A



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
