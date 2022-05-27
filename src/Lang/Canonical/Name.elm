module Lang.Canonical.Name exposing (Name, fromString, compare)


type Name =
    Name String

fromString : String -> Name
fromString =
    Name

compare : Name -> Name -> Order
compare (Name a) (Name b) =
    Basics.compare a b

