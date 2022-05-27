module Lang.Canonical.Name exposing (Name, fromString, toString)


type Name
    = Name String


fromString : String -> Name
fromString =
    Name


toString : Name -> String
toString (Name name) =
    name
