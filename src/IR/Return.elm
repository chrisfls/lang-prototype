module IR.Return exposing (Error(..), Return)


type alias Return a =
    Result Error a


type Error
    = Error
