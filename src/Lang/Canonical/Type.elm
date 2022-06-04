module Lang.Canonical.Type exposing (Name, Type(..), toString)


type alias Name =
    String


type Type
    = TVar Int
    | TArr Type Type


toString : Type -> String
toString thisT =
    case thisT of
        TVar index ->
            let
                argm =
                    max 0 (index - 12)
            in
            case List.head (List.drop (modBy length index) vars) of
                Just a ->
                    if argm > 0 then
                        a ++ String.fromInt argm

                    else
                        a

                Nothing ->
                    "unk" ++ String.fromInt index

        TArr ((TArr _ _) as f) t ->
            "[" ++ toString f ++ "] -> " ++ toString t

        TArr f t ->
            toString f ++ " -> " ++ toString t



--- internal


vars : List String
vars =
    String.split "" "abcdefghijkl"


length : Int
length =
    List.length vars
