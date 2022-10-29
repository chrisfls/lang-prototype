module Formatter exposing (Entries, format, span, text, wrap)


type Entries
    = Text String
    | Span (Maybe Wrapper) (List Entries)


type alias Wrapper =
    { start : String
    , separator : Maybe String
    , end : String
    }


type alias Context =
    { buffer : String
    , column : Int
    , dirty : Bool
    , max : Int
    }


text : String -> Entries
text =
    Text


span : List Entries -> Entries
span =
    Span Nothing


wrap : Wrapper -> List Entries -> Entries
wrap =
    Just >> Span


format : Int -> Entries -> String
format max entries =
    (fit 0
        entries
        { buffer = ""
        , column = 0
        , dirty = False
        , max = max
        }
    ).buffer



-- inline


inline : Entries -> Context -> Maybe Context
inline entry context =
    case entry of
        Text string ->
            write string context

        Span Nothing (this :: next) ->
            inlineList none space none this next context

        Span Nothing [] ->
            Just context

        Span (Just wrapper) (this :: next) ->
            inlineList (write wrapper.start >> andMap space)
                (writeMaybe wrapper.separator >> andMap space)
                (space >> andMap (write wrapper.end))
                this
                next
                context

        Span (Just wrapper) [] ->
            write wrapper.start context |> andMap (write wrapper.end)


inlineList :
    (Context -> Maybe Context)
    -> (Context -> Maybe Context)
    -> (Context -> Maybe Context)
    -> Entries
    -> List Entries
    -> Context
    -> Maybe Context
inlineList prefix separator suffix entry entries context =
    let
        context_ =
            prefix context
                |> andMap (inline entry)
    in
    case entries of
        next :: rest ->
            andMap (inlineList separator separator suffix next rest) context_

        [] ->
            andMap suffix context_


andMap : (a -> Maybe a) -> Maybe a -> Maybe a
andMap fn maybe =
    case maybe of
        Just context ->
            fn context

        nothing ->
            nothing


none : Context -> Maybe Context
none context =
    Just context


write : String -> Context -> Maybe Context
write string context =
    let
        column_ =
            context.column + String.length string
    in
    if not context.dirty || column_ <= context.max then
        Just
            { buffer = context.buffer ++ string
            , column = column_
            , dirty = True
            , max = context.max
            }

    else
        Nothing


writeMaybe : Maybe String -> Context -> Maybe Context
writeMaybe maybe context =
    case maybe of
        Just string ->
            write string context

        Nothing ->
            Just context


space : Context -> Maybe Context
space =
    write " "



-- fit


fit : Int -> Entries -> Context -> Context
fit depth entry context =
    case inline entry context of
        Just context_ ->
            context_

        Nothing ->
            spread depth entry context



-- spread


spread : Int -> Entries -> Context -> Context
spread depth entry context =
    case entry of
        Text string ->
            spreadWrite string context

        Span Nothing (this :: next) ->
            spreadList 2 identity identity identity depth this next context

        Span Nothing [] ->
            context

        Span (Just wrapper) (this :: next) ->
            case wrapper.separator of
                Just separator ->
                    spreadList 0
                        (spreadWrite wrapper.start >> spreadSpace)
                        (spreadWrite separator >> spreadSpace)
                        (spreadWrite wrapper.end)
                        depth
                        this
                        next
                        context

                Nothing ->
                    case next of
                        that :: next_ ->

                        [] ->
                            spreadList 2
                                (spreadWrite wrapper.start)
                                spreadSpace
                                (spreadWrite wrapper.end)
                                depth
                                this
                                next
                                context

        Span (Just wrapper) [] ->
            spreadWrite wrapper.start context |> spreadWrite wrapper.end


spreadList :
    Int
    -> (Context -> Context)
    -> (Context -> Context)
    -> (Context -> Context)
    -> Int
    -> Entries
    -> List Entries
    -> Context
    -> Context
spreadList count prefix separator suffix depth entry entries context =
    if count > 1 then
        let
            inlining =
                prefix context
                    |> inline entry
        in
        case inlining of
            Just inlined ->
                case entries of
                    next :: rest ->
                        spreadList (count - 1) separator separator suffix depth next rest inlined

                    [] ->
                        case toMaybe (suffix inlined) of
                            Just context_ ->
                                context_

                            Nothing ->
                                indent depth inlined |> suffix

            Nothing ->
                spreadList 0 prefix separator suffix (depth + 1) entry entries context

    else
        let
            context_ =
                prefix context
                    |> spreadSpace
                    |> fit depth entry
        in
        case entries of
            next :: rest ->
                indent depth context_ |> spreadList 0 separator separator suffix depth next rest

            [] ->
                indent depth context_ |> suffix


spreadWrite : String -> Context -> Context
spreadWrite string context =
    { buffer = context.buffer ++ string
    , column = context.column + String.length string
    , dirty = context.dirty
    , max = context.max
    }


spreadSpace : Context -> Context
spreadSpace =
    spreadWrite " "


indent : Int -> Context -> Context
indent depth context =
    { buffer = context.buffer ++ "\n" ++ String.repeat depth "  "
    , column = 2 * depth
    , dirty = False
    , max = context.max
    }


toMaybe : Context -> Maybe Context
toMaybe context =
    if context.column > context.max then
        Nothing

    else
        Just context
