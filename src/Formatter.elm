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


fitList :
    Int
    -> (Context -> Context)
    -> (Context -> Context)
    -> (Context -> Context)
    -> List Entries
    -> Context
    -> ( Context, List Entries )
fitList count prefix separator suffix entries context =
    if count > 0 then
        case entries of
            entry :: rest ->
                let
                    inlining =
                        prefix context
                            |> inline entry
                in
                case inlining of
                    Just inlined ->
                        fitList (count - 1) separator separator suffix rest inlined

                    Nothing ->
                        ( context, entries )

            [] ->
                ( context, entries )

    else
        ( context, entries )



-- spread


spread : Int -> Entries -> Context -> Context
spread depth entry context =
    case entry of
        Text string ->
            spreadWrite string context

        Span Nothing [] ->
            context

        Span Nothing entries ->
            let
                ( context_, rest ) =
                    fitList 2 identity spreadSpace identity entries context
            in
            case rest of
                this :: next ->
                    let
                        depth_ =
                            depth + 1
                    in
                    indent depth_ context_ |> spreadList identity identity identity depth_ depth_ this next

                [] ->
                    context_

        Span (Just wrapper) (this :: next) ->
            case wrapper.separator of
                Just separator ->
                    spreadList (spreadWrite wrapper.start >> spreadSpace)
                        (spreadWrite separator >> spreadSpace)
                        (indent depth >> spreadWrite wrapper.end)
                        depth
                        (depth + 1)
                        this
                        next
                        context

                Nothing ->
                    -- case next of
                    --     that :: next_ ->
                    --         Debug.todo "b"
                    --     [] ->
                    --         spread depth this context
                    -- spreadList
                    --     (spreadWrite wrapper.start)
                    --     spreadSpace
                    --     (spreadWrite wrapper.end)
                    --     depth
                    --     this
                    --     next
                    --     context
                    Debug.todo "b"

        Span (Just wrapper) [] ->
            spreadWrite wrapper.start context |> spreadWrite wrapper.end


spreadList :
    (Context -> Context)
    -> (Context -> Context)
    -> (Context -> Context)
    -> Int
    -> Int
    -> Entries
    -> List Entries
    -> Context
    -> Context
spreadList prefix separator suffix depth1 depth2 entry entries context =
    let
        context_ =
            prefix context
                |> fit depth2 entry
    in
    case entries of
        next :: rest ->
            indent depth1 context_ |> spreadList separator separator suffix depth1 depth2 next rest

        [] ->
            suffix context_


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
