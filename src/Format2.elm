module Format2 exposing (..)


type Text
    = Text String
    | Span (Maybe Wrap) (Maybe String) (List Text)
    | Rows (List Text)


type alias Wrap =
    { start : String
    , end : String
    }


text : String -> Text
text =
    Text


span : { wrap : Maybe Wrap, separator : Maybe String } -> List Text -> Text
span { wrap, separator } =
    Span wrap separator


rows : List Text -> Text
rows =
    Rows


format : Int -> Text -> String
format width entries =
    (fit 0
        entries
        { buffer = ""
        , column = 0
        , dirty = False
        , max = width
        }
    ).buffer



-- INTERNALS


type alias Context =
    { buffer : String
    , column : Int
    , dirty : Bool
    , max : Int
    }



---- fit


fit : Int -> Text -> Context -> Context
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
    -> List Text
    -> Context
    -> ( Context, List Text )
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



---- inline


inline : Text -> Context -> Maybe Context
inline entry context =
    case entry of
        Text string ->
            write string context

        Span (Just wrapper) _ [] ->
            write wrapper.start context |> andMap (write wrapper.end)

        Span Nothing _ [] ->
            Just context

        Span (Just wrapper) (Just separator) (this :: next) ->
            inlineList (write wrapper.start >> andMap space)
                (write separator >> andMap space)
                (space >> andMap (write wrapper.end))
                this
                next
                context

        Span (Just wrapper) Nothing (this :: next) ->
            inlineList (write wrapper.start)
                space
                (write wrapper.end)
                this
                next
                context

        Span Nothing (Just separator) (this :: next) ->
            inlineList none
                (write separator >> andMap space)
                none
                this
                next
                context

        Span Nothing Nothing (this :: next) ->
            inlineList none space none this next context

        Rows _ ->
            Nothing


inlineList :
    (Context -> Maybe Context)
    -> (Context -> Maybe Context)
    -> (Context -> Maybe Context)
    -> Text
    -> List Text
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



---- spread


spread : Int -> Text -> Context -> Context
spread depth entry context =
    case entry of
        Text string ->
            spreadWrite string context

        Span (Just wrapper) _ [] ->
            spreadWrite wrapper.start context |> spreadWrite wrapper.end

        Span Nothing _ [] ->
            context

        Span Nothing Nothing entries ->
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
                    indent depth_ context_ |> spreadList identity (indent depth_) identity depth_ this next

                [] ->
                    context_

        Span (Just wrapper) (Just separator) (this :: next) ->
            spreadList (spreadWrite wrapper.start >> spreadSpace)
                (indent depth >> spreadWrite separator >> spreadSpace)
                (indent depth >> spreadWrite wrapper.end)
                (depth + 1)
                this
                next
                context

        Span Nothing (Just separator) (this :: next) ->
            spreadList identity
                (spreadWrite separator >> spreadSpace)
                identity
                (depth + 1)
                this
                next
                context

        Span (Just wrapper) Nothing entries ->
            let
                ( context_, rest ) =
                    fitList 2 (spreadWrite wrapper.start) spreadSpace identity entries context
            in
            case rest of
                this :: next ->
                    let
                        depth_ =
                            depth + 1

                        separator =
                            indent depth_
                    in
                    spreadList separator
                        separator
                        (indent depth >> spreadWrite wrapper.end)
                        depth_
                        this
                        next
                        context_

                [] ->
                    context_

        Rows [] ->
            context

        Rows (this :: next) ->
            spreadList identity identity identity depth this next context


spreadList :
    (Context -> Context)
    -> (Context -> Context)
    -> (Context -> Context)
    -> Int
    -> Text
    -> List Text
    -> Context
    -> Context
spreadList prefix separator suffix depth2 entry entries context =
    let
        context_ =
            prefix context
                |> fit depth2 entry
    in
    case entries of
        next :: rest ->
            spreadList separator separator suffix depth2 next rest context_

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
