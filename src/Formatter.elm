module Formatter exposing (Entries, format, span, text, wrap)


type Entries
    = Text String
    | Span (List Entries)
    | Wrap Wrapper (List Entries)


type alias Wrapper =
    { start : String
    , separator : String
    , end : String
    , span : Bool
    }


text : String -> Entries
text =
    Text


span : List Entries -> Entries
span =
    Span


wrap : Wrapper -> List Entries -> Entries
wrap =
    Wrap


format : Int -> Entries -> Maybe String
format max entries =
    case fit max "" 0 0 entries of
        Just ( string, _ ) ->
            Just string

        _ ->
            Nothing


fit : Int -> String -> Int -> Int -> Entries -> Maybe ( String, Int )
fit max buffer column level entry =
    case entry of
        Text _ ->
            case inline max buffer column entry of
                Nothing ->
                    spread max buffer level entry

                just ->
                    just

        Span entries ->
            fitSpan max buffer column level entries

        Wrap wrapper entries ->
            fitWrap max buffer column level wrapper entries


fitSpan : Int -> String -> Int -> Int -> List Entries -> Maybe ( String, Int )
fitSpan max buffer column level entries =
    fitSpanHelp 1 max buffer column level entries



-- TODO: check if code from wrap and span are similar


fitSpanHelp : Int -> Int -> String -> Int -> Int -> List Entries -> Maybe ( String, Int )
fitSpanHelp till max buffer column level entries =
    if till > 0 then
        case entries of
            entry :: nextEntries ->
                case inline max buffer column entry of
                    (Just ( entryBuffer, entryColumn )) as just ->
                        if List.isEmpty nextEntries then
                            just

                        else if entryColumn + 1 < max then
                            fitSpanHelp (till - 1) max (entryBuffer ++ " ") (entryColumn + 1) level nextEntries

                        else
                            spreadSpan max buffer column level entries

                    Nothing ->
                        spreadSpan max buffer column level entries

            [] ->
                Just ( buffer, column )

    else
        case inlineSpan max buffer column entries of
            Nothing ->
                spreadSpan max buffer column (level + 1) entries

            just ->
                just



-- TODO: check if wrap has span: true, if it has, use fitWrapHelp


fitWrap : Int -> String -> Int -> Int -> Wrapper -> List Entries -> Maybe ( String, Int )
fitWrap max buffer column level wrapper entries =
    case inlineWrap True max buffer level wrapper entries of
        Nothing ->
            spreadWrap True max buffer column level wrapper entries

        just ->
            just



-- inline


inline : Int -> String -> Int -> Entries -> Maybe ( String, Int )
inline max buffer column entry =
    case entry of
        Text string ->
            let
                length =
                    String.length string
            in
            if length + column < max then
                Just ( buffer ++ string, column + length )

            else
                Nothing

        Span entries ->
            inlineSpan max buffer column entries

        Wrap wrapper entries ->
            inlineWrap True max buffer column wrapper entries


inlineSpan : Int -> String -> Int -> List Entries -> Maybe ( String, Int )
inlineSpan max buffer column entries =
    case entries of
        entry :: nextEntries ->
            case inline max buffer column entry of
                Just ( nextBuffer, nextColumn ) ->
                    if List.isEmpty nextEntries then
                        Just ( nextBuffer, nextColumn )

                    else if nextColumn + 1 < max then
                        inlineSpan max (nextBuffer ++ " ") (nextColumn + 1) nextEntries

                    else
                        Nothing

                nothing ->
                    nothing

        [] ->
            Just ( buffer, column )


inlineWrap : Bool -> Int -> String -> Int -> Wrapper -> List Entries -> Maybe ( String, Int )
inlineWrap first max buffer column wrapper entries =
    case entries of
        entry :: nextEntries ->
            let
                prefix =
                    if first then
                        wrapper.start

                    else
                        wrapper.separator

                probColumn =
                    column + String.length prefix + 1
            in
            if probColumn < max then
                case inline max (buffer ++ prefix ++ " ") probColumn entry of
                    Just ( nextBuffer, nextColumn ) ->
                        inlineWrap False max nextBuffer nextColumn wrapper nextEntries

                    nothing ->
                        nothing

            else
                Nothing

        [] ->
            let
                string =
                    if first then
                        wrapper.start ++ wrapper.end

                    else
                        " " ++ wrapper.end

                nextColumn =
                    column + String.length string
            in
            if nextColumn < max then
                Just ( buffer ++ string, nextColumn )

            else
                Nothing



-- spread


spread : Int -> String -> Int -> Entries -> Maybe ( String, Int )
spread max buffer level entry =
    let
        ( indentBuffer, indentColumn ) =
            indent level buffer
    in
    case entry of
        Text string ->
            Just ( indentBuffer ++ string, indentColumn + String.length string )

        Span entries ->
            spreadSpan max indentBuffer indentColumn level entries

        Wrap wrapper entries ->
            spreadWrap True max indentBuffer indentColumn level wrapper entries


spreadSpan : Int -> String -> Int -> Int -> List Entries -> Maybe ( String, Int )
spreadSpan max buffer column level entries =
    case entries of
        entry :: nextEntries ->
            case fit max buffer column (level + 1) entry of
                (Just ( fitBuffer, _ )) as just ->
                    if List.isEmpty nextEntries then
                        just

                    else
                        let
                            ( indentBuffer, indentColumn ) =
                                indent level fitBuffer
                        in
                        spreadSpan max indentBuffer indentColumn level nextEntries

                nothing ->
                    nothing

        [] ->
            Just ( buffer, column )



-- TODO: copy paste from wrap into span


spreadWrap : Bool -> Int -> String -> Int -> Int -> Wrapper -> List Entries -> Maybe ( String, Int )
spreadWrap first max buffer column level wrapper entries =
    case entries of
        entry :: nextEntries ->
            let
                prefix =
                    if first then
                        wrapper.start

                    else
                        wrapper.separator

                probColumn =
                    column + String.length prefix + 1
            in
            case fit max (buffer ++ prefix ++ " ") probColumn (level + 1) entry of
                Just ( fitBuffer, _ ) ->
                    let
                        ( indentBuffer, indentColumn ) =
                            indent level fitBuffer
                    in
                    spreadWrap False max indentBuffer indentColumn level wrapper nextEntries

                nothing ->
                    nothing

        [] ->
            let
                suffix =
                    if first then
                        wrapper.start ++ wrapper.end

                    else
                        wrapper.end
            in
            Just ( buffer ++ suffix, column + String.length suffix )


indent : Int -> String -> ( String, Int )
indent level buffer =
    let
        padding =
            String.repeat level "  "
    in
    if buffer == "" then
        ( padding, String.length padding )

    else
        ( buffer ++ "\n" ++ padding, String.length padding )
