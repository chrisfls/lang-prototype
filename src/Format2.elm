module Format2 exposing (BreakMode, Content, breakpoint, container, force, format, optional, span, text, toString, write)


type Content
    = Span Int (List Content)
    | BreakPoint BreakMode
    | Text String


type BreakMode
    = Force
    | Optional Separator
    | Container Separator


type alias Separator =
    Maybe String


text : String -> Content
text =
    Text


span : Int -> List Content -> Content
span =
    Span


breakpoint : BreakMode -> Content
breakpoint =
    BreakPoint


force : BreakMode
force =
    Force


optional : Maybe String -> BreakMode
optional =
    Optional


container : Maybe String -> BreakMode
container =
    Container


format : Int -> Content -> String
format width content =
    (item content
        { buffer = ""
        , column = 0
        , width = width
        }
    ).buffer
        |> String.trimRight


toString : Content -> String
toString =
    format 80



-- internals


type alias Context =
    { buffer : String
    , column : Int
    , width : Int
    }


item : Content -> Context -> Context
item content context =
    case content of
        Span indent entries ->
            list 0 indent (hasEnoughSpace entries context) entries context

        BreakPoint _ ->
            context

        Text string ->
            write string context


list : Int -> Int -> Bool -> List Content -> Context -> Context
list indent offset whole entries context =
    if whole then
        listHelp indent whole entries context

    else
        listHelp (indent + offset) whole entries context


listHelp : Int -> Bool -> List Content -> Context -> Context
listHelp indent whole xs context =
    case xs of
        head :: tail ->
            case head of
                BreakPoint (Optional fallback) ->
                    if whole then
                        writeMaybe fallback context
                            |> listHelp indent whole tail

                    else
                        case tail of
                            next :: _ ->
                                if hasEnoughSpace [ next ] context then
                                    writeMaybe fallback context
                                        |> listHelp indent whole tail

                                else
                                    break indent tail context

                            _ ->
                                break indent tail context

                BreakPoint Force ->
                    break indent tail context

                BreakPoint (Container fallback) ->
                    if whole then
                        writeMaybe fallback context
                            |> listHelp indent whole tail

                    else
                        break indent tail context

                Span offset entries ->
                    list indent offset (hasEnoughSpace entries context) entries context
                        |> listHelp indent whole tail

                Text string ->
                    write string context
                        |> listHelp indent whole tail

        [] ->
            context


break : Int -> List Content -> Context -> Context
break indent xs context =
    linebreak indent context
        |> listHelp indent False xs


hasEnoughSpace : List Content -> Context -> Bool
hasEnoughSpace xs context =
    case hasEnoughSpaceHelp context.width context.column xs of
        Just column ->
            column < context.width

        Nothing ->
            False


hasEnoughSpaceHelp : Int -> Int -> List Content -> Maybe Int
hasEnoughSpaceHelp width column xs =
    case xs of
        [] ->
            Just column

        this :: rest ->
            case this of
                BreakPoint Force ->
                    Nothing

                BreakPoint (Optional (Just offset)) ->
                    hasEnoughSpaceHelp width (column + String.length offset) rest

                BreakPoint (Container (Just offset)) ->
                    hasEnoughSpaceHelp width (column + String.length offset) rest

                BreakPoint _ ->
                    hasEnoughSpaceHelp width column rest

                Span _ content ->
                    case hasEnoughSpaceHelp width column content of
                        Just column_ ->
                            if column_ <= width then
                                hasEnoughSpaceHelp width column_ rest

                            else
                                Nothing

                        nothing ->
                            nothing

                Text string ->
                    let
                        column_ =
                            column + String.length string
                    in
                    if column_ <= width then
                        hasEnoughSpaceHelp width column_ rest

                    else
                        Nothing


write : String -> Context -> Context
write string context =
    { buffer = context.buffer ++ string
    , column = context.column + String.length string
    , width = context.width
    }


writeMaybe : Maybe String -> Context -> Context
writeMaybe maybe context =
    case maybe of
        Just string ->
            write string context

        Nothing ->
            context


linebreak : Int -> Context -> Context
linebreak column context =
    { buffer =
        String.trimRight context.buffer ++ "\n" ++ String.repeat column " "
    , column = column
    , width = context.width
    }
