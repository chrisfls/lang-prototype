module Format2 exposing (BreakMode, Content, breakpoint, continue, force, format, optional, span, text, toString, write)


type Content
    = Span (List Content)
    | BreakPoint BreakMode Int
    | Text String


type BreakMode
    = Force
    | Optional
    | Continue


text : String -> Content
text =
    Text


span : List Content -> Content
span =
    Span


breakpoint : BreakMode -> Int -> Content
breakpoint =
    BreakPoint


force : BreakMode
force =
    Force


optional : BreakMode
optional =
    Optional


continue : BreakMode
continue =
    Continue


format : Int -> Content -> String
format width content =
    (item content
        { buffer = ""
        , column = 0
        , width = width
        }
    ).buffer


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
        Span entries ->
            list 0 False entries context

        BreakPoint _ _ ->
            context

        Text string ->
            write string context


list : Int -> Bool -> List Content -> Context -> Context
list indent broken xs context =
    case xs of
        head :: tail ->
            case head of
                BreakPoint Optional offset ->
                    if hasEnoughSpace context tail then
                        list indent broken tail context

                    else
                        break (indent + offset) tail context

                BreakPoint Force offset ->
                    break (indent + offset) tail context

                BreakPoint Continue offset ->
                    if broken then
                        break (indent + offset) tail context

                    else if hasEnoughSpace context tail then
                        list indent broken tail context

                    else
                        break (indent + offset) tail context

                Span xs_ ->
                    list indent False xs_ context
                        |> list indent broken tail

                Text string ->
                    write string context

        [] ->
            context


break : Int -> List Content -> Context -> Context
break indent xs context =
    linebreak indent context
        |> list indent True xs


hasEnoughSpace : Context -> List Content -> Bool
hasEnoughSpace context xs =
    hasEnoughSpaceHelp context.width context.column xs


hasEnoughSpaceHelp : Int -> Int -> List Content -> Bool
hasEnoughSpaceHelp width column xs =
    case xs of
        [] ->
            True

        this :: rest ->
            case this of
                BreakPoint Force _ ->
                    True

                BreakPoint _ _ ->
                    hasEnoughSpaceHelp width column rest

                Span content ->
                    if hasEnoughSpaceHelp width column content then
                        hasEnoughSpaceHelp width column rest

                    else
                        False

                Text string ->
                    let
                        column_ =
                            column + String.length string
                    in
                    if column_ <= width then
                        hasEnoughSpaceHelp width column_ rest

                    else
                        False


write : String -> Context -> Context
write string context =
    { buffer = context.buffer ++ string
    , column = context.column + String.length string
    , width = context.width
    }


linebreak : Int -> Context -> Context
linebreak column context =
    { buffer = context.buffer ++ "\n" ++ String.repeat column " "
    , column = column
    , width = context.width
    }
