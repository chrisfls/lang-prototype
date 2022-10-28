module Formatter.Test exposing (suite)

import Expect
import Formatter exposing (..)
import Test exposing (..)


parens : List Entries -> Entries
parens =
    wrap { start = "(", separator = "", end = ")" }


brackets : List Entries -> Entries
brackets =
    wrap { start = "[", separator = ",", end = "]" }


braces : List Entries -> Entries
braces =
    wrap { start = "{", separator = ",", end = "}" }


suite : Test
suite =
    describe "Formatter"
        [ skip <|
            test "span" <|
                \_ ->
                    expectFormat
                        [ "function [v, a, i]"
                        , "  {s, e}"

                        -- , "  [f, [u, d, []], "
                        -- , "    ( e"
                        -- , "     r"
                        -- , "    )]"
                        , "  [ f"
                        , "  , [u, d, []]"
                        , "  , (e r)"
                        , "  ]"
                        ]
                    <|
                        span
                            [ text "function"
                            , brackets
                                [ text "v"
                                , text "a"
                                , text "i"
                                ]
                            , braces
                                [ text "s"
                                , text "e"
                                ]
                            , brackets
                                [ text "f"
                                , brackets
                                    [ text "u"
                                    , text "d"
                                    , brackets []
                                    ]
                                , parens [ text "e", text "r" ]
                                ]
                            ]
        , test "text" <|
            \_ ->
                expectFormat [ "text" ] (text "text")
        , test "empty wrap" <|
            \_ ->
                expectFormat [ "[]" ] (brackets [])
        , test "1-element wrap" <|
            \_ ->
                expectFormat [ "[ a ]" ] (brackets [ text "a" ])
        , test "2-element wrap" <|
            \_ ->
                expectFormat [ "[ a, b ]" ] (brackets [ text "a", text "b" ])
        , test "32-element wrap" <|
            \_ ->
                expectFormat [ "[ a, b, c ]" ] (brackets [ text "a", text "b", text "c" ])
        , test "1-depth wrap" <|
            \_ ->
                expectFormat
                    [ "[ a"
                    , ", b"
                    , ", c"
                    , ", d"
                    , "]"
                    ]
                    (brackets [ text "a", text "b", text "c", text "d" ])
        , test "2-depth wrap" <|
            \_ ->
                expectFormat
                    [ "[ a"
                    , ", [ b"
                    , "  , c"
                    , "  , d"
                    , "  ]"
                    , ", e"
                    , ", [ f ]"
                    , ", []"
                    , "]"
                    ]
                    (brackets <|
                        [ text "a"
                        , brackets [ text "b", text "c", text "d" ]
                        , text "e"
                        , brackets [ text "f" ]
                        , brackets []
                        ]
                    )
        , test "3-depth wrap" <|
            \_ ->
                expectFormat
                    [ "[ a"
                    , ", [ b"
                    , "  , [ c ]"
                    , "  , d"
                    , "  ]"
                    , ", e"
                    , "]"
                    ]
                    (brackets <|
                        [ text "a"
                        , brackets [ text "b", brackets [ text "c" ], text "d" ]
                        , text "e"
                        ]
                    )
        ]


a =
    [ "[ a"
    , ", [ b"
    , "  , c"
    , "  ]"
    , ", d"
    , ", [ e ]"
    , ", []"
    , "]"
    ]


expectFormat : List String -> Entries -> Expect.Expectation
expectFormat expect entries =
    Expect.equal expect (String.split "\n" <| Maybe.withDefault "NO SPACE" <| format 12 entries)
