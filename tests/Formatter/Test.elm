module Formatter.Test exposing (suite)

import Expect
import Formatter exposing (..)
import Test exposing (..)


brackets : List Entries -> Entries
brackets =
    wrap { start = "[", separator = ",", end = "]", span = False }


suite : Test
suite =
    describe "Formatter"
        [ test "text" <|
            \_ ->
                expectFormat [ "text" ] (text "text")
        , test "empty span" <|
            \_ ->
                expectFormat [ "" ] (span [])
        , test "1-element span" <|
            \_ ->
                expectFormat [ "a" ] (span [ text "a" ])
        , test "2-element span" <|
            \_ ->
                expectFormat [ "a b" ] (span [ text "a", text "b" ])
        , test "3-element span" <|
            \_ ->
                expectFormat [ "a b c" ] (span [ text "a", text "b", text "c" ])
        , test "1-depth span" <|
            \_ ->
                expectFormat
                    [ "Lor.em a"
                    , "  b"
                    , "  c"
                    , "  d"
                    ]
                    (span [ text "Lor.em", text "a", text "b", text "c", text "d" ])
        , test "2-depth span" <|
            \_ ->
                expectFormat
                    [ "Lor.em a"
                    , "  b"
                    , "  yuck c"
                    , "    d"
                    , "    e"
                    , "  f"
                    ]
                    (span [ text "Lor.em", text "a", text "b", span [ text "yuck", text "c", text "d", text "e" ], text "f" ])
        , test "3-depth span" <|
            \_ ->
                expectFormat
                    [ "Lor.em a"
                    , "  yuck b"
                    , "    puck c"
                    , "      d"
                    , "    e"
                    , "  f"
                    ]
                    (span [ text "Lor.em", text "a", span [ text "yuck", text "b", span [ text "puck", text "c", text "d" ], text "e" ], text "f" ])
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


expectFormat : List String -> Entries -> Expect.Expectation
expectFormat expect entries =
    Expect.equal expect (String.split "\n" <| Maybe.withDefault "NO SPACE" <| format 12 entries)
