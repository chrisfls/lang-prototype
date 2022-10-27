module Formatter.Test exposing (suite)

import Expect
import Formatter exposing (..)
import Test exposing (..)


parens : List Entries -> Entries
parens =
    wrap { start = "(", separator = " ", end = ")" }


brackets : List Entries -> Entries
brackets =
    wrap { start = "[", separator = ", ", end = "]" }


braces : List Entries -> Entries
braces =
    wrap { start = "{", separator = ", ", end = "}" }


suite : Test
suite =
    describe "Formatter"
        [ test "span" <|
            \_ ->
                expectFormat "function [v, a, i]\n    {s, e}\n    [ f\n    , [u, d, []]\n    , (e r)\n    ]" <|
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
                expectFormat "pamonha" (text "pamonha")
        ]


expectFormat : String -> Entries -> Expect.Expectation
expectFormat expect entries =
    Expect.equal expect (Maybe.withDefault "" <| format 20 entries)
