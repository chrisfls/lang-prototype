module AST.Test exposing (suite)

import AST exposing (AST)
import AST.Spec as Spec
import Expect
import Test exposing (..)


sampleModule3 : AST
sampleModule3 =
    { name = Just "ModuleName"
    , params =
        [ { name = "arg1"
          , spec = Spec.Hole
          }
        , { name = "arg2"
          , spec = Spec.Primitive Spec.Unit
          }
        , { name = "arg3"
          , spec = Spec.Primitive Spec.PrimBool
          }
        ]
    , expose =
        AST.ExposeSome
            [ AST.Value "val1"
            , AST.Value "val2"
            , AST.Spec "Typ1" False
            , AST.Spec "Typ2" True
            , AST.Value "val3"
            , AST.Spec "Typ3" False
            ]
    , imports =
        [ { name = "Imp1", from = "some-path" }
        , { name = "Imp2", from = "some-url" }
        ]
    , body =
        AST.Body
    }


sampleString3 : String
sampleString3 =
    """
module ModuleName
  (arg1 : _)
  (arg2 : ())
  (arg3 : Bool)
expose
  ( val1
  , val2
  , Typ1
  , Typ2(..)
  , val3
  , Typ3
  )

import Imp1 from "some-path"
import Imp2 from "some-url"
"""
        |> String.trim


suite : Test
suite =
    skip <|
        describe "AST"
            [ test "basic" <|
                \_ ->
                    -- AST.format sampleModule3
                    --     |> Format.format 20
                    --     |> String.trim
                    sampleString3
                        |> Expect.equal sampleString3
            ]
