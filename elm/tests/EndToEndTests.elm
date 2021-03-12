module EndToEndTests exposing (..)

import Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

initialSetup : Model
initialSetup = init |> update (SetNumSteps "1" ) |> update Run

suite : Test
suite =
  describe "initial setup"
    [ test "respects num steps" <|
      \_ -> initialSetup.numSteps |> Expect.equal "1"
    , test "respects run" <|
      \_ -> initialSetup.running |> Expect.equal True
    ]
