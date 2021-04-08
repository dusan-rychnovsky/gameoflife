module MainTests exposing (..)

import Main exposing (..)
import Grid exposing (Grid)
import MaybeBool
import ListExt

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Array exposing (Array, get)
import List exposing (filter, range, map)

initialSetup : Model
initialSetup = init |> update (SetNumSteps "2" ) |> update Run

loneCell : Model
loneCell = initialSetup |> update (ToggleCell 2 2)

suite : Test
suite =
  describe "game of life"
    [ describe "initial setup"
      [ test "respects num steps" <|
        \_ -> initialSetup.numSteps |> Expect.equal "2"
      , test "respects running" <|
        \_ -> initialSetup.running |> Expect.equal True
      ]
    , describe "tick"
      [ test "gets ignored when not running - num steps remains untouched" <|
        \_ -> (initialSetup |> update Stop |> update Tick).numSteps |> Expect.equal "2"
      , test "gets applied when running - num steps gets decreased" <|
        \_ -> (initialSetup |> update Tick).numSteps |> Expect.equal "1"
      ]
    ]
