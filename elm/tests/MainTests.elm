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
initialSetup = init |> update (SetNumSteps "1" ) |> update Run

loneCell : Model
loneCell = initialSetup |> update (ToggleCell 2 2)

suite : Test
suite =
  describe "game of life"
    [ describe "initial setup"
      [ test "respects num steps" <|
        \_ -> initialSetup.numSteps |> Expect.equal "1"
      , test "respects running" <|
        \_ -> initialSetup.running |> Expect.equal True
      ]
    , describe "set num steps"
      [ test "rejects negative values" <|
        \_ -> (update (SetNumSteps "-1") initialSetup).numSteps |> Expect.equal "0"
      ]
    , describe "tick"
      [ test "gets ignored when not running - num steps remains untouched" <|
        \_ -> (loneCell |> update Stop |> update Tick).numSteps |> Expect.equal "1"
      , test "gets ignored when not running - running stays False" <|
        \_ -> (loneCell |> update Stop |> update Tick).running |> Expect.equal False
      , test "gets ignored when not running - grid remains untouched" <|
        \_ -> Grid.equals loneCell.grid (loneCell |> update Stop |> update Tick).grid |> Expect.equal True
      , test "gets applied when running - num steps gets decreased" <|
        \_ -> (loneCell |> update Tick).numSteps |> Expect.equal "0"
      , test "gets applied when running - running stays True" <|
        \_ -> (loneCell |> update Tick).running |> Expect.equal True
      , test "gets applied when running - grid gets updated" <|
        \_ -> Grid.equals initialSetup.grid (loneCell |> update Tick).grid |> Expect.equal True
      , test "stops when finished - num steps remains 0" <|
        \_ -> (initialSetup |> update Tick |> update Tick).numSteps |> Expect.equal "0"
      , test "stops when finished - running gets set to False" <|
        \_ -> (initialSetup |> update Tick |> update Tick).running |> Expect.equal False
      ]
    ]
