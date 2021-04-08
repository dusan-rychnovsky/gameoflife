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

emptyGrid : Grid
emptyGrid = Grid.create 5 5

suite : Test
suite =
  describe "game of life"
    [ describe "initial setup"
      [ test "respects num steps" <|
        \_ -> initialSetup.numSteps |> Expect.equal "1"
      , test "respects running" <|
        \_ -> initialSetup.running |> Expect.equal True
      ]
    , describe "tick"
      [ skip <| test "gets applied when running" <|
        \_ -> Expect.equal True False
      , skip <| test "gets ignored when not running" <|
        \_ -> Expect.equal True False
      ]
    ]
