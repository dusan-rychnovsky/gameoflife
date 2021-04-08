module EndToEndTests exposing (..)

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

suite : Test
suite =
  describe "game of life"
    [ test "initial setup respects num steps" <|
    \_ -> initialSetup.numSteps |> Expect.equal "1"
    , test "initial setup respects running" <|
      \_ -> initialSetup.running |> Expect.equal True
    , test "empty grid stays empty" <|
      \_ ->
        let
          oneTickCells = (update Tick initialSetup).grid
        in
          aliveCellsEquals [] oneTickCells |> Expect.true "Contains zero alive cells"
    , test "lone cell dies off" <|
      \_ ->
        let
          loneCell = initialSetup |> update (ToggleCell 3 3)
          oneTickCells = (update Tick loneCell).grid
        in
          aliveCellsEquals [] oneTickCells |> Expect.true "Contains zero alive cells"
    , test "block remains still" <|
      \_ ->
        let
          block = initialSetup |> update (ToggleCell 3 3) |> update (ToggleCell 3 4) |> update (ToggleCell 4 3) |> update (ToggleCell 4 4)
          oneTickCells = (update Tick block).grid
        in
          aliveCellsEquals [(3, 3), (3, 4), (4, 3), (4, 4)] oneTickCells |> Expect.true "Contains a block"
    ]

maybeBoolToString : Maybe Bool -> String
maybeBoolToString maybe =
  case maybe of
    Just True -> "T"
    Just False -> "F"
    Nothing -> "X"

aliveCellsEquals : List (Int, Int) -> Grid -> Bool
aliveCellsEquals coords grid =
  areEqual coords (Grid.alivePositions grid)

areEqual : List a -> List a -> Bool
areEqual firstList secondList =
  (List.length firstList == List.length secondList) && (isSubset firstList secondList)

isSubset : List a -> List a -> Bool
isSubset firstList secondList =
  case firstList of
    [] -> True
    head :: tail -> (List.member head secondList) && (isSubset tail secondList)
