module EndToEndTests exposing (..)

import Main exposing (..)

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
          oneTickCells = (update Tick initialSetup).cells
        in
          aliveCellsEquals [] oneTickCells |> Expect.true "Contains zero alive cells"
    ]

aliveCellsEquals : List (Int, Int) -> Array Bool -> Bool
aliveCellsEquals coords cells =
  areEqual coords (toAliveCoords cells)

areEqual : List a -> List a -> Bool
areEqual firstList secondList =
  (List.length firstList == List.length secondList) && (isSubset firstList secondList)

isSubset : List a -> List a -> Bool
isSubset firstList secondList =
  case firstList of
    [] -> True
    head :: tail -> (List.member head secondList) && (isSubset tail secondList)

toAliveCoords : Array Bool -> List (Int, Int)
toAliveCoords cells =
  let
    allCoords = List.map2 (\posY posX -> (posY, posX)) (List.range 0 (grid_height - 1)) (List.range 0 (grid_width - 1))
  in
    allCoords |>
      List.map (\(posY, posX) -> (posY, posX, Array.get (coordsToIndex posY posX) cells)) |>
      List.filter (\(_, _, alive) -> maybeBoolToBool alive) |>
      List.map (\(posY, posX, _) -> (posY, posX))