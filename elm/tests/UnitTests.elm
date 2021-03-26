module UnitTests exposing (..)

import Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Array exposing (Array, set)
import List exposing (foldl)

suite : Test
suite =
  describe "transition function"
    [ test "a live cell with fewer than two live neighbours dies, as if by underpopulation" <|
      \_ -> (staysAlive (3, 3) (populateGrid [(3, 3)])) |> Expect.equal False
    , skip <| test "a live cell with two live neighbours lives on to the next generation" <|
      \_ -> False |> Expect.equal True
    , skip <| test "a live cell with three live neighbours lives on to the next generation" <|
      \_ -> False |> Expect.equal True
    , skip <| test "a live cell with more than three live neighbours dies, as if by overpopulation" <|
      \_ -> False |> Expect.equal True
    , skip <| test "a dead cell with exactly three live neighbours becomes a live cell, as if by reproduction" <|
      \_ -> False |> Expect.equal True
    ]

populateGrid : List (Int, Int) -> Array Bool
populateGrid coords =
  let
    emptyGrid = Array.initialize (grid_width * grid_height) (always False)
  in
    List.foldl markCellAlive emptyGrid coords

markCellAlive : (Int, Int) -> Array Bool -> Array Bool
markCellAlive (posY, posX) grid =
  Array.set (coordsToIndex posY posX) True grid
