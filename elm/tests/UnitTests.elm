module UnitTests exposing (..)

import Main exposing (..)
import Grid

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Array exposing (Array, set)
import List exposing (foldl)

suite : Test
suite =
  describe "unit tests"
    [ describe "staysAlive"
      [ test "a live cell with fewer than two live neighbours dies, as if by underpopulation" <|
        \_ -> (Grid.staysAlive (populateGrid [(3, 3)]) 3 3) |> Expect.equal False
      , test "a live cell with two live neighbours lives on to the next generation" <|
        \_ -> (Grid.staysAlive (populateGrid [(3, 3), (3, 2), (3, 4)]) 3 3) |> Expect.equal True
      , test "a live cell with three live neighbours lives on to the next generation" <|
        \_ -> (Grid.staysAlive (populateGrid [(3, 3), (3, 2), (3, 4), (2, 3)]) 3 3) |> Expect.equal True
      , test "a live cell with more than three live neighbours dies, as if by overpopulation" <|
        \_ -> (Grid.staysAlive (populateGrid [(3, 3), (3, 2), (3, 4), (2, 3), (4, 4)]) 3 3) |> Expect.equal False
      , test "a dead cell with exactly three live neighbours becomes a live cell, as if by reproduction" <|
        \_ -> (Grid.staysAlive (populateGrid [(3, 2), (3, 4), (2, 3)]) 3 3) |> Expect.equal True
      , test "a dead cell with two neighbours remains dead" <|
        \_ -> (Grid.staysAlive (populateGrid [(3, 2), (3, 4)]) 3 3) |> Expect.equal False
      ]
    , describe "numOfAliveNeighbours"
      [ test "0 alive neighbours" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(3, 3)]) 3 3) |> Expect.equal 0
      , test "1 alive neighbour" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(3, 3), (3, 4)]) 3 3) |> Expect.equal 1
      , test "2 alive neighbours" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(3, 3), (3, 4), (4, 2)]) 3 3) |> Expect.equal 2
      , test "3 alive neighbours" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(3, 3), (3, 4), (4, 2), (2, 2)]) 3 3) |> Expect.equal 3
      , test "4 alive neighbours" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(3, 3), (3, 4), (4, 2), (2, 2), (2, 3)]) 3 3) |> Expect.equal 4
      , test "5 alive neighbours" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(3, 3), (3, 4), (4, 2), (2, 2), (2, 3), (4, 4)]) 3 3) |> Expect.equal 5
      , test "6 alive neighbours" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(3, 3), (3, 4), (4, 2), (2, 2), (2, 3), (4, 4), (4, 3)]) 3 3) |> Expect.equal 6
      , test "7 alive neighbours" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(3, 3), (3, 4), (4, 2), (2, 2), (2, 3), (4, 4), (4, 3), (2, 4)]) 3 3) |> Expect.equal 7
      , test "8 alive neighbours" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(3, 3), (3, 4), (4, 2), (2, 2), (2, 3), (4, 4), (4, 3), (2, 4), (3, 2)]) 3 3) |> Expect.equal 8
      , test "3 alive neighbours in the corner of the grid" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(0, 0), (0, 1), (1, 0), (1, 1)]) 0 0) |> Expect.equal 3
      , test "3 alive neighbours of a dead cell" <|
        \_ -> (Grid.numOfAliveNeighbours (populateGrid [(3, 4), (4, 2), (2, 2)]) 3 3) |> Expect.equal 3
      ]
    , describe "isAlive"
      [ test "alive cell" <|
        \_ -> (Grid.isAlive (populateGrid [(3, 3)]) 3 3) |> Expect.equal True
      , test "dead cell" <|
        \_ -> (Grid.isAlive (populateGrid [(4, 4)]) 3 3) |> Expect.equal False
      ]
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
