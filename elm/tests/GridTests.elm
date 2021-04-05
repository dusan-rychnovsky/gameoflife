module GridTests exposing (..)

import Main exposing (..)
import Grid exposing (Grid)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Array exposing (Array, set)
import List exposing (foldl)

suite : Test
suite =
  let
    grid = Grid.flipAll (Grid.create 4 4) [(0, 0), (0, 3), (1, 1), (1, 2), (1, 3), (2, 2), (3, 0)]
  in
    describe "unit tests"
      [ describe "staysAlive"
        [ test "a live cell with zero live neighbours dies" <|
          \_ -> Expect.equal False (Grid.staysAlive grid 3 0)
        , test "a live cell with one live neighbours dies" <|
          \_ -> Expect.equal False (Grid.staysAlive grid 0 0)
        , test "a live cell with two live neighbours lives" <|
          \_ -> Expect.equal True (Grid.staysAlive grid 0 3)
        , test "a live cell with three live neighbours lives" <|
          \_ -> Expect.equal True (Grid.staysAlive grid 1 1)
        , test "a live cell with four live neighbours dies" <|
          \_ -> Expect.equal False (Grid.staysAlive grid 1 2)
        , test "a dead cell with three live neighbours becomes live" <|
          \_ -> Expect.equal True (Grid.staysAlive grid 0 1)
        , test "a dead cell with two live neighbours remains dead" <|
          \_ -> Expect.equal False (Grid.staysAlive grid 3 1)
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

populateGrid : List (Int, Int) -> Grid
populateGrid coords =
  let
    emptyGrid = Array.initialize (grid_width * grid_height) (always False)
  in
    List.foldl markCellAlive (Grid.create grid_height grid_width) coords

markCellAlive : (Int, Int) -> Grid -> Grid
markCellAlive (posY, posX) grid =
  Grid.set grid posY posX True