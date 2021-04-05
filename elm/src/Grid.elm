module Grid exposing (Grid, create, get, set, flip, tick, isAlive, staysAlive, numOfAliveNeighbours)

import MaybeBool
import ListExt
import Array exposing (Array)

type alias Grid =
  { width : Int
  , height : Int
  , cells : Array Bool
  }

create : Int -> Int -> Grid
create w h =
  { width = w
  , height = h
  , cells = Array.initialize (w * h) (always False)
  }

get : Grid -> Int -> Int -> Maybe Bool
get grid y x =
  Array.get (coordsToIndex grid y x) grid.cells

set : Grid -> Int -> Int -> Bool -> Grid
set grid y x val =
  { grid | cells = Array.set (coordsToIndex grid y x) val grid.cells }

flip : Grid -> Int -> Int -> Grid
flip grid y x =
  set grid y x (MaybeBool.swap (get grid y x))

tick : Grid -> Grid
tick grid =
  List.foldl
    (\(y, x) acc -> set acc y x (staysAlive grid y x))
    (create grid.width grid.height)
    (positions grid)

positions : Grid -> List (Int, Int)
positions grid =
  ListExt.cartesian (List.range 0 (grid.height - 1)) (List.range 0 (grid.width - 1))

staysAlive : Grid -> Int -> Int -> Bool
staysAlive grid y x =
  let
    alive = isAlive grid y x
    num = numOfAliveNeighbours grid y x
  in
    (alive && num == 2) || num == 3

numOfAliveNeighbours : Grid -> Int -> Int -> Int
numOfAliveNeighbours grid y x =
  let
    neighbours = [
        (y - 1, x - 1), (y - 1, x), (y - 1, x + 1),
        (y, x - 1), (y, x + 1),
        (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)]
  in
    neighbours |>
    List.filter (\(py, px) -> isAlive grid py px) |>
    List.foldl (\_ acc -> acc + 1) 0

isAlive : Grid -> Int -> Int -> Bool
isAlive grid y x =
  MaybeBool.toBool (get grid y x)

coordsToIndex : Grid -> Int -> Int -> Int
coordsToIndex grid y x = y * grid.width + x
