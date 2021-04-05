module Grid exposing (create, get, set, flip, tick, isAlive, staysAlive, numOfAliveNeighbours)

import MaybeBool
import Array exposing (Array)

grid_width = 10
grid_height = 10

create : Array Bool
create =
  Array.initialize (grid_width * grid_height) (always False)

get : Array Bool -> Int -> Int -> Maybe Bool
get grid posY posX =
  Array.get (coordsToIndex posY posX) grid

set : Array Bool -> Int -> Int -> Bool -> Array Bool
set grid posY posX value =
  Array.set (coordsToIndex posY posX) value grid

flip : Array Bool -> Int -> Int -> Array Bool
flip grid posY posX =
  set grid posY posX (MaybeBool.swap (get grid posY posX))

tick : Array Bool -> Array Bool
tick grid =
  List.foldl
    (\(posY, posX) acc -> set acc posY posX (staysAlive grid posY posX))
    create
    positions

positions : List (Int, Int)
positions =
  cartesian (List.range 0 (grid_height - 1)) (List.range 0 (grid_width - 1))

cartesian : List a -> List b -> List (a,b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs

staysAlive : Array Bool -> Int -> Int -> Bool
staysAlive grid posY posX =
  let
    alive = isAlive grid posY posX
    num = numOfAliveNeighbours grid posY posX
  in
    (alive && num == 2) || num == 3

numOfAliveNeighbours : Array Bool -> Int -> Int -> Int
numOfAliveNeighbours grid posY posX =
  let
    neighbours = [
        (posY - 1, posX - 1), (posY - 1, posX), (posY - 1, posX + 1),
        (posY, posX - 1), (posY, posX + 1),
        (posY + 1, posX - 1), (posY + 1, posX), (posY + 1, posX + 1)]
  in
    neighbours |>
    List.filter (\(py, px) -> isAlive grid py px) |>
    List.foldl (\_ acc -> acc + 1) 0

isAlive : Array Bool -> Int -> Int -> Bool
isAlive grid posY posX =
  MaybeBool.toBool (get grid posY posX)

coordsToIndex : Int -> Int -> Int
coordsToIndex posY posX = posY * grid_width + posX
