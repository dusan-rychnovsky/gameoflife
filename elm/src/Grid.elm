module Grid exposing (create, get, set, flip)

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

coordsToIndex : Int -> Int -> Int
coordsToIndex posY posX = posY * grid_width + posX
