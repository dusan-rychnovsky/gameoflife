module Grid exposing (create, get)

import Array exposing (Array)

grid_width = 10
grid_height = 10

create : Array Bool
create =
  Array.initialize (grid_width * grid_height) (always False)

get : Array Bool -> Int -> Int -> Maybe Bool
get grid posY posX =
  Array.get (coordsToIndex posY posX) grid

coordsToIndex : Int -> Int -> Int
coordsToIndex posY posX = posY * grid_width + posX
