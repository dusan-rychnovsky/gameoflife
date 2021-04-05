module Grid exposing (create)

import Array exposing (Array)

grid_width = 10
grid_height = 10

create : Array Bool
create =
  Array.initialize (grid_width * grid_height) (always False)
