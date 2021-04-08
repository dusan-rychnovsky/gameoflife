module ListExt exposing (cartesian)

import List

cartesian : List a -> List b -> List (a,b)
cartesian xs ys =
  List.concatMap
    (\x -> List.map (\y -> (x, y)) ys)
    xs
