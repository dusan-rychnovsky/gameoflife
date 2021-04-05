module MaybeBool exposing (toBool, swap)

swap : Maybe Bool -> Bool
swap maybe =
  not(toBool maybe)

toBool : Maybe Bool -> Bool
toBool maybe =
  case maybe of
    Just True -> True
    Just False -> False
    Nothing -> False