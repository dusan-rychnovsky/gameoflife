module MaybeBool exposing (toBool, swap)

swap : Maybe Bool -> Bool
swap maybe =
  not(toBool maybe)

toBool : Maybe Bool -> Bool
toBool maybe =
  Maybe.withDefault False maybe
