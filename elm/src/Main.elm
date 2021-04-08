module Main exposing (..)

import Grid exposing (Grid)
import Browser
import Html exposing (Html, button, div, input, text, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (toInt, fromInt)
import Array exposing (Array, set, get)
import List exposing (filter, foldl)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- CONST

grid_width = 10
grid_height = 10

-- MODEL

type alias Model = 
  {  running : Bool
  ,  numSteps : String
  ,  grid : Grid
  }

init : Model
init =
  { running = False, numSteps = "", grid = Grid.create grid_height grid_width }

-- UPDATE

type Msg
  = ToggleCell Int Int
  | SetNumSteps String
  | Run
  | Stop
  | Tick

update : Msg -> Model -> Model
update msg model =
  case msg of
    ToggleCell y x ->
      { model | grid = Grid.flip model.grid y x }
    
    SetNumSteps str ->
      let
        stepsNo = toInt str
      in
        if stepsNo >= 0 then
          { model | numSteps = String.fromInt stepsNo }
        else
          { model | numSteps = "0" }

    Run ->
      { model | running = True }

    Stop ->
      { model | running = False }

    Tick ->
      let
        stepsNo = toInt model.numSteps
      in
        case model.running of
          True ->
            if stepsNo > 0 then
              { model | grid = Grid.tick model.grid, numSteps = String.fromInt (stepsNo - 1)}
            else
              { model | running = False}
          False -> model

toInt : String -> Int
toInt value =
  Maybe.withDefault 0 (String.toInt value)

-- VIEW

view : Model -> Html Msg
view model =
  div [ style "padding" "100px" ] [ viewGrid model, viewMenu model ]

viewMenu : Model -> Html Msg
viewMenu model =
  div [ style "float" "left"
      , style "border" "0px black solid"
      , style "width" "250px"
      , style "margin" "0px 0px 0px 50px"
      , style "padding" "5px"
      , style "text-align" "center"
      , style "font-weight" "bold" ]
    [ label [] [ text "Steps: " ]
    , input [ type_ "number", value model.numSteps, onInput SetNumSteps, style "margin-bottom" "10px" ] [ ]
    , button [ onClick Run, style "font-weight" "600" ] [ text "Run" ]
    ]
  
viewGrid : Model -> Html Msg
viewGrid model =
  div [ style "float" "left", style "border" "2px black solid", style "width" "520px"] (List.map (viewGridRow model) (List.range 0 (model.grid.height - 1)))

viewGridRow : Model -> Int -> Html Msg
viewGridRow model y =
  div [] (List.map (viewGridCell model y) (List.range 0 (model.grid.width - 1)))

viewGridCell : Model -> Int -> Int -> Html Msg
viewGridCell model y x =
  div [ onClick (ToggleCell y x)
      , style "float" "left"
      , style "border" "1px black solid"
      , style "background-color" (backgroundColor model y x)
      , style "width" "50px"
      , style "height" "50px" ] []

backgroundColor : Model -> Int -> Int -> String
backgroundColor model y x =
  case (Grid.get model.grid y x) of
    Just True -> "green"
    _ -> "gray"
