module Main exposing (..)
import Browser
import Html exposing (Html, button, div, input, text, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (toInt, fromInt)
import Array exposing (Array, set, get)

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
  ,  cells : Array Bool
  }

init : Model
init =
  { running = False, numSteps = "", cells = Array.initialize (grid_width * grid_height) (always False) }

-- UPDATE

type Msg
  = ToggleCell Int Int
  | SetNumSteps String
  | Run
  | Tick

update : Msg -> Model -> Model
update msg model =
  case msg of
    ToggleCell posX posY ->
      { model | cells = flipCell (coordsToIndex posY posX) model.cells }
    
    SetNumSteps str ->
      { model | numSteps = str }

    Run ->
      { model | running = True }

    Tick ->
      { model | cells = Array.initialize (grid_width * grid_height) (always False) }

flipCell : Int -> Array Bool -> Array Bool
flipCell idx arr = Array.set idx (flipMaybeBool (Array.get idx arr)) arr

flipMaybeBool : Maybe Bool -> Bool
flipMaybeBool maybe = not(maybeBoolToBool maybe)

maybeBoolToBool : Maybe Bool -> Bool
maybeBoolToBool maybe = case maybe of
  Just True -> True
  Just False -> False
  Nothing -> False

staysAlive : (Int, Int) -> Array Bool -> Bool
staysAlive (posY, posX) grid = False

numOfAliveNeighbours : (Int, Int) -> Array Bool -> Int
numOfAliveNeighbours (posY, posX) grid = 0

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
  div [ style "float" "left", style "border" "2px black solid", style "width" "520px"] (List.map (viewGridRow model) (List.range 0 (grid_height - 1)))

viewGridRow : Model -> Int -> Html Msg
viewGridRow model posY =
  div [] (List.map (viewGridCell model posY) (List.range 0 (grid_width - 1)))

viewGridCell : Model -> Int -> Int -> Html Msg
viewGridCell model posY posX =
  div [ onClick (ToggleCell posX posY)
      , style "float" "left"
      , style "border" "1px black solid"
      , style "background-color" "gray"
      , style "width" "50px"
      , style "height" "50px" ] [ text (Debug.toString (Array.get (coordsToIndex posY posX) model.cells)) ]

-- UTIL

coordsToIndex : Int -> Int -> Int
coordsToIndex posY posX = posY * grid_width + posX
