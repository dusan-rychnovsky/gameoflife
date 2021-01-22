module Main exposing (..)
import Browser
import Html exposing (Html, button, div, input, text, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (toInt, fromInt)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- CONST

grid_width = 10
grid_height = 10

-- MODEL

type alias Model = String

init : Model
init =
  ""

-- UPDATE

type Msg
  = SetNumSteps String
  | Run

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetNumSteps str ->
      str

    Run ->
      model

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
    , input [ type_ "number", value model, onInput SetNumSteps, style "margin-bottom" "10px" ] [ ]
    , button [ onClick Run, style "font-weight" "600" ] [ text "Run" ]
    ]
  
viewGrid : Model -> Html Msg
viewGrid model =
  div [ style "float" "left", style "border" "2px black solid", style "width" "520px"] (List.repeat grid_height (viewGridRow model))

viewGridRow : Model -> Html Msg
viewGridRow model =
  div [] (List.repeat grid_width (viewGridCell model))

viewGridCell : Model -> Html Msg
viewGridCell model =
  div [ onClick (SetNumSteps "3")
      , style "float" "left"
      , style "border" "1px black solid"
      , style "background-color" "gray"
      , style "width" "50px"
      , style "height" "50px" ] [ text model ]
