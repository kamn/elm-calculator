module Calc exposing (..)

import Html exposing (Html, text, div, button)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

centerStyle : List (String, String)
centerStyle =
  [ ("display", "flex"),
    ("align-items", "center"),
    ("justify-content", "center"),
    ("height", "100vh")]

-- MODEL
type alias Model = Int

model: Model
model =
  0

-- Update

type alias Msg = Int

update: Msg -> Model -> Model
update msg model =
  msg


-- VIEW

view: Model -> Html Msg
view model =
  div [style centerStyle] [
    div [] [
      text "Calculator",
      div [] [text (toString model)],
      div [] [button [onClick 1] [text "1"],
              button [onClick 2] [text "2"],
              button [onClick 3] [text "3"]],
      div [] [button [onClick 4] [text "4"],
              button [onClick 5] [text "5"],
              button [onClick 6] [text "6"]],
      div [] [button [onClick 7] [text "7"],
              button [onClick 8] [text "8"],
              button [onClick 9] [text "9"]],
      div [] [button [onClick 0] [text "0"]]]]

main =
  App.beginnerProgram { model = model, view = view, update = update }
