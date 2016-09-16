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
type Operation = Addition | Subtraction | Multiplication | Division | None

type alias Model =
  {
    value : Int,
    operation : Operation
  }

model: Model
model =
  {
    value = 0
  , operation = None
  }

-- Update
type Msg = Number Int | Op Operation

update: Msg -> Model -> Model
update msg model =
  case msg of
    Number n ->
      {model | value = n}
    Op o ->
      {model | operation = o}


-- VIEW

view: Model -> Html Msg
view model =
  div [style centerStyle] [
    div [] [
      text "Calculator",
      div [] [text (toString model)],
      div [] [button [onClick (Number 1)] [text "1"],
              button [onClick (Number 2)] [text "2"],
              button [onClick (Number 3)] [text "3"]],
      div [] [button [onClick (Number 4)] [text "4"],
              button [onClick (Number 5)] [text "5"],
              button [onClick (Number 6)] [text "6"]],
      div [] [button [onClick (Number 7)] [text "7"],
              button [onClick (Number 8)] [text "8"],
              button [onClick (Number 9)] [text "9"]],
      div [] [button [onClick (Number 0)] [text "0"]],
      div [] [button [onClick (Op Addition)] [text "+"],
              button [onClick (Op Subtraction)] [text "-"],
              button [onClick (Op Multiplication)] [text "*"],
              button [onClick (Op Division)] [text "/"]]]]

main =
  App.beginnerProgram { model = model, view = view, update = update }
