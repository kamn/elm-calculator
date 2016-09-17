module Calc exposing (..)

import Html exposing (Html, text, div, button)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array as Array

centerStyle : List (String, String)
centerStyle =
  [ ("display", "flex"),
    ("align-items", "center"),
    ("justify-content", "center"),
    ("height", "100vh")]

-- MODEL
type Msg = Number Int | Op Operation

type Operation = Addition | Subtraction | Multiplication | Division | None


type alias Model =
  {
    list : List Msg
  }

baseModel: Model
baseModel =
  {
    list = [Number 0]
  }

-- Update

calcNewValue: Int -> Model -> Model
calcNewValue val model =
  case List.head model.list of
    Nothing -> model
    Just value ->
      case value of
        Number n ->
          case (List.tail model.list) of
            Just rest ->
              {list = (Number ((n * 10) + val)) :: rest}
            Nothing ->
              {list = [Number 0]}
        Op o ->
          case o of
            Addition ->
              {list = [Number 0]}
            Subtraction ->
              model
            Multiplication ->
              model
            Division ->
              model
            None ->
              {list = [Number 0]}

update: Msg -> Model -> Model
update msg model =
  case msg of
    Number n ->
      calcNewValue n model
    Op o ->
      case o of
        None ->
          {list = [Number 0]}
        _ ->
          {list = (Op o) :: model.list}


-- VIEW

view: Model -> Html Msg
view model =
  div [style centerStyle] [
    div [] [
      div [] [text (toString model)],
      div [] [text (toString model.list)],
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
              button [onClick (Op Division)] [text "/"]],
      div [] [button [onClick (Op None)] [text "C"],
              button [onClick (Op None)] [text "="]]]]

main =
  App.beginnerProgram { model = baseModel, view = view, update = update }
