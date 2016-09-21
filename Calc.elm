module Calc exposing (..)

import Html exposing (Html, text, div, button)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array as Array
import String as String

-- STYLES
centerStyle : List (String, String)
centerStyle =
  [ ("display", "flex"),
    ("align-items", "center"),
    ("justify-content", "center"),
    ("height", "100vh")]

alignTextRight : List (String, String)
alignTextRight =
  [ ("text-align", "right")]

-- MODEL
type Msg = Number Int | Op Operation

type Operation = Addition | Subtraction | Multiplication | Division | None | Calculate


type alias Model =
  {
    list : List Msg,
    history : List (List Msg)
  }

baseModel: Model
baseModel =
  {
    list = [Number 0],
    history = []
  }

-- Update

type alias ExpressionHelper =
  {
    value: Int,
    operation: Operation
  }

defaultExprHelper = {value = 0, operation = None}

exprToMsg: ExpressionHelper -> Msg
exprToMsg expr =
  Number expr.value

foldExpr: Msg -> ExpressionHelper -> ExpressionHelper
foldExpr msg expr =
  case msg of
    Number n ->
      case expr.operation of
        Addition -> {expr | value = n + expr.value}
        Subtraction -> {expr | value = expr.value - n}
        Multiplication -> {expr | value = n * expr.value}
        Division -> {expr | value = n + expr.value}
        None -> {expr | value = n}
        _ -> expr
    Op o ->
      {expr | operation = o}

calcExpression: List Msg -> Msg
calcExpression list =
  list
    |> List.foldr foldExpr defaultExprHelper
    |> exprToMsg

calcNewValue: Int -> Model -> Model
calcNewValue val model =
  case List.head model.list of
    Nothing -> model
    Just value ->
      case value of
        Number n ->
          case (List.tail model.list) of
            Just rest ->
              {model | list = (Number ((n * 10) + val)) :: rest}
            Nothing ->
              {model | list = [Number 0]}
        Op o ->
          case o of
            Calculate ->
              {model | list = [(calcExpression model.list)]}
            _ ->
              {model | list = (Number val) :: model.list}

update: Msg -> Model -> Model
update msg model =
  case msg of
    Number n ->
      calcNewValue n model
    Op o ->
      case o of
        None ->
          {model | list = [Number 0]}
        Calculate ->
          {model | list = [(calcExpression model.list)], history = model.list :: model.history}
        _ ->
          {model | list = (Op o) :: model.list}


-- VIEW

msgToString: Msg -> String
msgToString msg =
  case msg of
    Number n -> toString n
    Op o ->
      case o of
        Addition -> "+"
        Subtraction -> "-"
        Multiplication -> "*"
        Division -> "/"
        _ -> ""

listToString: List Msg -> String
listToString list =
  list
    |> List.reverse
    |> List.map msgToString
    |> String.join " "

view: Model -> Html Msg
view model =
  div [style centerStyle] [
    div [] [
      div [] [text (toString model)],
      div [] [text (toString model.list)],
      div [style alignTextRight] [text (listToString model.list)],
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
              button [onClick (Op Calculate)] [text "="]]]]

main =
  App.beginnerProgram { model = baseModel, view = view, update = update }
