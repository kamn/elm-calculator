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
type Msg = Number Float | Op Operation

type Operation = Addition | Subtraction | Multiplication | Division | None | ClearLast | Calculate


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
    value: Float,
    operation: Operation
  }

defaultExprHelper = {value = 0, operation = None}

exprToMsg: ExpressionHelper -> Msg
exprToMsg expr =
  Number expr.value

-- TODO: Doc
tailOfList: Model -> Model
tailOfList model =
  case (List.tail model.list) of
    Just rest ->
      {model | list = rest}
    Nothing ->
      {model | list = [Number 0]}

appendNumber: Msg -> Model -> Model
appendNumber msg model =
  {model | list = msg :: model.list}

ensureListWithZero: Model -> Model
ensureListWithZero model =
  if List.isEmpty model.list then
    {model | list = [Number 0]}
  else model

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

calcNewValue: Float -> Model -> Model
calcNewValue val model =
  case List.head model.list of
    Nothing -> model
    Just value ->
      case value of
        Number n ->
          model
          |> tailOfList
          |> appendNumber (Number ((n * 10) + val))
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
        ClearLast -> model
          |> tailOfList
          |> ensureListWithZero
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

calcBtn: String -> Msg -> Html Msg
calcBtn string msg =
  button [onClick msg] [text string]


view: Model -> Html Msg
view model =
  div [style centerStyle] [
    div [] [
      div [] [text (toString model)],
      div [] [text (toString model.list)],
      div [style alignTextRight] [text (listToString model.list)],
      div [] [calcBtn "(" (Op None),
              calcBtn ")" (Op None),
              calcBtn "C" (Op None),
              calcBtn "CE" (Op ClearLast)],
      div [] [calcBtn "7" (Number 7),
              calcBtn "8" (Number 8),
              calcBtn "9" (Number 9),
              calcBtn "/" (Op Division)],
      div [] [calcBtn "4" (Number 4),
              calcBtn "5" (Number 5),
              calcBtn "6" (Number 6),
              calcBtn "*" (Op Multiplication)],
      div [] [calcBtn "1" (Number 1),
              calcBtn "2" (Number 2),
              calcBtn "3" (Number 3),
              calcBtn "-" (Op Subtraction)],
      div [] [calcBtn "0" (Number 0),
              calcBtn "." (Op None),
              calcBtn "=" (Op Calculate),
              calcBtn "+" (Op Addition)]]]

main =
  App.beginnerProgram { model = baseModel, view = view, update = update }
