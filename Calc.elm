module Calc exposing (..)

import Html exposing (Html, text, div, button)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array as Array
import String as String
import Debug as Debug

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

btnStyle : List (String, String)
btnStyle =
  [ ("width", "50px"),
    ("height", "50px"),
    ("border-radius", "2px"),
    ("background-color", "rgba(158,158,158,.2)")]


-- MODEL
type Msg = Number Float | Op Operation

type Operation =
  Addition | Subtraction
  | Multiplication | Division
  | None | ClearLast | Calculate
  | Dot | Clear


type alias Model =
  {
    list : List Msg,
    history : List (List Msg),
    decimal: Bool,
    decimalOffset: Float
  }

baseModel: Model
baseModel =
  {
    list = [Number 0],
    history = [],
    decimal = False,
    decimalOffset = 10
  }

-- Update
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

-- The list fold does not work well for order of operations
-- A better alternative would be building a tree then calculating off of the tree
-- Then we could recurse down each side to get a good result

type TreeMsg =
  Empty
  | Node Msg TreeMsg TreeMsg

insertRightMost: Msg -> TreeMsg -> TreeMsg
insertRightMost msg treeMsg =
  case treeMsg of
    Node oldMsg left right ->
      Node oldMsg left (insertRightMost msg right)
    Empty ->
      Node msg Empty Empty

insert: Msg -> TreeMsg -> TreeMsg
insert msg treeMsg =
  case msg of
    Op o ->
      case o of
        Subtraction ->
          Node msg treeMsg Empty
        Addition ->
          Node msg treeMsg Empty
        Multiplication ->
          case treeMsg of
            Node oldMsg left right ->
              Debug.log "Multiplication" (Node oldMsg left (Node msg right Empty))
            _ ->
              Node msg treeMsg Empty
        Division ->
          case treeMsg of
            Node oldMsg left right ->
              Node oldMsg left (Node msg right Empty)
            _ ->
              Node msg treeMsg Empty
        _ ->
          Node msg treeMsg Empty
    Number n ->
      case treeMsg of
        Node oldMsg left right ->
          insertRightMost msg treeMsg
          -- Node oldMsg left (Node msg Empty Empty)
        Empty ->
          Node msg Empty Empty

calcTreeMap: TreeMsg -> Float
calcTreeMap treeMsg =
  case treeMsg of
    Node msg left right ->
      case msg of
        Op o ->
          case o of
            Subtraction ->
              (calcTreeMap left) - (calcTreeMap right)
            Addition ->
              (calcTreeMap left) + (calcTreeMap right)
            Multiplication ->
              (calcTreeMap left) * (calcTreeMap right)
            Division ->
              (calcTreeMap left) / (calcTreeMap right)
            _ -> -- What to do in this case?
              0
        Number n ->
          n
    Empty -> 0 -- IT should not reach this case

increaseDecimalOffset: Model -> Model
increaseDecimalOffset model =
  {model | decimalOffset = model.decimalOffset * 10}

calcExpression: List Msg -> Msg
calcExpression list =
    list
    |> List.foldr insert Empty
    |> Debug.log "Tree"
    |> calcTreeMap
    |> Number

calcNewValue: Float -> Model -> Model
calcNewValue val model =
  case List.head model.list of
    Nothing -> model
    Just value ->
      case value of
        Number n ->
          if model.decimal then
            model
            |> tailOfList
            |> appendNumber (Number (n + (val / model.decimalOffset)))
            |> increaseDecimalOffset
          else
            model
            |> tailOfList
            |> appendNumber (Number ((n * 10) + val))
        Op o ->
          {model | list = (Number val) :: model.list}

-- TODO We don't want to append an additional operation. Unless it is Subsstraction to become negative
appendOperation : Operation -> Model -> Model
appendOperation o model =
    case List.head model.list of
      Nothing -> model
      Just value ->
        case value of
          Number n ->{model | list = (Op o) :: model.list}
          Op o ->
            case o of
              Subtraction ->
                model
              _ ->
                model

update: Msg -> Model -> Model
update msg model =
  case msg of
    Number n ->
      calcNewValue n model
    Op o ->
      case o of
        None -> model
        Clear ->
          {model | list = [Number 0], decimal = False, decimalOffset = 10}
        Calculate ->
          {model | list = [(calcExpression model.list)], history = model.list :: model.history, decimal = False, decimalOffset = 10}
        ClearLast -> model
          |> tailOfList
          |> ensureListWithZero
        Dot ->
          {model | decimal = (not model.decimal)}
        _ ->
          appendOperation o model


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
  button [onClick msg, style btnStyle] [text string]


view: Model -> Html Msg
view model =
  div [style centerStyle] [
    div [] [
      div [] [text (toString model)],
      div [] [text (toString model.list)],
      div [style alignTextRight] [text (listToString model.list)],
      div [] [calcBtn "(" (Op None),
              calcBtn ")" (Op None),
              calcBtn "C" (Op Clear),
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
              calcBtn "." (Op Dot),
              calcBtn "=" (Op Calculate),
              calcBtn "+" (Op Addition)]]]

main =
  App.beginnerProgram { model = baseModel, view = view, update = update }
