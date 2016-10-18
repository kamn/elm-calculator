module Calc exposing (..)

import Html exposing (Html, text, div, button)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array as Array
import String as String
import Debug as Debug
import View exposing (view)
import Model exposing (Msg (Number, Op, Answer),
  Operation (Addition, Subtraction, Multiplication, Division, ParensOpen, ParensClose, None, ClearLast, Calculate, Dot, Clear),
  Model)

baseModel: Model
baseModel =
  {
    list = [Model.Number 0],
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
  else
    model

-- The list fold does not work well for order of operations
-- A better alternative would be building a tree then calculating off of the tree
-- Then we could recurse down each side to get a good result

type TreeMsg =
  Empty
  | Leaf Float
  | Node Operation TreeMsg TreeMsg
  | SubGroup (List Msg) Int

insertRightMost: Float -> TreeMsg -> TreeMsg
insertRightMost n treeMsg =
  case treeMsg of
    Node oldMsg left right ->
      Node oldMsg left (insertRightMost n right)
    Leaf oldNum->
      Leaf n
    Empty ->
      Leaf n
    SubGroup l t ->
      SubGroup (Number n :: l) t

getOrderOfOpMsg: Msg -> Float
getOrderOfOpMsg msg =
  case msg of
    Op o -> getOrderOfOp o
    _ ->    0

getOrderOfOp: Operation -> Float
getOrderOfOp op =
  case op of
    ParensOpen -> 10
    ParensClose -> 10
    Subtraction -> 1
    Addition -> 1
    Multiplication -> 2
    Division -> 2
    _ -> 0

orderOfOpInsert: Operation -> TreeMsg -> TreeMsg
orderOfOpInsert o treeMsg =
  case treeMsg of
    Node oldMsg left right ->
      case right of
        SubGroup l t -> Node oldMsg left  (SubGroup ((Op o) :: l) t)
        _ ->
          if (getOrderOfOp oldMsg) >= (getOrderOfOp o) then
            Node o treeMsg Empty
          else
            Node oldMsg left (Node o right Empty)
    _ ->
      Node o treeMsg Empty

insert: Msg -> TreeMsg -> TreeMsg
insert msg treeMsg =
  case msg of
    Op o ->
      case o of
        Subtraction ->
          orderOfOpInsert o treeMsg
        Addition ->
          orderOfOpInsert o treeMsg
        Multiplication ->
          orderOfOpInsert o treeMsg
        Division ->
          orderOfOpInsert o treeMsg
        ParensOpen ->
          case treeMsg of
            Node oldMsg left right ->
              case right of
                SubGroup l n -> Node oldMsg left (SubGroup ((Op o) :: l) (n + 1))
                _ -> Node oldMsg left (SubGroup [] 0)
            _ -> Node o treeMsg Empty
        ParensClose ->
          case treeMsg of
            Node oldMsg left right ->
              case right of
                SubGroup l n ->
                  case n of
                    0 -> Node oldMsg left (List.foldr insert Empty l)
                    _ -> Node oldMsg left (SubGroup ((Op o) :: l) (n - 1))
                _ -> Node o treeMsg Empty
            _ ->
              Node o treeMsg Empty
        _ ->
          Node o treeMsg Empty
    Number n ->
      case treeMsg of
        Node oldMsg left right ->
          insertRightMost n treeMsg
        Leaf oldNum -> Leaf n
        Empty -> Leaf n
        SubGroup l t -> SubGroup (msg :: l) t -- TODO: Do I need to reverse later?
    Answer n ->
      case treeMsg of
        Node oldMsg left right ->
          insertRightMost n treeMsg
        Leaf oldNum -> Leaf n
        Empty -> Leaf n
        SubGroup l t -> SubGroup (msg :: l) t -- TODO: Do I need to reverse later?

calcTreeMap: TreeMsg -> Float
calcTreeMap treeMsg =
  case treeMsg of
    Node o left right ->
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
    Leaf f -> f
    Empty -> 0 -- IT should not reach this case
    SubGroup l t -> 0 -- IT should not reach this case

increaseDecimalOffset: Model -> Model
increaseDecimalOffset model =
  {model | decimalOffset = model.decimalOffset * 10}

calcExpression: List Msg -> Msg
calcExpression list =
    list
    |> List.foldr insert Empty
    |> Debug.log "Tree"
    |> calcTreeMap
    |> Answer

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
        Answer n ->
          {model | list = [Number val]}
        Op o ->
          {model | list = (Number val) :: model.list}

-- TODO We don't want to append an additional operation. Unless it is Subsstraction to become negative
appendOperation : Operation -> Model -> Model
appendOperation o model =
    case List.head model.list of
      Nothing -> model
      Just value ->
        case value of
          Number n -> {model | list = (Op o) :: model.list}
          Answer n -> {model | list = (Op o) :: model.list}
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
    Answer n ->
      {model | list = [Number n]}
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
        ParensOpen ->
          {model | list = (Op o) :: model.list}
        ParensClose ->
          {model | list = (Op o) :: model.list}
        _ ->
          appendOperation o model

main =
  App.beginnerProgram { model = baseModel, view = view, update = update }
