module Model exposing (..)

type Msg = Number Float | Op Operation | Answer Float

type Operation =
  Addition
  | Subtraction
  | Multiplication
  | Division
  | ParensOpen
  | ParensClose
  | None
  | ClearLast
  | Calculate
  | Dot
  | Clear

type alias Model =
  {
    list : List Msg,
    history : List (List Msg),
    decimal: Bool,
    decimalOffset: Float
  }
