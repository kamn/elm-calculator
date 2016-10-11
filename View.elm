module View exposing (..)
import String as String
import Html exposing (Html, text, div, button)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Msg (Number, Op),
  Operation (Addition, Subtraction, Multiplication, Division, ParensOpen, ParensClose, None, ClearLast, Calculate, Dot, Clear),
  Model)
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

-- VIEWS

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
        ParensOpen -> "("
        ParensClose -> ")"
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
      div [style alignTextRight] [text (listToString model.list)],
      div [] [calcBtn "(" (Op ParensOpen),
              calcBtn ")" (Op ParensClose),
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
              calcBtn "." (Op None),
              calcBtn "=" (Op Calculate),
              calcBtn "+" (Op Addition)]]]
