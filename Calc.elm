module Calc exposing (..)

import Html exposing (text, div, button)

main =
  div [] [
    text "Calculator",
    div [] [button [] [text "1"],
            button [] [text "2"],
            button [] [text "3"]],
    div [] [button [] [text "4"],
            button [] [text "5"],
            button [] [text "6"]],
    div [] [button [] [text "7"],
            button [] [text "8"],
            button [] [text "9"]],
    div [] [button [] [text "0"]]]
