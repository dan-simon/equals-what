module ColorScheme exposing (ColorScheme(..),
  toString, fromString, toDisplayString, getStyle, colorSchemes, toCssStyle)

import Html exposing (..)

type ColorScheme = GreenOnBlack | BlackOnWhite

colorSchemes : List ColorScheme
colorSchemes = [GreenOnBlack, BlackOnWhite]

toString : ColorScheme -> String
toString s = case s of
  GreenOnBlack -> "green-on-black"
  BlackOnWhite -> "black-on-white"

fromString : String -> ColorScheme
fromString s = if s == "black-on-white" then BlackOnWhite else GreenOnBlack

toDisplayString : ColorScheme -> String
toDisplayString s = case s of
  GreenOnBlack -> "green on black"
  BlackOnWhite -> "black on white"

getStyle : ColorScheme -> List (String, String)
getStyle s = case s of
  GreenOnBlack -> [("color", "green"), ("background-color", "black")]
  BlackOnWhite -> [("color", "black"), ("background-color", "white")]

surroundCss : String -> String
surroundCss s = "body {" ++ s ++ "}"

toCssStyle : List (String, String) -> Html msg
toCssStyle l = l |> List.map (\(a, b) -> a ++ ": " ++ b) |> String.join "; "
  |> surroundCss |> text
