module TextToHtml exposing (textToHtml)

import Html exposing (..)

import Regex exposing (Regex)

import Utils exposing (intersperseBreaks)

replaceNewlinesWithBreaks : String -> List (Html msg)
replaceNewlinesWithBreaks s = s |> String.split "\n" |> List.map text
   |> intersperseBreaks

preRegex : Regex
preRegex = Regex.regex "<pre>\n[^<>]+\n</pre>"

innerPreRegex : Regex
innerPreRegex = Regex.regex "<pre>\n([^<>]+)\n</pre>"

findPreContent : String -> String
findPreContent = Regex.find (Regex.AtMost 1) innerPreRegex >> List.head
  >> Maybe.andThen (.submatches >> List.head) >> Maybe.andThen (\x -> x)
  >> Maybe.withDefault "oops?"

findPre : String -> List String
findPre = Regex.find Regex.All preRegex >> List.map (.match >> findPreContent)

findText : String -> List String
findText = Regex.split Regex.All preRegex

toPre : String -> Html msg
toPre x = pre [] [text x]

interspersePre : List x -> x -> x -> List (List x) -> List x
interspersePre pre useless err texts = List.map2 (::) (useless :: pre) texts
  |> List.concat |> List.tail |> Maybe.withDefault [err]

textToHtml : String -> List (Html msg)
textToHtml x =
  let
    pre = findPre x
    texts = findText x
    uselessItemToSatisfyCompiler = br [] []
    err = text "This case is literally impossible if this function is being used correctly."
  in
    texts |> List.map replaceNewlinesWithBreaks
    |> interspersePre (List.map toPre pre) uselessItemToSatisfyCompiler err
