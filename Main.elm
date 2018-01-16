import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Model exposing (
  Model, QuestionId, Page(..),
  JSModel, encode, decode)
import LocalStorage exposing (setStorage)
import Data exposing (questionName, questionText, correctAnswers, allQuestions)
import TextToHtml exposing (textToHtml)
import ColorScheme exposing (ColorScheme)
import Utils exposing (listDefault, getDefault, applyToEach,
  sortBoolLists, intersperseBreaks)

main : Program (Maybe JSModel) Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

initModel : Model
initModel = {seen = [], page = MainPage, colorScheme = ColorScheme.GreenOnBlack}

init : Maybe JSModel -> (Model, Cmd Msg)
init savedModel = (
  Maybe.withDefault initModel (Maybe.map decode savedModel),
  Cmd.none)

type Msg = TryAnswer QuestionId String
  | Goto Page
  | SwitchColorScheme ColorScheme

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let newModel =
    case msg of
      TryAnswer q answer -> if
        not (hasSeen model q) &&
        getDigits answer == getDefault correctAnswers "0" q
        then
          {model | seen = q :: model.seen}
        else
          model
      Goto p ->
        {model | page = p}
      SwitchColorScheme cs ->
        {model | colorScheme = cs}
  in
    (newModel, setStorage (encode newModel))

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

getDigits : String -> String
getDigits x = String.filter isDigit x

isDigit : Char -> Bool
isDigit c = List.member c (String.toList "1234567890")

getName : QuestionId -> String
getName x = getDefault questionName "not named yet" x

linkTo : Page -> String -> Html Msg
linkTo p s
  = a [style [("color", "blue")], onClick (Goto p)]
  [text s]

showQuestionLink : QuestionId -> Html Msg
showQuestionLink x = linkTo (Question x) (getName x)

questionLinks : List QuestionId -> Html Msg -> List (Html Msg)
questionLinks l default
  = l |> sortBoolLists |> List.map showQuestionLink
  |> intersperseBreaks
  |> (\x -> listDefault x [default])

solved : Model -> List (Html Msg)
solved model = (h2 [] [text "Solved:"]) ::
  questionLinks model.seen solvedDefault

solvedDefault : Html Msg
solvedDefault = text "None yet."

available : Model -> List (Html Msg)
available model = (h2 [] [text "Available:"]) ::
  questionLinks (getAvailable model) availableDefault

availableDefault : Html Msg
availableDefault = text "You've solved everything!"

showAvailable : QuestionId -> Html Msg
showAvailable x = text (getDefault questionName "not named yet" x)

hasSeen : Model -> QuestionId -> Bool
hasSeen model x = List.member x model.seen

backToMain : Html Msg
backToMain = linkTo MainPage "Back to main page"

getAvailable : Model -> List QuestionId
getAvailable model =
  allQuestions
  |> List.filter (\x -> not (hasSeen model x) &&
    List.all (\y -> y == x || hasSeen model y)
    (applyToEach (always False) x))

getQuestionTextHTML : QuestionId -> List (Html Msg)
getQuestionTextHTML q =
  q |> getDefault questionText "not written yet"
  |> textToHtml

switchColorSchemeHtml : ColorScheme -> Html Msg
switchColorSchemeHtml cs = a
  [style [("color", "blue")], onClick (SwitchColorScheme cs)]
  [text ("Set style to " ++ ColorScheme.toDisplayString cs)]

switchColorSchemeAllHtml : ColorScheme -> List (Html Msg)
switchColorSchemeAllHtml cs = ColorScheme.colorSchemes
  |> List.filter ((/=) cs)
  |> List.map switchColorSchemeHtml

whatIsLeft : Model -> String
whatIsLeft model = let numLeft = List.length (getAvailable model)
  in
    if numLeft == 0
    then "You've solved everything!"
    else "You have " ++ toString numLeft ++ " available unsolved questions. " ++
      " Go back to the main page to see them."

solvedElement : Model -> QuestionId -> List (Html Msg)
solvedElement model q = if hasSeen model q
  then [h3 [] [text "Solved"], text (whatIsLeft model), br [] [], br [] []]
  else [h3 [] [text "Unsolved"]]

view : Model -> Html Msg
view model =
  case model.page of
    MainPage ->
      div [] (
        [
          node "style" [] [model.colorScheme
          |> ColorScheme.getStyle |> ColorScheme.toCssStyle],
          h1 [] [ text "= ?" ]
        ] ++ solved model ++ available model ++ (
          br [] [] :: br [] [] :: switchColorSchemeAllHtml model.colorScheme))
    Question q ->
      div [] (
        [
          node "style" [] [model.colorScheme
          |> ColorScheme.getStyle |> ColorScheme.toCssStyle],
          h1 [] [ text "= ?" ],
          backToMain,
          h2 [] [ text (getName q) ]
        ] ++ solvedElement model q ++ getQuestionTextHTML q ++ [
          br [] [],
          input [ placeholder "Answer?", onInput (TryAnswer q) ] [],
          br [] [],
          br [] []] ++ switchColorSchemeAllHtml model.colorScheme)
