import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Model exposing (
  Model, QuestionId, Page(..),
  JSModel, encode, decode)
import LocalStorage exposing (setStorage)
import Data exposing (questionName, questionText, correctAnswers, allQuestions)
import Utils exposing (listDefault, getDefault, applyToEach, sortBoolLists)
import Regex exposing (Regex)

main : Program (Maybe JSModel) Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

initModel : Model
initModel = {seen = [], page = MainPage}

init : Maybe JSModel -> (Model, Cmd Msg)
init savedModel = (
  Maybe.withDefault initModel (Maybe.map decode savedModel),
  Cmd.none)

type Msg = TryAnswer QuestionId String | Goto Page

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TryAnswer q answer -> if
      not (hasSeen model q) &&
      answer == getDefault correctAnswers "0" q
      then
        let
          newModel = {model | seen = q :: model.seen}
        in
          (newModel, setStorage (encode newModel))
      else (model, Cmd.none)
    Goto p ->
      let
        newModel = {model | page = p}
      in
        (newModel, setStorage (encode newModel))

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

getName : QuestionId -> String
getName x = getDefault questionName "not named yet" x

linkTo : Page -> String -> Html Msg
linkTo p s
  = a [style [("color", "blue")], onClick (Goto p)]
  [text s]

showQuestionLink : QuestionId -> Html Msg
showQuestionLink x = linkTo (Question x) (getName x)

intersperseBreaks : List (Html Msg) -> List (Html Msg)
intersperseBreaks = List.intersperse (br [] [])

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

replaceNewlinesWithBreaks : String -> List (Html Msg)
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
findText = (\x -> Debug.log x x) >> Regex.split Regex.All preRegex

toPre : String -> Html Msg
toPre x = pre [] [text x]

interspersePre : List x -> x -> x -> List (List x) -> List x
interspersePre pre useless err texts = List.map2 (::) (useless :: pre) texts
  |> List.concat |> List.tail |> Maybe.withDefault [err]

textToHtml : String -> List (Html Msg)
textToHtml x =
  let
    pre = findPre x
    texts = findText x
    uselessItemToSatisfyCompiler = br [] []
    err = text "This case is literally impossible if this function is being used correctly."
  in
    texts |> List.map replaceNewlinesWithBreaks
    |> interspersePre (List.map toPre pre) uselessItemToSatisfyCompiler err

getQuestionTextHTML : QuestionId -> List (Html Msg)
getQuestionTextHTML q =
  q |> getDefault questionText "not written yet"
  |> textToHtml

view : Model -> Html Msg
view model =
  case model.page of
    MainPage ->
      div []
        ([ h1 [] [ text "= ?" ] ] ++ solved model ++ available model)
    Question q ->
      div [] (
        [
          h1 [] [ text "= ?" ],
          backToMain,
          h2 [] [ text (getName q) ],
          h3 [] [ text (if hasSeen model q then "Solved" else "Unsolved") ]
        ] ++ getQuestionTextHTML q ++ [
          br [] [],
          input [ placeholder "Answer?", onInput (TryAnswer q) ] []
        ]
        )
