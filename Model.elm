module Model exposing (
  Model, QuestionId, Page(..),
  JSModel, encode, decode)

type Page = MainPage | Question QuestionId

type alias QuestionId = List Bool

type alias Model = {seen: List QuestionId, page: Page}

type alias JSModel = {seen: List QuestionId, page: List Bool}

encode : Model -> JSModel
encode model = {model | page = case model.page of
    MainPage -> []
    Question x -> x
  }

decode : JSModel -> Model
decode model = {model | page = case model.page of
    [] -> MainPage
    (y :: ys) as xs -> Question xs
  }
