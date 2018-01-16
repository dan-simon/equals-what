module Model exposing (
  Model, QuestionId, Page(..),
  JSModel, encode, decode)
import ColorScheme exposing (ColorScheme)

type Page = MainPage | Question QuestionId

type alias QuestionId = List Bool

type alias Model = {seen: List QuestionId, page: Page, colorScheme: ColorScheme}

type alias JSModel = {seen: List QuestionId, page: List Bool, colorScheme: String}

encode : Model -> JSModel
encode model = {
    seen = model.seen,
    page = case model.page of
      MainPage -> []
      Question x -> x,
    colorScheme = ColorScheme.toString model.colorScheme
  }

decode : JSModel -> Model
decode model = {
    seen = model.seen,
    page = case model.page of
      [] -> MainPage
      (y :: ys) as xs -> Question xs,
    colorScheme = ColorScheme.fromString model.colorScheme
  }
