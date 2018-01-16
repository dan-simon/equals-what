module Utils exposing (listDefault, getDefault, applyToEach,
  boolToInt, sortBoolLists, intersperseBreaks)
import EveryDict exposing (EveryDict)
import Html exposing (Html, br)

listDefault : List x -> List x -> List x
listDefault xs ys = case xs of
  [] -> ys
  z :: zs -> xs

getDefault : EveryDict x y -> y -> x -> y
getDefault d default x = Maybe.withDefault default (EveryDict.get x d)

applyToEach : (x -> x) -> List x -> List (List x)
applyToEach f =
  List.foldr (\x (y, z) -> (x :: y, (f x :: y) :: List.map ((::) x) z)) ([], [])
  >> Tuple.second

boolToInt : Bool -> Int
boolToInt b = if b then 1 else 0

sortBoolLists : List (List Bool) -> List (List Bool)
sortBoolLists = List.sortBy (List.map boolToInt)

intersperseBreaks : List (Html msg) -> List (Html msg)
intersperseBreaks = List.intersperse (br [] [])
