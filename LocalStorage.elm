port module LocalStorage exposing (setStorage)

import Model exposing (JSModel)

port setStorage : JSModel -> Cmd msg
