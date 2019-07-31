module CLI
  ( CLI
  , sandbox
  , text
  ) where

data CLI msg =
  Todo

sandbox :: Record '[Tagged "init" (flags -> model), Tagged "update" (msg -> model -> model), Tagged "view" (model -> CLI msg)] -> IO ()
sandbox args = error "Todo"

text :: String -> CLI msg
text msg = error "Todo"
