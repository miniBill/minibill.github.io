{-# LANGUAGE PackageImports #-}

module CLI
  ( CLI
  , Program
  , run
  , run_
  , sandbox
  , text
  ) where

import qualified "base" Control.Concurrent as Concurrent
import qualified "base" Control.Monad      as Monad
import           "base" Prelude            (return)
import qualified "base" Prelude

data CLI msg =
  Text String

instance Prelude.Show (CLI msg) where
  show (Text t) = t

data Program flags model msg =
  Program
    (flags -> model)
    (model -> CLI msg)
    (msg -> model -> (model, IO [msg]))

run_ :: Program () model msg -> IO ()
run_ = run ()

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view update) =
  let model = init flags
   in do Prelude.print <| view model
         Monad.forever <| Concurrent.threadDelay 20

sandbox ::
     model
  -> (model -> CLI msg)
  -> (msg -> model -> model)
  -> Program () model msg
sandbox init view update =
  let init' _ = init
      update' msg model = (update msg model, return [])
   in Program init' view update'

text :: String -> CLI msg
text = Text
