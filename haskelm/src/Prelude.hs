module Prelude
  ( IO
  , Record
  , String
  , Tagged
  , error
  ) where

import qualified "base" Prelude as P -- Import the standard Prelude
import qualified Data.HList.Record as DHR

type IO a = P.IO a

type String = P.String

type Record = DHR.Record
type Tagged = DHR.Tagged

error :: String -> a
error msg = P.error msg
