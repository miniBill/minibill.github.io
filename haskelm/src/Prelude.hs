{-# LANGUAGE PackageImports #-}

module Prelude
  ( Label
  , IO
  , Record
  , String
  , Tagged
  , error
  ) where

import qualified Data.HList.Record as DHR
import           GHC.DataLabels
import qualified "base" Prelude    as P

type IO a = P.IO a

type String = P.String

type Record = DHR.Record

type Label = DHR.Label

type Tagged = DHR.Tagged

error :: String -> a
error = P.error
