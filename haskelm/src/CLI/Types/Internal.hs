{-# LANGUAGE ImplicitPrelude #-}

module CLI.Types.Internal
  ( Focus(..)
  ) where

data Focus
  = RowChild Int Focus
  | ColumnChild Int Focus
  | This
