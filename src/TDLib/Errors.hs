{-# LANGUAGE DeriveAnyClass #-}

module TDLib.Errors where

import Control.Exception
import Data.Aeson
import Data.ByteString (ByteString)

data TDLibError
  = ExtraFieldNotInt !Value
  | UnableToParseJSON !ByteString
  | UnableToParseValue !Value
  | UnknownError
  deriving (Show, Eq, Exception)
