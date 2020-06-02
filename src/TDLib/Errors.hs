{-# LANGUAGE DeriveAnyClass #-}

module TDLib.Errors where

import Control.Exception
import Data.Aeson
import Data.ByteString (ByteString)

data TDLibError
  = -- | Recieved an answer but the field "@extra" is not present or is not a integer
    ExtraFieldNotInt !Value
  | -- | The json value cannot be parsed into the expected data type
    UnableToParseJSON !ByteString
  | -- | The bytestring failed to be parsed into a 'Value'
    UnableToParseValue !Value
  | -- | Unknown Error
    UnknownError
  deriving (Show, Eq, Exception)
