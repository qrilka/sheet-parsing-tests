{-# LANGUAGE OverloadedStrings #-}
module XMLParsers.XmlConduit.Common
  ( n_
  ) where

import Data.Text (Text)
import Text.XML

n_ :: Text -> Name
n_ x = Name
  { nameLocalName = x
  , nameNamespace = Just "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
  , namePrefix = Just "n"
  }
