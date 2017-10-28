{-# LANGUAGE OverloadedStrings #-}
module XMLParsers.XmlConduit.Cursor
  ( parseCellsCursor
  ) where

import Data.Maybe
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Safe
import Text.XML
import Text.XML.Cursor as Cur
import XMLParsers.Common
import XMLParsers.Types
import XMLParsers.XmlConduit.Common

parseCellsCursor :: LB.ByteString -> [CellData]
parseCellsCursor bs =
  fromDocument (parseLBS_ def bs) $/ element (n_ "sheetData") &/
  element (n_ "row") >=>
  parseRow
  where
    parseRow cur = cur $/ element (n_ "c") >=> parseCell
    parseCell cur = do
      rC <- cur $| attribute "r"
      let f = listToMaybe (cur $/ element (n_ "f") &/ Cur.content)
          v = listToMaybe (cur $/ element (n_ "v") &/ Cur.content)
          s = listToMaybe (cur $| attribute "s")
          t = listToMaybe (cur $| attribute "t")
      return (parseSingleCellRefNoting rC, readMay =<< fmap T.unpack s, fromMaybe "n" t, v, f)
