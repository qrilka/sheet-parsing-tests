{-# LANGUAGE OverloadedStrings #-}
module XMLParsers.Hexml
  ( parseCellsHexml
  ) where

import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Safe
import Text.XML.Hexml
import XMLParsers.Common
import XMLParsers.Types

parseCellsHexml :: ByteString -> [CellData]
parseCellsHexml bs = parseRows . findSheetData . fromRight $ parse bs
  where
    fromRight (Left err) = error $ "Can't parse: " ++ show err
    fromRight (Right root) = root
    findSheetData n =
      headNote "no sheetData" $ childrenBy (findWorksheet n) "sheetData"
    findWorksheet n =
      headNote "no ws found" $
      filter (\ch -> name ch == "worksheet") (nodesUpTo n 1)
    nodesUpTo :: Node -> Int -> [Node]
    nodesUpTo n lvl
      | lvl == 0 = [n]
      | otherwise = n:concatMap (\ch -> nodesUpTo ch (lvl - 1)) (children n)
    parseRows sheetData = concatMap parseRow $ childrenBy sheetData "row"
    parseRow :: Node -> [CellData]
    parseRow r = mapMaybe parseCell $ childrenBy r "c"
    parseCell c = do
      ref <- attrValBy c "r"
      let s = attrValBy c "s"
          t = attrValBy c "t"
          v = textContents <$> listToMaybe (childrenBy c "v")
          f = textContents <$> listToMaybe (childrenBy c "f")
      return
        ( parseSingleCellRefNoting ref
        , readMay =<< fmap T.unpack s
        , fromMaybe "n" t
        , v
        , f)
    attrValBy node nm = T.decodeUtf8 . attributeValue <$> attributeBy node nm
    textContents node = T.concat [T.decodeUtf8 t | Left t <- contents node]
