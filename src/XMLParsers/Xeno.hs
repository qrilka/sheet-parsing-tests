{-# LANGUAGE OverloadedStrings #-}
module XMLParsers.Xeno
  ( parseCellsXeno
  ) where

import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Safe
import Xeno.DOM
import XMLParsers.Common
import XMLParsers.Types

parseCellsXeno :: ByteString -> [CellData]
parseCellsXeno bs = parseRows . findSheetData . fromRight $ parse bs
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
      ref <- attrTextBy c "r"
      let s = attrTextBy c "s"
          t = attrTextBy c "t"
          v = textContents <$> listToMaybe (childrenBy c "v")
          f = textContents <$> listToMaybe (childrenBy c "f")
      return
        ( parseSingleCellRefNoting ref
        , readMay =<< fmap T.unpack s
        , fromMaybe "n" t
        , v
        , f)
    attrTextBy node nm = T.decodeUtf8 <$> attributeBy node nm
    attributeBy node nm = lookup nm (attributes node)
    childrenBy node nm = filter (\ch -> name ch == nm) $ children node
    textContents node = T.concat [T.decodeUtf8 t | Text t <- contents node]
