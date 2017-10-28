{-# LANGUAGE OverloadedStrings #-}
module XMLParsers.Hexpat.Tree
  ( parseCellsExpatT
  ) where

import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Safe
import Text.XML.Expat.Tree
import XMLParsers.Common
import XMLParsers.Types

parseCellsExpatT :: LB.ByteString -> [CellData]
parseCellsExpatT bs = parseCells $ parseThrowing defaultParseOptions bs
  where
    parseCells ::
         UNode Text -> [((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)]
    parseCells root = concat
      [ parseRows (eChildren ch)
      | ch <- eChildren root
      , eName ch == "sheetData"
      ]
    parseRows :: [UNode Text] -> [((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)]
    parseRows children = concat [parseRow (eChildren row) | row <- children, eName row == "row"]
    parseRow children = catMaybes [parseCell c | c <- children, eName c == "c"]
    parseCell :: UNode Text -> Maybe ((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)
    parseCell c = do
      let attrs = eAttributes c
      ref <- lookup "r" attrs
      let s = lookup "s" attrs
          t = fromMaybe "n" $ lookup "t" attrs
          getText :: UNode Text -> Text
          getText n = T.concat [ t | Text t <- eChildren n ]
          (v, afterV) = case eChildren c of
            (e:rest) | eName e == "v" -> (Just $ getText e, rest)
            other -> (Nothing, other)
          f = case afterV of
            (e:rest) | eName e == "v" -> Just (getText e)
            _ -> Nothing
      return (parseSingleCellRefNoting ref, readMay =<< fmap T.unpack s, t, Nothing, f)

