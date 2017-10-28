{-# LANGUAGE OverloadedStrings #-}
module XMLParsers.XmlConduit.Stream
  ( parseCellsStream
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString.Lazy as LB
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import qualified Data.Text as T
import Safe
import qualified Text.XML.Stream.Parse as SP
import Text.XML.Stream.Parse hiding (parseLBS)
import XMLParsers.Common
import XMLParsers.Types
import XMLParsers.XmlConduit.Common

parseCellsStream :: (MonadThrow m) =>
     LB.ByteString
  -> m (Maybe [[CellData]])
parseCellsStream bs =
  CL.sourceList (LB.toChunks bs) =$= parseBytes def $$ SP.force "cells required" parseCells
  where
    parseCells = tagIgnoreAttrs (n_ "worksheet") $ do
      void $ ignoreTreeName (n_ "sheetPr")
      void $ ignoreTreeName (n_ "dimension")
      void $ ignoreTreeName (n_ "sheetViews")
      void $ ignoreTreeName (n_ "sheetFormatPr")
      void $ ignoreTreeName (n_ "cols")
      rows <- tagIgnoreAttrs (n_ "sheetData") $ many parseRow
      void $ ignoreTreeName (n_ "printOptions")
      void $ ignoreTreeName (n_ "pageMargins")
      void $ ignoreTreeName (n_ "pageSetup")
      void $ ignoreTreeName (n_ "headerFooter")
      void $ ignoreTreeName (n_ "drawing")
      return rows
    parseRow = tagIgnoreAttrs (n_ "row") $
      many $ tagName (n_ "c") ((,,) <$> requireAttr "r" <*> attr "s" <*> attr "t") $ \(rC, s, t) -> do
        f <- tagIgnoreAttrs (n_ "f") SP.content
        v <- tagNoAttr (n_ "v") SP.content
        return (parseSingleCellRefNoting rC, readMay =<< fmap T.unpack s, fromMaybe "n" t, v, f)
