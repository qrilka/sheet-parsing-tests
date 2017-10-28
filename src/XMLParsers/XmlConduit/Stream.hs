{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module XMLParsers.XmlConduit.Stream
  ( parseCellsStream
  ) where

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
      ignoreTreeName (n_ "sheetPr")
      ignoreTreeName (n_ "dimension")
      ignoreTreeName (n_ "sheetViews")
      ignoreTreeName (n_ "sheetFormatPr")
      ignoreTreeName (n_ "cols")
      rows <- tagIgnoreAttrs (n_ "sheetData") $ many parseRow
      ignoreTreeName (n_ "printOptions")
      ignoreTreeName (n_ "pageMargins")
      ignoreTreeName (n_ "pageSetup")
      ignoreTreeName (n_ "headerFooter")
      ignoreTreeName (n_ "drawing")
      return rows
    parseRow = tagName (n_ "row") (requireAttr "r" <* ignoreAttrs) $ \rR -> do
      many $ tagName (n_ "c") ((,,) <$> requireAttr "r" <*> attr "s" <*> attr "t") $ \(rC, s, t) -> do
        f <- tagIgnoreAttrs (n_ "f") SP.content
        v <- tagNoAttr (n_ "v") SP.content
        return (parseSingleCellRefNoting $ rC, (readMay {-@ Int-}) =<< fmap T.unpack s, fromMaybe "n" t, v, f)
