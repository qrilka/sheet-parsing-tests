{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Exception
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Either.Extra (fromRight)
import Data.Ix (inRange)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.XML.Types
import Safe
import Text.XML
import qualified Text.XML.Expat.SAX as ES
import qualified Text.XML.Expat.Tree as ET
import Text.XML.Cursor as Cur
import qualified Text.XML.Stream.Parse as SP
import Text.XML.Stream.Parse hiding (parseLBS)

main :: IO ()
main = do
  -- use 6000x26.xml if large file is needed
  sheetBs <- LB.readFile "testSheet.xml"
  let fromEx :: Either SomeException a -> a
      fromEx (Left ex) = error $ "parsing failed: " ++ show ex
      fromEx (Right a) = a
  defaultMain
    [ bgroup
        "cell parsing"
        [ bench "with xml-conduit/stream" $
          nf (fromEx . parseCellsStream) sheetBs
        , bench "with xml-conduit/cursor" $
          nf parseCellsCursor sheetBs
        , bench "with hexpat/tree" $
          nf parseCellsExpatT sheetBs
        , bench "with hexpat/SAX" $
          nf parseCellsExpatS sheetBs
        ]
    ]

parseCellsStream :: (MonadThrow m) =>
     LB.ByteString
  -> m (Maybe [[((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)]])
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
        return (parseSingleCellRefNoting $ rC, (readMay @ Int) =<< fmap T.unpack s, fromMaybe "n" t, v, f)

parseSingleCellRefNoting txt = fromJustNote errMsg $ parseSingleCellRefRaw txt
  where
    errMsg = "Bad cell reference '" ++ T.unpack txt ++ "'"
    parseSingleCellRefRaw t = do
      let (colT, rowT) = T.span (inRange ('A', 'Z')) t
      guard $ not (T.null colT) && not (T.null rowT) && T.all isDigit rowT
      row <- decimal rowT
      return (row, col2int colT)

col2int :: Text -> Int
col2int = T.foldl' (\i c -> i * 26 + let2int c) 0
    where
        let2int c = 1 + ord c - ord 'A'

decimal :: (Monad m, Integral a) => Text -> m a
decimal t = case T.signed T.decimal $ t of
  Right (d, leftover) | T.null leftover -> return d
  _ -> fail $ "invalid decimal" ++ show t

n_ :: Text -> Name
n_ x = Name
  { nameLocalName = x
  , nameNamespace = Just "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
  , namePrefix = Just "n"
  }

parseCellsCursor :: LB.ByteString -> [((Int,Int), Maybe Int, Text, Maybe Text, Maybe Text)]
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

parseCellsExpatT :: LB.ByteString -> [((Int,Int), Maybe Int, Text, Maybe Text, Maybe Text)]
parseCellsExpatT bs = parseCells $ ET.parseThrowing ET.defaultParseOptions bs
  where
    parseCells ::
         ET.UNode Text -> [((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)]
    parseCells root = concat
      [ parseRows (ET.eChildren ch)
      | ch <- ET.eChildren root
      , ET.eName ch == "sheetData"
      ]
    parseRows :: [ET.UNode Text] -> [((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)]
    parseRows children = concat [parseRow (ET.eChildren row) | row <- children, ET.eName row == "row"]
    parseRow children = catMaybes [parseCell c | c <- children, ET.eName c == "c"]
    parseCell :: ET.UNode Text -> Maybe ((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)
    parseCell c = do
      let attrs = ET.eAttributes c
      ref <- lookup "r" attrs
      let s = lookup "s" attrs
          t = fromMaybe "n" $ lookup "t" attrs
          getText :: ET.UNode Text -> Text
          getText n = T.concat [ t | ET.Text t <- ET.eChildren n ]
          (v, afterV) = case ET.eChildren c of
            (e:rest) | ET.eName e == "v" -> (Just $ getText e, rest)
            other -> (Nothing, other)
          f = case afterV of
            (e:rest) | ET.eName e == "v" -> Just (getText e)
            _ -> Nothing
      return (parseSingleCellRefNoting $ ref, (readMay @ Int) =<< fmap T.unpack s, t, Nothing, f)

parseCellsExpatS :: LB.ByteString -> [((Int,Int), Maybe Int, Text, Maybe Text, Maybe Text)]
parseCellsExpatS bs = parseCells $ ES.parse ES.defaultParseOptions bs
  where
    parseCells ::
         [ES.SAXEvent Text Text]
      -> [((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)]
    parseCells evs =
      parseRows . (\(_,inner,_) -> inner) . fromJustNote "missing sheetData" $ takeTag "sheetData" evs
    takeTag nm evs =
      let (upToEnd, rest) = span (not . isEndTag nm) $
            dropWhile (not . isStartTag nm) evs
      in case upToEnd of
        (ES.StartElement tag' attrs):inner -> Just (attrs, inner, rest)
        _ -> Nothing
    parseRows :: [ES.SAXEvent Text Text]
              -> [((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)]
    parseRows evs =
      case  takeTag "row" evs of
        Just (_, rowInner, rest) ->
          parseRow rowInner ++ parseRows rest
        _ -> []
    parseRow :: [ES.SAXEvent Text Text]
              -> [((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)]
    parseRow evs =
      case  takeTag "c" evs of
        Just (attrs, cInner, rest) ->
          case parseCell attrs cInner of
            Just c -> c:parseRow rest
            Nothing -> parseRow rest
        _ -> []
    parseCell :: [(Text, Text)] -> [ES.SAXEvent Text Text]
              -> Maybe ((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)
    parseCell attrs inner = do
      ref <- lookup "r" attrs
      let s = lookup "s" attrs
          t = fromMaybe "n" $ lookup "t" attrs
          (v, afterV) = case takeTag "v" inner of
            Just (_, inV, rest) -> (Just $ getContent inV, rest)
            Nothing -> (Nothing, inner)
          f = case takeTag "v" inner of
            Just (_, inF, _) -> Just $ getContent inF
            Nothing -> Nothing
      return (parseSingleCellRefNoting $ ref, (readMay @ Int) =<< fmap T.unpack s, t, Nothing, f)

getContent :: [ES.SAXEvent Text Text] -> Text
getContent =
    T.concat . map getCharData
  where
    getCharData (ES.CharacterData text) = text
    getCharData _                    = ""


isStartTag :: Eq tag => tag -> ES.SAXEvent tag text -> Bool
isStartTag tag (ES.StartElement tag' _) = tag == tag'
isStartTag _   _ = False

isEndTag :: Eq tag => tag -> ES.SAXEvent tag text -> Bool
isEndTag tag (ES.EndElement tag') = tag == tag'
isEndTag _   _ = False
