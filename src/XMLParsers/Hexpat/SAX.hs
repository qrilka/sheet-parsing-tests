{-# LANGUAGE OverloadedStrings #-}
module XMLParsers.Hexpat.SAX
  ( parseCellsExpatS
  ) where

import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Safe
import qualified Text.XML.Expat.SAX as ES
import XMLParsers.Common
import XMLParsers.Types

parseCellsExpatS :: LB.ByteString -> [CellData]
parseCellsExpatS bs = parseCells $ ES.parse ES.defaultParseOptions bs
  where
    parseCells ::
         [ES.SAXEvent Text Text]
      -> [((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)]
    parseCells evs =
      parseRows . (\(_,inner,_) -> inner) . fromJustNote "missing sheetData" $ takeTag "sheetData" evs
    takeTag nm evs =
      let (upToEnd, rest) = break (isEndTag nm) $
            dropWhile (not . isStartTag nm) evs
      in case upToEnd of
        ES.StartElement _tag attrs : inner -> Just (attrs, inner, rest)
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
          f = case takeTag "v" afterV of
            Just (_, inF, _) -> Just $ getContent inF
            Nothing -> Nothing
      return (parseSingleCellRefNoting ref, readMay  =<< fmap T.unpack s, t, v, f)

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
