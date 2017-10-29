module Main where

import qualified Data.ByteString.Lazy as LB
import Weigh

import XMLParsers

main :: IO ()
main = do
  withSheetBs $ \sheetBs ->
    mainWith $ do
      func "with xml-conduit/stream" (fromSomeEx . parseCellsStream) sheetBs
      func "with xml-conduit/cursor" parseCellsCursor sheetBs
      func "with hexpat/tree" parseCellsExpatT sheetBs
      func "with hexpat/SAX" parseCellsExpatS sheetBs
      func "with hexml" parseCellsHexml (LB.toStrict sheetBs)
      func "with xeno/DOM" parseCellsXeno (LB.toStrict sheetBs)
