module Main where

import Control.Exception
import qualified Data.ByteString.Lazy as LB
import Weigh

import XMLParsers

main :: IO ()
main = do
  sheetBs <- LB.readFile "testSheet.xml"
  let fromEx :: Either SomeException a -> a
      fromEx (Left ex) = error $ "parsing failed: " ++ show ex
      fromEx (Right a) = a
  mainWith $ do
    func "with xml-conduit/stream" (fromEx . parseCellsStream) sheetBs
    func "with xml-conduit/cursor" parseCellsCursor sheetBs           
    func "with hexpat/tree" parseCellsExpatT sheetBs                  
    func "with hexpat/SAX" parseCellsExpatS sheetBs                   
