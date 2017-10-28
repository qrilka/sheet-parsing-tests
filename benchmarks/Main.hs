{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception
import Criterion.Main
import qualified Data.ByteString.Lazy as LB

import XMLParsers

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
        , bench "with hexml" $
          nf parseCellsHexml (LB.toStrict sheetBs)
        ]
    ]
