{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import qualified Data.ByteString.Lazy as LB

import XMLParsers

main :: IO ()
main =
  withSheetBs $ \sheetBs ->
    defaultMain
      [ bgroup
          "cell parsing"
          [ bench "with xml-conduit/stream" $
            nf (fromSomeEx . parseCellsStream) sheetBs
          , bench "with xml-conduit/cursor" $ nf parseCellsCursor sheetBs
          , bench "with hexpat/tree" $ nf parseCellsExpatT sheetBs
          , bench "with hexpat/SAX" $ nf parseCellsExpatS sheetBs
          , bench "with hexml" $ nf parseCellsHexml (LB.toStrict sheetBs)
          , bench "with xeno/DOM" $ nf parseCellsXeno (LB.toStrict sheetBs)
          ]
      ]
