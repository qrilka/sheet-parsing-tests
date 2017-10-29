module XMLParsers
  ( parseCellsCursor
  , parseCellsExpatS
  , parseCellsExpatT
  , parseCellsHexml
  , parseCellsStream
  , parseCellsXeno
  , withSheetBs
  , fromSomeEx
  ) where

import Control.Exception (SomeException)
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import XMLParsers.Hexml
import XMLParsers.Hexpat.SAX
import XMLParsers.Hexpat.Tree
import XMLParsers.Xeno
import XMLParsers.XmlConduit.Cursor
import XMLParsers.XmlConduit.Stream

withSheetBs :: (LB.ByteString -> IO ()) -> IO ()
withSheetBs action = do
  fileArg <- lookupEnv "XMLFILE"
  let fileName = fromMaybe "testSheetNoPrologue.xml" $ fileArg
  bs <- LB.readFile fileName
  action bs

fromSomeEx :: Either SomeException a -> a
fromSomeEx (Left ex) = error $ "parsing failed: " ++ show ex
fromSomeEx (Right a) = a
