module XMLParsers.Types
  ( CellData
  ) where

import Data.Text (Text
                 )
type CellData = ((Int, Int), Maybe Int, Text, Maybe Text, Maybe Text)
