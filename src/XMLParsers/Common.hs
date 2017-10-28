module XMLParsers.Common
  ( parseSingleCellRefNoting
  ) where

import Control.Monad
import Data.Char
import Data.Ix (inRange)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Safe

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
decimal t = case T.signed T.decimal t of
  Right (d, leftover) | T.null leftover -> return d
  _ -> fail $ "invalid decimal" ++ show t
