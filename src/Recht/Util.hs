{-# LANGUAGE TypeApplications #-}
module Recht.Util (blockSize, choose, split, retry) where

import qualified Data.Text as Text
import Safe (atMay)
import Control.Exception
import System.Random (randomRIO)
import Control.Concurrent

blockSize :: Int
blockSize = 8 * 1024 ^ (2 :: Int)

choose :: [a] -> IO (Maybe a)
choose list = (list `atMay`) <$> randomRIO (0, length list - 1)

retry :: IO a -> IO a
retry f = catch @SomeException f (const $ threadDelay 1000000 >> f)

-- split a string by a delimiter, keeping the delimiter in front:
-- split isDigit "hello1world2foo354bar" == ["hello", "1world", "2foo", "354bar"]
split :: (Char -> Bool) -> Text.Text -> [Text.Text]
split p string =
  if Text.null untilFirst then loop firstAndRest else untilFirst : loop firstAndRest
  where
    (untilFirst, firstAndRest) = Text.break p string
    loop s
      | Text.null s = []
      | otherwise =
        let (delim, rest) = Text.span p s
            (untilDelim, rest') = Text.break p rest
         in (delim <> untilDelim) : loop rest'
