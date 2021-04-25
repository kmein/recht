{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Recht.Util (blockSize, choose, split, retry, unicodeSuperscript, ppToTTY) where

import Blessings (Blessable, Blessings, pp, stripSGR)
import Control.Concurrent
import Control.Exception
import qualified Data.Text as Text
import Safe (atMay)
import System.IO (hIsTerminalDevice, stdout)
import System.Random (randomRIO)

ppToTTY :: Blessable s => Blessings s -> IO s
ppToTTY blessings = do
  isTTY <- hIsTerminalDevice stdout
  return $
    pp $
      if isTTY
        then blessings
        else stripSGR blessings

blockSize :: Int
blockSize = 8 * 1024 ^ (2 :: Int)

choose :: [a] -> IO (Maybe a)
choose list = (list `atMay`) <$> randomRIO (0, length list - 1)

retry :: IO a -> IO a
retry f = catch @SomeException f (const $ threadDelay 1000000 >> f)

unicodeSuperscript :: Char -> Char
unicodeSuperscript = \case
  '0' -> '⁰'
  '1' -> '¹'
  '2' -> '²'
  '3' -> '³'
  '4' -> '⁴'
  '5' -> '⁵'
  '6' -> '⁶'
  '7' -> '⁷'
  '8' -> '⁸'
  '9' -> '⁹'
  c -> c

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
