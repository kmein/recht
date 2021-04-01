module Recht.Util (blockSize, choose) where

import Safe (atMay)
import System.Random (randomRIO)

blockSize :: Int
blockSize = 8 * 1024 ^ (2 :: Int)

choose :: [a] -> IO (Maybe a)
choose list = (list `atMay`) <$> randomRIO (0, length list - 1)
