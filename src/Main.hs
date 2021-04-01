{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}

module Main (main) where

import Data.List (find)
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Recht.Options
import Recht.Render
import Recht.Scraper
import Recht.Types
import Recht.Util (blockSize, choose)
import System.IO

runRecht :: RechtOptions -> IO ()
runRecht options = do
  laws <- lawEntries
  case rechtAction options of
    Random maybeBuch -> do
      foundLaw <- case maybeBuch of
        Nothing -> lawFromEntry . fromJust =<< choose laws
        Just buch -> findLaw buch laws
      choose (lawNorms foundLaw) >>= \case
        Just randomNorm -> do
          Text.putStrLn $ "# " <> prettyLawTitle foundLaw
          Text.putStr $ prettyNorm randomNorm
        Nothing -> runRecht options
    List Nothing -> mapM_ (Text.putStrLn . prettyLawEntry) laws
    List (Just buch) -> mapM_ (Text.putStrLn . prettyNormTitle) . lawNorms =<< findLaw buch laws
    Get buch maybeSearch -> do
      foundLaw <- findLaw buch laws
      case maybeSearch of
        Just search ->
          case find (normMatches search) $ lawNorms foundLaw of
            Just foundNorm -> Text.putStr $ prettyNorm foundNorm
            Nothing -> Text.putStrLn $ "Keine Einzelnorm mit '" <> search <> "' in '" <> lawTitle foundLaw <> " ' gefunden."
        Nothing -> Text.putStr $ prettyLaw foundLaw
  where
    findLaw string entries =
      case find (lawEntryMatches string) entries of
        Just lawEntry -> lawFromEntry lawEntry
        Nothing -> fail $ "Kein Gesetz mit der Abkürzung '" <> Text.unpack string <> "' gefunden."

main :: IO ()
main = do
  hSetBuffering stdout (BlockBuffering (Just blockSize))
  getRechtOptions >>= runRecht
