{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main (main) where

import Blessings
import Data.List (find)
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing)
import Recht.Options
import Recht.Render
import Control.Monad (void, forM_)
import System.FilePath
import Recht.Scraper
import Recht.Types
import Recht.Util (blockSize, choose, retry)
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
          Text.putStrLn $ pp $ prettyLawTitle foundLaw
          Text.putStr $ pp $ prettyNorm Nothing randomNorm
        Nothing -> runRecht options
    Dump dumpDirectory -> do
      hSetBuffering stdout LineBuffering
      createDirectoryIfMissing True dumpDirectory
      forM_ laws $ \lawEntry -> retry $ do
        let dumpFileName = makeValid $ Text.unpack (Text.replace " " "_" $ Text.replace "/" "_" $ lawEntryAbbreviation lawEntry) <.> "md"
        law <- lawFromEntry lawEntry
        void $ Text.writeFile (dumpDirectory </> dumpFileName) $ pp $ stripSGR $ prettyLaw law
        Text.putStrLn $ Text.unwords ["-", "[" <> lawEntryAbbreviation lawEntry <> "](" <> Text.pack (dumpDirectory </> dumpFileName) <> ")", lawEntryTitle lawEntry]
    List Nothing -> mapM_ (Text.putStrLn . pp . prettyLawEntry) laws
    List (Just buch) -> mapM_ (Text.putStrLn . pp . prettyNormTitle) . lawNorms =<< findLaw buch laws
    Get buch maybeFocus -> do
      foundLaw <- findLaw buch laws
      case maybeFocus of
        Just focus ->
          case find (normMatches focus) $ lawNorms foundLaw of
            Just foundNorm -> Text.putStr $ pp $ prettyNorm (Just focus) foundNorm
            Nothing -> Text.putStrLn $ "Keine Einzelnorm mit '" <> Text.pack (show focus) <> "' in '" <> lawTitle foundLaw <> " ' gefunden."
        Nothing -> Text.putStr $ pp $ prettyLaw foundLaw
  where
    findLaw string entries =
      case find (lawEntryMatches string) entries of
        Just lawEntry -> lawFromEntry lawEntry
        Nothing -> fail $ "Kein Gesetz mit der Abkürzung '" <> Text.unpack string <> "' gefunden."

main :: IO ()
main = do
  hSetBuffering stdout (BlockBuffering (Just blockSize))
  getRechtOptions >>= runRecht
