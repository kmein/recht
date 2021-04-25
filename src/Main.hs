{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main (main) where

import Blessings
import Control.Monad (forM_, void, (<=<))
import Data.List (find)
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Recht.Options
import Recht.Render
import Recht.Scraper
import Recht.Types
import Recht.Util (blockSize, choose, ppToTTY, retry)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.IO
import Text.Megaparsec.Error (errorBundlePretty)

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
          Text.putStrLn =<< ppToTTY (prettyLawTitle foundLaw)
          Text.putStr =<< ppToTTY (prettyNorm Nothing randomNorm)
        Nothing -> runRecht options
    Dump dumpDirectory -> do
      hSetBuffering stdout LineBuffering
      createDirectoryIfMissing True dumpDirectory
      forM_ laws $ \lawEntry -> retry $ do
        let dumpFileName = makeValid $ Text.unpack (Text.replace " " "_" $ Text.replace "/" "_" $ lawEntryAbbreviation lawEntry) <.> "md"
        law <- lawFromEntry lawEntry
        void $ Text.writeFile (dumpDirectory </> dumpFileName) $ pp $ stripSGR $ prettyLaw law
        Text.putStrLn $ Text.unwords ["-", "[" <> lawEntryAbbreviation lawEntry <> "](" <> Text.pack (dumpDirectory </> dumpFileName) <> ")", lawEntryTitle lawEntry]
    List Nothing -> mapM_ (Text.putStrLn <=< ppToTTY . prettyLawEntry) laws
    List (Just buch) -> mapM_ (Text.putStrLn <=< ppToTTY . prettyNormTitle) . lawNorms =<< findLaw buch laws
    Get buch maybeFocus -> do
      foundLaw <- findLaw buch laws
      case maybeFocus of
        Just (Right focus) ->
          case find (normMatches focus) $ lawNorms foundLaw of
            Just foundNorm -> Text.putStr =<< ppToTTY (prettyNorm (Just focus) foundNorm)
            Nothing -> Text.putStrLn $ "Keine Einzelnorm mit '" <> Text.pack (show focus) <> "' in '" <> lawTitle foundLaw <> " ' gefunden."
        Just (Left parseError) ->
          putStrLn $ errorBundlePretty parseError
        Nothing -> Text.putStr =<< ppToTTY (prettyLaw foundLaw)
  where
    findLaw string entries =
      case find (lawEntryMatches string) entries of
        Just lawEntry -> lawFromEntry lawEntry
        Nothing -> fail $ "Kein Gesetz mit der Abkürzung '" <> Text.unpack string <> "' gefunden."

main :: IO ()
main = do
  hSetBuffering stdout (BlockBuffering (Just blockSize))
  getRechtOptions >>= runRecht
