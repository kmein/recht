{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeOperators     #-}

import Control.Concurrent.Async
import Control.Monad
import Options.Generic
import Data.Default
import Data.Either (fromRight)
import Data.List (dropWhileEnd, stripPrefix, find)
import Data.Maybe
import Options.Generic
import Safe (atMay)
import System.Random
import Text.HTML.Scalpel
import Text.Pandoc
import Text.Pandoc.Class
import Text.Regex.TDFA
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


data RechtOptions = Get Text (Maybe Text) | List (Maybe Text) | Random (Maybe Text)
    deriving (Generic, Show)

instance ParseRecord RechtOptions

htmlToPlain :: Text.Text -> Text.Text
htmlToPlain string = fromRight "" $ runPure $ writePlain def { writerWrapText = WrapNone } =<< readHtml def string

data LawEntry = LawEntry
  { lawEntryAbbreviation :: Text.Text
  , lawEntryTitle :: Text.Text
  , lawEntryUrl :: URL
  }
  deriving Show

data Law = Law
  { lawTitle :: Text.Text
  , lawAbbreviation :: Text.Text
  , lawDate :: Text.Text
  , lawId :: Text.Text
  , lawNorms :: [Norm]
  }
  deriving Show

data Norm = Norm
  { normTitle :: Text.Text
  , normNumber :: Text.Text
  , normId :: Text.Text
  , normText :: Text.Text
  }
  deriving Show

stringToMaybe :: Text.Text -> Maybe Text.Text
stringToMaybe string = if Text.null string then Nothing else Just string

prettyLaw :: Law -> Text.Text
prettyLaw Law{..} =
  Text.unlines $ mapMaybe stringToMaybe $
    [ lawTitle <> maybe "" (\x -> " (" <> x <> ")") (stringToMaybe lawAbbreviation)
    , lawDate
    , " "
    ] ++ map prettyNorm lawNorms

prettyNorm :: Norm -> Text.Text
prettyNorm Norm{..} =
  Text.unlines [normNumber <> maybe "" (" – " <>) (stringToMaybe normTitle), "", normText]

choose :: [a] -> IO (Maybe a)
choose list = (list `atMay`) <$> randomRIO (0, length list - 1)

root :: URL
root = "https://www.gesetze-im-internet.de"

prefixRoot :: URL -> URL
prefixRoot x = root <> "/" <> fromMaybe x (stripPrefix "./" x)

lawEntries :: IO [LawEntry]
lawEntries = concat <$> (mapConcurrently lawsOfIndex =<< indices)
  where
    indices :: IO [URL]
    indices = maybe [] (map prefixRoot) <$> scrapeURL (prefixRoot "aktuell.html") (attrs "href" ("a" @: [hasClass "alphabet"]))
    lawsOfIndex :: URL -> IO [LawEntry]
    lawsOfIndex index = fmap (fromMaybe []) $
      scrapeURL index $
        chroot ("div" @: ["id" @= "container"]) $ chroots "p" $ do
          lawEntryUrl <- dropWhileEnd (/= '/') . prefixRoot <$> attr "href" "a"
          lawEntryAbbreviation <- Text.strip . Text.pack <$> text "abbr"
          lawEntryTitle <- Text.pack <$> attr "title" "abbr"
          return LawEntry{..}

lawLink :: LawEntry -> IO (Maybe URL)
lawLink LawEntry{..} = scrapeURL lawEntryUrl $
  chroot ("div" @: ["id" @= "container"]) $ chroot ("h2" // "a") $ do
    linkType <- text (anySelector // "abbr")
    guard $ linkType == "HTML"
    (lawEntryUrl <>) <$> attr "href" anySelector


law :: URL -> IO (Maybe Law)
law url = scrapeURL url $
  chroot ("div" @: ["id" @= "container"]) $ do
    (lawTitle, lawAbbreviation, lawDate, lawId) <- chroot ("div" @: [hasClass "jnnorm", "title" @= "Rahmen"]) $ do
      lawTitle <- text $ "div" @: [hasClass "jnheader"] // "h1"
      [lawAbbreviation, lawDate] <- texts $ "div" @: [hasClass "jnheader"] // "p"
      lawId <- attr "id" anySelector
      return (lawTitle, lawAbbreviation, lawDate, lawId)
    lawNorms <- chroots ("div" @: [hasClass "jnnorm", "title" @= "Einzelnorm"]) norm
    return Law {..}
  where
    norm = do
      normId <- attr "id" anySelector
      normTitle <- text ("span" @: [hasClass "jnentitel"])
      normNumber <- text ("span" @: [hasClass "jnenbez"])
      normText <- htmlToPlain <$> innerHTML ("div" @: [hasClass "jnhtml"])
      return Norm {..}

main = do
  options <- getRecord "Gesetzbrowser CLI"
  laws <- lawEntries
  case options of
    Random maybeBuch -> do
      foundLaw <- case maybeBuch of
        Nothing -> choose laws >>= \case
          Just l -> lawLink l >>= \case
            Just url -> law url >>= \case
              Just l -> return l
              Nothing -> fail "parse law failed"
            Nothing -> fail "no law link"
          Nothing -> fail "not enough laws"
        Just buch -> findLaw buch laws
      randomNorm <- choose (lawNorms foundLaw)
      case randomNorm of
        Just norm -> Text.putStrLn $ prettyNorm norm
        Nothing -> fail "not enough norms"
    List Nothing -> forM_ laws $ \LawEntry{..} -> Text.putStrLn $ "[" <> lawEntryAbbreviation <> "] " <> lawEntryTitle
    List (Just buch) -> do
      foundLaw <- findLaw buch laws
      forM_ (lawNorms foundLaw) $ \Norm{..} ->
        Text.putStrLn $ normNumber <> maybe "" (" – " <>) (stringToMaybe normTitle)
    Get buch paragraph -> do
      laws <- lawEntries
      foundLaw <- findLaw buch laws
      case paragraph of
        Just number ->
          case find (\Norm{..} -> number `Text.isInfixOf` normNumber) (lawNorms foundLaw) of
            Just foundNorm -> Text.putStr $ prettyNorm foundNorm
            Nothing -> Text.putStrLn $ "'" <> buch <> "', " <> number <> " ist nicht auffindbar"
        Nothing -> Text.putStr $ prettyLaw foundLaw
  where
    findLaw string lawEntries =
      case find (\LawEntry{..} -> Text.toLower string == Text.toLower lawEntryAbbreviation) lawEntries of
        Just lawEntry -> lawLink lawEntry >>= \case
          Just lawUrl -> law lawUrl >>= \case
            Just foundLaw -> return foundLaw
            Nothing -> fail "kaputt"
          Nothing -> fail $ "'" <> Text.unpack (lawEntryTitle lawEntry) <> "'" <> " besitzt keine HTML-Version."
        Nothing -> fail $ "'" <> Text.unpack string <> "' ist nicht auffindbar"
