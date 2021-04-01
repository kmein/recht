{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Recht.Scraper (lawEntries, lawFromEntry) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (guard)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Recht.Cache
import Recht.Render (htmlToPlain)
import Recht.Types
import System.FilePath ((<.>))
import Text.HTML.Scalpel

root :: URL
root = "https://www.gesetze-im-internet.de"

prefixRoot :: URL -> URL
prefixRoot x = root <> "/" <> fromMaybe x (stripPrefix "./" x)

lawEntries :: IO [LawEntry]
lawEntries =
  cached "lawentries" $
    concat <$> (mapConcurrently lawsOfIndex =<< indices)
  where
    indices :: IO [URL]
    indices = maybe [] (map prefixRoot) <$> scrapeURL (prefixRoot "aktuell.html") (attrs "href" ("a" @: [hasClass "alphabet"]))
    lawsOfIndex :: URL -> IO [LawEntry]
    lawsOfIndex index = fmap (fromMaybe []) $
      scrapeURL index $
        chroot ("div" @: ["id" @= "container"]) $
          chroots "p" $ do
            lawEntryUrl <- dropWhileEnd (/= '/') . prefixRoot <$> attr "href" "a"
            lawEntryAbbreviation <- Text.strip . Text.pack <$> text "abbr"
            lawEntryTitle <- Text.pack <$> attr "title" "abbr"
            return LawEntry {..}

lawFromEntry :: LawEntry -> IO Law
lawFromEntry entry = cached (Text.unpack (lawEntryAbbreviation entry) <.> "law") $ getLaw =<< lawLink entry

lawLink :: LawEntry -> IO URL
lawLink LawEntry {..} =
  fmap (fromMaybe $ fail $ "no HTML version for " <> Text.unpack lawEntryAbbreviation) $
    scrapeURL lawEntryUrl $
      chroot ("div" @: ["id" @= "container"]) $
        chroot ("h2" // "a") $ do
          linkType <- text (anySelector // "abbr")
          guard $ linkType == "HTML"
          (lawEntryUrl <>) <$> attr "href" anySelector

getLaw :: URL -> IO Law
getLaw url =
  fmap (fromMaybe (error $ "Gesetz unter " <> url <> " konnte nicht gelesen werden.")) $
    scrapeURL url $
      chroot ("div" @: ["id" @= "container"]) $ do
        (lawTitle, lawAbbreviation, lawDate, lawId) <- chroot ("div" @: [hasClass "jnnorm", "title" @= "Rahmen"]) $ do
          lawTitle <- Text.strip <$> text ("div" @: [hasClass "jnheader"] // "h1")
          [lawAbbreviation, lawDate] <- texts $ "div" @: [hasClass "jnheader"] // "p"
          lawId <- attr "id" anySelector
          return (lawTitle, lawAbbreviation, lawDate, lawId)
        lawNorms <- chroots ("div" @: [hasClass "jnnorm", "title" @= "Einzelnorm"]) norm
        return Law {..}
  where
    norm = do
      normId <- attr "id" anySelector
      normTitle <- Text.strip <$> text ("span" @: [hasClass "jnentitel"])
      normNumber <- Text.strip <$> text ("span" @: [hasClass "jnenbez"])
      normText <- htmlToPlain <$> innerHTML ("div" @: [hasClass "jnhtml"])
      return Norm {..}
