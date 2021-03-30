{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

import Control.Concurrent.Async
import Control.Monad
import Data.Default
import Data.Either (fromRight)
import Data.List (dropWhileEnd, find, stripPrefix)
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Generic
import Safe (atMay)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.Random
import Text.HTML.Scalpel
import Text.Pandoc (WrapOption (WrapNone), readHtml, writePlain, writerWrapText)
import Text.Pandoc.Class (runPure)
import Text.Read
import Text.Regex.TDFA

data RechtOptions = Get Text (Maybe Text) | List (Maybe Text) | Random (Maybe Text)
  deriving (Generic, Show)

instance ParseRecord RechtOptions

htmlToPlain :: Text.Text -> Text.Text
htmlToPlain string = fromRight "" $ runPure $ writePlain def {writerWrapText = WrapNone} =<< readHtml def string

data LawEntry = LawEntry
  { lawEntryAbbreviation :: Text.Text,
    lawEntryTitle :: Text.Text,
    lawEntryUrl :: URL
  }
  deriving (Show, Read)

data Law = Law
  { lawTitle :: Text.Text,
    lawAbbreviation :: Text.Text,
    lawDate :: Text.Text,
    lawId :: Text.Text,
    lawNorms :: [Norm]
  }
  deriving (Show, Read)

data Norm = Norm
  { normTitle :: Text.Text,
    normNumber :: Text.Text,
    normId :: Text.Text,
    normText :: Text.Text
  }
  deriving (Show, Read)

cached :: (Read a, Show a) => FilePath -> IO a -> IO a
cached cacheName action = do
  cacheDirectoryPath <- getCacheDirectoryPath
  createDirectoryIfMissing True cacheDirectoryPath
  let cacheFile = cacheDirectoryPath </> map escape cacheName
  cachedFileExists <- doesFileExist cacheFile
  if cachedFileExists
    then do
      cachedFileContents <- readMaybe <$> readFile cacheFile
      case cachedFileContents of
        Just value -> pure value
        Nothing -> runAndCache cacheFile
    else runAndCache cacheFile
  where
    getCacheDirectoryPath = do
      xdgCacheHome <- lookupEnv "XDG_CACHE_HOME"
      case xdgCacheHome of
        Just path@(_ : _) -> pure $ path </> "recht"
        _ -> maybe ".recht" (\home -> home </> ".cache" </> "recht") <$> lookupEnv "HOME"
    escape '/' = '_'
    escape x = x
    runAndCache file = do
      result <- action
      result <$ writeFile file (show result)

stringToMaybe :: Text.Text -> Maybe Text.Text
stringToMaybe string = if Text.null string then Nothing else Just string

prettyLaw :: Law -> Text.Text
prettyLaw Law {..} =
  Text.unlines $
    mapMaybe stringToMaybe $
      [ lawTitle <> maybe "" (\x -> " (" <> x <> ")") (stringToMaybe lawAbbreviation),
        lawDate,
        " "
      ]
        ++ map prettyNorm lawNorms

prettyNorm :: Norm -> Text.Text
prettyNorm Norm {..} =
  Text.unlines [normNumber <> maybe "" (" – " <>) (stringToMaybe normTitle), "", normText]

choose :: [a] -> IO (Maybe a)
choose list = (list `atMay`) <$> randomRIO (0, length list - 1)

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
  fmap (fromMaybe (error "kaputt")) $
    scrapeURL url $
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

runRecht :: [LawEntry] -> RechtOptions -> IO ()
runRecht laws options =
  case options of
    Random maybeBuch -> do
      foundLaw <- case maybeBuch of
        Nothing -> lawFromEntry . fromJust =<< choose laws
        Just buch -> findLaw buch laws
      maybeRandomNorm <- choose (lawNorms foundLaw)
      case maybeRandomNorm of
        Just randomNorm -> do
          Text.putStrLn $ lawAbbreviation foundLaw
          Text.putStrLn $ lawTitle foundLaw
          Text.putStr $ prettyNorm randomNorm
        Nothing -> runRecht laws options
    List Nothing -> forM_ laws $ \LawEntry {..} -> Text.putStrLn $ "[" <> lawEntryAbbreviation <> "] " <> lawEntryTitle
    List (Just buch) -> do
      foundLaw <- findLaw buch laws
      forM_ (lawNorms foundLaw) $ \Norm {..} ->
        Text.putStrLn $ normNumber <> maybe "" (" – " <>) (stringToMaybe normTitle)
    Get buch paragraph -> do
      laws <- lawEntries
      foundLaw <- findLaw buch laws
      case paragraph of
        Just number ->
          case find (\Norm {..} -> number `Text.isInfixOf` normNumber) (lawNorms foundLaw) of
            Just foundNorm -> Text.putStr $ prettyNorm foundNorm
            Nothing -> Text.putStrLn $ "'" <> buch <> "', " <> number <> " ist nicht auffindbar"
        Nothing -> Text.putStr $ prettyLaw foundLaw
  where
    findLaw string lawEntries =
      case find (\LawEntry {..} -> Text.toLower string == Text.toLower lawEntryAbbreviation) lawEntries of
        Just lawEntry -> lawFromEntry lawEntry
        Nothing -> fail $ "'" <> Text.unpack string <> "' ist nicht auffindbar"

main :: IO ()
main = join $ runRecht <$> lawEntries <*> getRecord "Gesetzbrowser CLI"
