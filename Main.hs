{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

import Control.Concurrent.Async
import Control.Monad
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B (hGet, length)
import Data.Default
import Data.Either (fromRight)
import Data.List (dropWhileEnd, find, stripPrefix)
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
import GHC.Generics (Generic)
import Options.Applicative
import Safe (atMay)
import System.Directory
import System.FilePath
import System.IO
import System.Random
import Text.HTML.Scalpel
import Text.Pandoc (WrapOption (WrapNone), readHtml, writeMarkdown, writerExtensions, writerWrapText)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Extensions

newtype RechtOptions = RechtOptions {rechtAction :: RechtAction}

data RechtAction = Get Text.Text (Maybe Text.Text) | List (Maybe Text.Text) | Random (Maybe Text.Text)

rechtArguments :: Parser RechtOptions
rechtArguments =
  RechtOptions
    <$> subparser
      ( mconcat
          [ command "get" $ info ((Get <$> buch <*> optional search) <**> helper) $ progDesc "Gesetze oder Einzelnormen anzeigen",
            command "list" $ info ((List <$> optional buch) <**> helper) $ progDesc "Alle Gesetze bzw. Einzelnormen eines Gesetzes auflisten.",
            command "random" $ info ((Random <$> optional buch) <**> helper) $ progDesc "Eine zufällige Einzelnorm (optional aus einem spezifierten Gesetzbuch) ausgeben."
          ]
      )
  where
    buch = strArgument $ metavar "BUCH" <> help "Abkürzung eines Gesetzbuches"
    search = strArgument $ metavar "ZAHL|TITEL" <> help "Suchbegriff für den Titel der Einzelnorm"

getRechtOptions :: IO RechtOptions
getRechtOptions = execParser $ info (helper <*> rechtArguments) $ fullDesc <> header "The recht Gesetz-Browser"

blockSize :: Int
blockSize = 8 * 1024 ^ (2 :: Int)

-- Taken from https://hackage.haskell.org/package/binary-0.8.8.0/docs/src/Data.Binary.html#decodeFileOrFail
-- to adjust chunk size to 8M instead of 32K (the Haskell default).
decodeFileOrFail' :: Binary a => FilePath -> IO (Either (ByteOffset, String) a)
decodeFileOrFail' f =
  withBinaryFile f ReadMode $ \h -> do
    feed (runGetIncremental get) h
  where
    feed (Done _ _ x) _ = return (Right x)
    feed (Fail _ pos string) _ = return (Left (pos, string))
    feed (Partial k) h = do
      chunk <- B.hGet h blockSize
      case B.length chunk of
        0 -> feed (k Nothing) h
        _ -> feed (k (Just chunk)) h

htmlToPlain :: Text.Text -> Text.Text
htmlToPlain string = fromRight "" $ runPure $ writeMarkdown def {writerWrapText = WrapNone, writerExtensions = enableExtension Ext_simple_tables (writerExtensions def)} =<< readHtml def string

data LawEntry = LawEntry
  { lawEntryAbbreviation :: Text.Text,
    lawEntryTitle :: Text.Text,
    lawEntryUrl :: URL
  }
  deriving (Show, Generic)

data Law = Law
  { lawTitle :: Text.Text,
    lawAbbreviation :: Text.Text,
    lawDate :: Text.Text,
    lawId :: Text.Text,
    lawNorms :: [Norm]
  }
  deriving (Show, Generic)

data Norm = Norm
  { normTitle :: Text.Text,
    normNumber :: Text.Text,
    normId :: Text.Text,
    normText :: Text.Text
  }
  deriving (Show, Generic)

instance Binary Norm

instance Binary Law

instance Binary LawEntry

cached :: (Binary a) => FilePath -> IO a -> IO a
cached cacheName ioAction = do
  cacheDirectoryPath <- getXdgDirectory XdgCache "recht"
  createDirectoryIfMissing True cacheDirectoryPath
  let cacheFile = cacheDirectoryPath </> map escape cacheName
  cachedFileExists <- doesFileExist cacheFile
  if cachedFileExists
    then do
      cacheTooOld <- isCacheInvalidated cacheFile
      if cacheTooOld
        then runAndCache cacheFile
        else do
          cachedFileContents <- decodeFileOrFail' cacheFile
          case cachedFileContents of
            Right decoded -> pure decoded
            _ -> runAndCache cacheFile
    else runAndCache cacheFile
  where
    isCacheInvalidated file = do
      modificationTime <- getModificationTime file
      now <- getCurrentTime
      let cacheAge = diffUTCTime now modificationTime
      return $ cacheAge >= nominalDay
    escape '/' = '_'
    escape x = x
    runAndCache file = do
      result <- ioAction
      result <$ encodeFile file result

stringToMaybe :: Text.Text -> Maybe Text.Text
stringToMaybe string = if Text.null string then Nothing else Just string

prettyLaw :: Law -> Text.Text
prettyLaw Law {..} =
  Text.unlines $
    mapMaybe stringToMaybe $
      [ "# " <> lawTitle <> maybe "" (\x -> " (" <> x <> ")") (stringToMaybe lawAbbreviation),
        lawDate,
        " "
      ]
        ++ map prettyNorm lawNorms

prettyNorm :: Norm -> Text.Text
prettyNorm Norm {..} =
  Text.unlines ["## " <> normNumber <> maybe "" (" – " <>) (stringToMaybe normTitle), "", normText]

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

runRecht :: RechtOptions -> IO ()
runRecht options = do
  laws <- lawEntries
  case rechtAction options of
    Random maybeBuch -> do
      foundLaw <- case maybeBuch of
        Nothing -> lawFromEntry . fromJust =<< choose laws
        Just buch -> findLaw buch laws
      maybeRandomNorm <- choose (lawNorms foundLaw)
      case maybeRandomNorm of
        Just randomNorm -> do
          Text.putStrLn $ "# " <> lawAbbreviation foundLaw <> " – " <> lawTitle foundLaw
          Text.putStr $ prettyNorm randomNorm
        Nothing -> runRecht options
    List Nothing -> forM_ laws $ \LawEntry {..} -> Text.putStrLn $ "[" <> lawEntryAbbreviation <> "] " <> lawEntryTitle
    List (Just buch) -> do
      foundLaw <- findLaw buch laws
      forM_ (lawNorms foundLaw) $ \Norm {..} ->
        Text.putStrLn $ normNumber <> maybe "" (" – " <>) (stringToMaybe normTitle)
    Get buch paragraph -> do
      foundLaw <- findLaw buch laws
      case paragraph of
        Just number ->
          case find (\Norm {..} -> number `Text.isInfixOf` normNumber) (lawNorms foundLaw) of
            Just foundNorm -> Text.putStr $ prettyNorm foundNorm
            Nothing -> Text.putStrLn $ "'" <> buch <> "', " <> number <> " ist nicht auffindbar"
        Nothing -> Text.putStr $ prettyLaw foundLaw
  where
    findLaw string entries =
      case find (\LawEntry {..} -> Text.toLower string == Text.toLower lawEntryAbbreviation) entries of
        Just lawEntry -> lawFromEntry lawEntry
        Nothing -> fail $ "'" <> Text.unpack string <> "' ist nicht auffindbar"

main :: IO ()
main = do
  hSetBuffering stdout (BlockBuffering (Just blockSize))
  getRechtOptions >>= runRecht
