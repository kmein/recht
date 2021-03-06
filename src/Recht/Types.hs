{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Recht.Types (LawEntry (..), Law (..), Norm (..), normMatches, lawEntryMatches, parseFocus, Focus (..), ParseError) where

import Data.Binary (Binary)
import Data.Text (Text, isInfixOf, pack, toLower)
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.HTML.Scalpel (URL)
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char

data LawEntry = LawEntry
  { lawEntryAbbreviation :: Text,
    lawEntryTitle :: Text,
    lawEntryUrl :: URL
  }
  deriving (Show, Generic)

instance Binary LawEntry

data Law = Law
  { lawTitle :: Text,
    lawAbbreviation :: Text,
    lawDate :: Text,
    lawId :: Text,
    lawNorms :: [Norm]
  }
  deriving (Show, Generic)

instance Binary Law

data Norm = Norm
  { normTitle :: Text,
    normNumber :: Maybe Text,
    normId :: Text,
    normParagraphs :: [Text]
  }
  deriving (Show, Generic)

instance Binary Norm

normMatches :: Focus -> Norm -> Bool
normMatches focus Norm {..} =
  case focusNorm focus of
    Just search ->
      ( case normNumber of
          Just number -> search `isInfixOf` number
          Nothing -> False
      )
        || toLower search
        `isInfixOf` toLower normTitle
    Nothing -> False

lawEntryMatches :: Text -> LawEntry -> Bool
lawEntryMatches search LawEntry {..} = toLower search == toLower lawEntryAbbreviation

data Focus = Focus
  { focusNorm :: Maybe Text,
    focusParagraph :: Maybe [Text],
    focusSentence :: Maybe [Text],
    focusNumber :: Maybe [Text]
  }
  deriving (Show)

type ParseError = ParseErrorBundle Text Void

parseFocus :: Text -> Either ParseError Focus
parseFocus = parse focusParser "command-line"
  where
    focusParser = do
      focusNorm <- optional $ optional (try (string "Artikel") <|> string "Art." <|> string "§") >> space >> number
      space
      focusParagraph <- optional $ (try (string "Absatz") <|> string "Abs.") >> space >> enumerationOf number
      space
      focusSentence <- optional $ string "Satz" >> space >> enumerationOf number
      space
      focusNumber <- optional $ (try (string "Nummer") <|> string "Nr.") >> space >> enumerationOf number
      Focus {..} <$ eof
      where
        number = pack <$> ((++) <$> some digitChar <*> many lowerChar)
        enumerationOf x = x `sepBy1` (try (string " und ") <|> try (string " u. ") <|> string " oder " <|> string ", ")
