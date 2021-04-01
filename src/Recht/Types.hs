{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Recht.Types (LawEntry (..), Law (..), Norm (..), normMatches, lawEntryMatches) where

import Data.Binary (Binary)
import Data.Text (Text, isInfixOf, toLower)
import GHC.Generics (Generic)
import Text.HTML.Scalpel (URL)

data LawEntry = LawEntry
  { lawEntryAbbreviation :: Text,
    lawEntryTitle :: Text,
    lawEntryUrl :: URL
  }
  deriving (Show, Generic)

data Law = Law
  { lawTitle :: Text,
    lawAbbreviation :: Text,
    lawDate :: Text,
    lawId :: Text,
    lawNorms :: [Norm]
  }
  deriving (Show, Generic)

data Norm = Norm
  { normTitle :: Text,
    normNumber :: Text,
    normId :: Text,
    normText :: Text
  }
  deriving (Show, Generic)

normMatches :: Text -> Norm -> Bool
normMatches search Norm {..} = search `isInfixOf` normNumber || toLower search `isInfixOf` toLower normTitle

lawEntryMatches :: Text -> LawEntry -> Bool
lawEntryMatches string LawEntry {..} = toLower string == toLower lawEntryAbbreviation

instance Binary Norm

instance Binary Law

instance Binary LawEntry
