{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Recht.Render (htmlToPlain, prettyNorm, prettyLaw, prettyNormTitle, prettyLawEntry, prettyLawTitle) where

import Data.Default (Default (def))
import Data.Either (fromRight)
import Data.Maybe (mapMaybe)
import Data.Text (Text, null, unlines)
import Recht.Types
import Text.Pandoc (WrapOption (WrapNone), readHtml, writeMarkdown, writerExtensions, writerWrapText)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Extensions

htmlToPlain :: Text -> Text
htmlToPlain string = fromRight "" $ runPure $ writeMarkdown def {writerWrapText = WrapNone, writerExtensions = enableExtension Ext_simple_tables (writerExtensions def)} =<< readHtml def string

stringToMaybe :: Text -> Maybe Text
stringToMaybe string = if Data.Text.null string then Nothing else Just string

prettyLawTitle :: Law -> Text
prettyLawTitle Law {..} = lawTitle <> maybe "" (\x -> " (" <> x <> ")") (stringToMaybe lawAbbreviation)

prettyLaw :: Law -> Text
prettyLaw law@Law {..} =
  Data.Text.unlines $
    mapMaybe stringToMaybe $
      [ "# " <> prettyLawTitle law,
        lawDate,
        " "
      ]
        ++ map prettyNorm lawNorms

prettyNormTitle :: Norm -> Text
prettyNormTitle Norm {..} = normNumber <> maybe "" (" – " <>) (stringToMaybe normTitle)

prettyNorm :: Norm -> Text
prettyNorm norm@Norm {..} =
  Data.Text.unlines ["## " <> prettyNormTitle norm, "", normText]

prettyLawEntry :: LawEntry -> Text
prettyLawEntry LawEntry {..} = "[" <> lawEntryAbbreviation <> "] " <> lawEntryTitle
