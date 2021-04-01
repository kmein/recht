{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Recht.Render (htmlToPlain, prettyNorm, prettyLaw, prettyNormTitle, prettyLawEntry, prettyLawTitle) where

import Data.Default (Default (def))
import Data.Either (fromRight)
import Data.Maybe (mapMaybe)
import Data.Text (Text, null, pack, unlines)
import Recht.Types
import System.Console.ANSI
import Text.Pandoc (WrapOption (WrapNone), readHtml, writePlain, writerExtensions, writerWrapText)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Extensions

withSGR :: [SGR] -> Text -> Text
withSGR codes string = pack (setSGRCode codes) <> string <> pack (setSGRCode [Reset])

htmlToPlain :: Text -> Text
htmlToPlain string = fromRight "" $ runPure $ writePlain def {writerWrapText = WrapNone, writerExtensions = enableExtension Ext_simple_tables (writerExtensions def)} =<< readHtml def string

stringToMaybe :: Text -> Maybe Text
stringToMaybe string = if Data.Text.null string then Nothing else Just string

prettyLawTitle :: Law -> Text
prettyLawTitle Law {..} = maybe "" (\x -> "[" <> withSGR [SetColor Foreground Dull Red] x <> "] ") (stringToMaybe lawAbbreviation) <> lawTitle

prettyLaw :: Law -> Text
prettyLaw law@Law {..} =
  Data.Text.unlines $
    mapMaybe stringToMaybe $
      [ prettyLawTitle law,
        lawDate,
        " "
      ]
        ++ map prettyNorm lawNorms

prettyNormTitle :: Norm -> Text
prettyNormTitle Norm {..} = withSGR [SetConsoleIntensity BoldIntensity] normNumber <> maybe "" ((" – " <>) . withSGR [SetColor Foreground Dull Cyan]) (stringToMaybe normTitle)

prettyNorm :: Norm -> Text
prettyNorm norm@Norm {..} =
  Data.Text.unlines [prettyNormTitle norm, "", normText]

prettyLawEntry :: LawEntry -> Text
prettyLawEntry LawEntry {..} = "[" <> withSGR [SetColor Foreground Dull Red] lawEntryAbbreviation <> "] " <> lawEntryTitle
