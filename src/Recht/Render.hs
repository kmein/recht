{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Recht.Render (prettyNorm, prettyLaw, prettyNormTitle, prettyLawEntry, prettyLawTitle) where

import Data.Default (Default (def))
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, isInfixOf, null, pack, unlines)
import Recht.Types
import System.Console.ANSI
import Text.Pandoc (WrapOption (WrapNone), readHtml, writeOrg, writerWrapText)
import Text.Pandoc.Class (runPure)

withSGR :: [SGR] -> Text -> Text
withSGR codes string = pack (setSGRCode codes) <> string <> pack (setSGRCode [Reset])

concatWith :: Text -> Maybe Text -> Maybe Text -> Text
concatWith middle left right =
  case (left, right) of
    (Just x, Just y) -> x <> middle <> y
    (Just x, Nothing) -> x
    (Nothing, Just y) -> y
    (Nothing, Nothing) -> ""

stringToMaybe :: Text -> Maybe Text
stringToMaybe string = if Data.Text.null string then Nothing else Just string

htmlToPlain :: Text -> Text
htmlToPlain string = fromRight "" $ runPure $ writeOrg def {writerWrapText = WrapNone} =<< readHtml def string

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
        ++ map (prettyNorm Nothing) lawNorms

prettyNormTitle :: Norm -> Text
prettyNormTitle Norm {..} = concatWith " - " (withSGR [SetConsoleIntensity BoldIntensity] <$> normNumber) (withSGR [SetColor Foreground Dull Cyan] <$> stringToMaybe normTitle)

prettyNorm :: Maybe Focus -> Norm -> Text
prettyNorm focus norm@Norm {..} =
  Data.Text.unlines $ [prettyNormTitle norm, ""] <> map (maybeHighlight . htmlToPlain) normParagraphs
  where
    highlight = withSGR [SetColor Foreground Dull Yellow]
    maybeHighlight text = if any (\p -> ("(" <> p <> ")") `isInfixOf` text) (fromMaybe [] $ focusParagraph =<< focus) then highlight text else text

prettyLawEntry :: LawEntry -> Text
prettyLawEntry LawEntry {..} = "[" <> withSGR [SetColor Foreground Dull Red] lawEntryAbbreviation <> "] " <> lawEntryTitle
