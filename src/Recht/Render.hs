{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Recht.Render (prettyNorm, prettyLaw, prettyNormTitle, prettyLawEntry, prettyLawTitle, split) where

import Blessings.Text
import Data.Char (isDigit)
import Data.Default (Default (def))
import Data.Either (fromRight)
import Data.List
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Recht.Types
import Recht.Util (split)
import Text.Pandoc (WrapOption (WrapNone), readHtml, writeOrg, writerWrapText)
import Text.Pandoc.Class (runPure)

concatWith :: Monoid m => m -> Maybe m -> Maybe m -> m
concatWith middle left right =
  case (left, right) of
    (Just x, Just y) -> x <> middle <> y
    (Just x, Nothing) -> x
    (Nothing, Just y) -> y
    (Nothing, Nothing) -> mempty

stringToMaybe :: (Eq m, Monoid m) => m -> Maybe m
stringToMaybe string = if mempty == string then Nothing else Just string

htmlToPlain :: Text -> Text
htmlToPlain string = fromRight "" $ runPure $ writeOrg def {writerWrapText = WrapNone} =<< readHtml def string

prettyLawTitle :: Law -> Blessings Text
prettyLawTitle Law {..} = maybe "" (\x -> "[" <> SGR [2, 31] (Plain x) <> "] ") (stringToMaybe lawAbbreviation) <> Plain lawTitle

prettyLaw :: Law -> Blessings Text
prettyLaw law@Law {..} =
  mconcat . intersperse "\n\n" $
    mapMaybe stringToMaybe $
      [ prettyLawTitle law,
        Plain lawDate,
        " "
      ]
        ++ map (prettyNorm Nothing) lawNorms

prettyNormTitle :: Norm -> Blessings Text
prettyNormTitle Norm {..} = concatWith " - " (SGR [1] . Plain <$> normNumber) (SGR [2, 36] . Plain <$> stringToMaybe normTitle)

prettyNorm :: Maybe Focus -> Norm -> Blessings Text
prettyNorm focus norm@Norm {..} =
  mconcat . intersperse "\n" $ [prettyNormTitle norm, ""] <> map (maybeHighlight . replaceSentenceMark . htmlToPlain) normParagraphs
  where
    unicodeSuperscript = \case
      '0' -> '⁰'
      '1' -> '¹'
      '2' -> '²'
      '3' -> '³'
      '4' -> '⁴'
      '5' -> '⁵'
      '6' -> '⁶'
      '7' -> '⁷'
      '8' -> '⁸'
      '9' -> '⁹'
      c -> c
    replaceSentenceMark = Text.concat . map prettifySentenceMark . Text.splitOn "^{"
      where
        prettifySentenceMark sentence =
          if Text.all isDigit mark then Text.map unicodeSuperscript mark <> Text.drop 1 sentence' else sentence
          where
            (mark, sentence') = Text.breakOn "}" sentence
    maybeHighlight text =
      if any (\p -> ("(" <> p <> ")") `Text.isInfixOf` text) (fromMaybe [] $ focusParagraph =<< focus)
        then highlight . mconcat . map maybeHighlightSentence . splitSentences $ text
        else Plain text
      where
        highlight = SGR [2, 33]
        splitSentences = split (`elem` ("⁰¹²³⁴⁵⁶⁷⁸⁹" :: String))
        maybeHighlightSentence sentence =
          if any (\s -> Text.map unicodeSuperscript s `Text.isInfixOf` sentence) (fromMaybe [] $ focusSentence =<< focus)
            then highlightSentence sentence
            else Plain sentence
          where
            highlightSentence = SGR [1, 33] . Plain

prettyLawEntry :: LawEntry -> Blessings Text
prettyLawEntry LawEntry {..} = "[" <> SGR [2, 31] (Plain lawEntryAbbreviation) <> "] " <> Plain lawEntryTitle
