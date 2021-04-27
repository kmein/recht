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
import Recht.Util (split, unicodeSuperscript)
import Text.Pandoc (WrapOption (WrapNone), readHtml, writeMarkdown, writerExtensions, writerWrapText)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Extensions (githubMarkdownExtensions)

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
htmlToPlain string = replaceSuperscript $ simplifyMarkdown $ fromRight "" $ runPure $ writeMarkdown def {writerWrapText = WrapNone, writerExtensions = githubMarkdownExtensions} =<< readHtml def string
  where
    simplifyMarkdown = Text.replace "\\(" "(" . Text.replace "\\)" ")" . Text.replace "\\.  \n" ". " . Text.replace "\\)  \n" ") "
    replaceSuperscript = Text.concat . map prettifySentenceMark . Text.splitOn supBegin
      where
        (supBegin, supEnd) = ("<sup>", "</sup>")
        prettifySentenceMark sentence =
          if Text.all isDigit mark then Text.map unicodeSuperscript mark <> Text.drop (Text.length supEnd) sentence' else sentence
          where
            (mark, sentence') = Text.breakOn supEnd sentence

prettyLawTitle :: Law -> Blessings Text
prettyLawTitle Law {..} = SGR [34] "% " <> SGR [1] (Plain lawTitle)

prettyLaw :: Law -> Blessings Text
prettyLaw law@Law {..} =
  mconcat . intersperse "\n" $
    mapMaybe stringToMaybe $
      [ prettyLawTitle law,
        "% " <> Plain lawDate,
        " "
      ]
        ++ map (prettyNorm Nothing) lawNorms

prettyNormTitle :: Norm -> Blessings Text
prettyNormTitle Norm {..} = SGR [34] "# " <> SGR [1] (concatWith " – " (Plain <$> normNumber) (Plain <$> stringToMaybe normTitle))

prettyNorm :: Maybe Focus -> Norm -> Blessings Text
prettyNorm focus norm@Norm {..} =
  mconcat . intersperse "\n" $ [prettyNormTitle norm, ""] <> map (maybeHighlight . htmlToPlain) normParagraphs
  where
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
