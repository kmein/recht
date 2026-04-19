module Recht.Options (RechtAction (..), RechtOptions (..), getRechtOptions) where

import Data.Text (Text, unwords)
import Options.Applicative hiding (ParseError)
import Recht.Types (Focus, ParseError, parseFocus)
import Text.Regex.TDFA ((=~))

newtype RechtOptions = RechtOptions {rechtAction :: RechtAction}

data RechtAction = Get Text (Maybe (Either ParseError Focus)) | List (Maybe Text) | Random (Maybe Text) | Dump FilePath (Maybe Text)

rechtArguments :: Parser RechtOptions
rechtArguments =
  RechtOptions
    <$> subparser
      ( mconcat
          [ command "get" $ info ((Get <$> buch <*> optional search) <**> helper) $ progDesc "Gesetze oder Einzelnormen anzeigen",
            command "list" $ info ((List <$> optional buch) <**> helper) $ progDesc "Alle Gesetze bzw. Einzelnormen eines Gesetzes auflisten.",
            command "random" $ info ((Random <$> optional buch) <**> helper) $ progDesc "Eine zufällige Einzelnorm (optional aus einem spezifierten Gesetzbuch) ausgeben.",
            command "dump" $ info ((Dump <$> dumpDirectory <*> optional dumpRegex) <**> helper) $ progDesc "Alle Gesetze in einen Ordner herunterladen."
          ]
      )
  where
    buch = strArgument $ metavar "BUCH" <> help "Abkürzung eines Gesetzbuches"
    dumpDirectory = strArgument $ metavar "PFAD" <> help "Zielordner (wird erstellt wenn nicht vorhanden)"
    dumpRegex = strArgument $ metavar "REGEX" <> help "Nur Gesetze herunterladen, deren Abkürzung diesen Regulären Ausdruck matched"
    search = parseFocus . Data.Text.unwords <$> some (argument str (metavar "ZAHL|TITEL" <> help "Suchbegriff für den Titel der Einzelnorm"))

getRechtOptions :: IO RechtOptions
getRechtOptions = execParser $ info (helper <*> rechtArguments) $ fullDesc <> header "The recht Gesetz-Browser"
