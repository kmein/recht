module Recht.Options (RechtAction (..), RechtOptions (..), getRechtOptions) where

import Data.Text (Text)
import Options.Applicative

newtype RechtOptions = RechtOptions {rechtAction :: RechtAction}

data RechtAction = Get Text (Maybe Text) | List (Maybe Text) | Random (Maybe Text)

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
