module Ability (Ability (..), setupAbilities) where

import Control.Applicative (empty)
import Control.Exception (throwIO)
import Control.Monad (guard)
import qualified Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.HTML.Scalpel
import Utils (toIdCsv)

getAbilityFlavorText :: Text -> IO Text
getAbilityFlavorText abil = do
  let url =
        "https://pokemondb.net/ability/"
          <> (T.toLower . T.replace " " "-" . T.replace "'" "" $ abil)
  T.putStrLn $ "Scraping " <> url
  let flavorTextScraper :: Scraper Text Text
      flavorTextScraper = chroot ("div" @: [hasClass "grid-col"]) $ inSerial $ do
        -- Find the first h2 that contains "Game descriptions"
        seekNext $ do
          h2 <- text "h2"
          guard $ "Game descriptions" `T.isInfixOf` h2
        -- Pick out the next table
        seekNext $ chroot "table" $ do
          -- Get the last td (corresponding to the latest game)
          tds <- texts "td"
          case tds of
            [] -> empty
            _ -> pure $ last tds
  flavorText <- scrapeURL (T.unpack url) flavorTextScraper
  case flavorText of
    Nothing -> throwIO $ userError $ "Could not scrape " <> T.unpack url
    Just ft -> pure ft

getAllAbilities :: IO [Text]
getAllAbilities = do
  T.putStrLn "Scraping all ability names..."
  let url = "https://pokemondb.net/ability"
  let abilityScraper :: Scraper Text [Text]
      abilityScraper = chroot ("table" @: ["id" @= "abilities"]) $ do
        texts $ "a" @: [hasClass "ent-name"]
  abilities <- scrapeURL (T.unpack url) abilityScraper
  case abilities of
    Nothing -> throwIO $ userError $ "Could not scrape " <> T.unpack url
    Just abils -> pure abils

data Ability = Ability
  { abilityName :: Text,
    abilityFlavorText :: Text
  }
  deriving (Show)

instance Csv.ToNamedRecord Ability where
  toNamedRecord (Ability name flavorText) =
    Csv.namedRecord
      [ "name" Csv..= name,
        "flavor_text" Csv..= flavorText
      ]

instance Csv.FromNamedRecord Ability where
  parseNamedRecord m =
    Ability
      <$> m Csv..: "name"
      <*> m Csv..: "flavor_text"

instance Csv.DefaultOrdered Ability where
  headerOrder _ =
    Csv.header
      [ "name",
        "flavor_text"
      ]

setupAbilities :: IO ()
setupAbilities = do
  abilityNames <- getAllAbilities
  flavorTexts <- mapM getAbilityFlavorText abilityNames
  let abilitiesAndTexts = zipWith Ability abilityNames flavorTexts
  toIdCsv "csv/abilities.csv" abilitiesAndTexts
