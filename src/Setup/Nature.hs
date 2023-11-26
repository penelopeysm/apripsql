module Setup.Nature (setupNatures) where

import qualified Data.Csv as Csv
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Setup.Pokemon (uniqueName)
import Utils (fromCsv, fromIdCsvWithId, makeMapFromWithIds, toCsv, (?!))

data RawNature = RawNature
  { rnPkmnUniqueName :: Text,
    rnPenny :: Text,
    rnJemmaSwSh :: Text,
    rnJemmaBDSP :: Text,
    rnJemmaG7 :: Text
  }
  deriving (Eq, Ord, Show)

instance Csv.FromNamedRecord RawNature where
  parseNamedRecord m =
    RawNature
      <$> m Csv..: "unique_name"
      <*> m Csv..: "penny"
      <*> m Csv..: "jemma_swsh"
      <*> m Csv..: "jemma_bdsp"
      <*> m Csv..: "jemma_g7"

instance Csv.DefaultOrdered RawNature where
  headerOrder _ =
    Csv.header
      [ "unique_name",
        "penny",
        "jemma_swsh",
        "jemma_bdsp",
        "jemma_g7"
      ]

data Nature = Nature
  { pkmnId :: Int,
    penny :: Text,
    jemmaSwSh :: Text,
    jemmaBDSP :: Text,
    jemmaG7 :: Text
  }
  deriving (Eq, Ord, Show)

instance Csv.ToNamedRecord Nature where
  toNamedRecord n =
    Csv.namedRecord
      [ "pokemon_id" Csv..= pkmnId n,
        "penny" Csv..= penny n,
        "jemma_swsh" Csv..= jemmaSwSh n,
        "jemma_bdsp" Csv..= jemmaBDSP n,
        "jemma_g7" Csv..= jemmaG7 n
      ]

instance Csv.DefaultOrdered Nature where
  headerOrder _ =
    Csv.header
      [ "pokemon_id",
        "penny",
        "jemma_swsh",
        "jemma_bdsp",
        "jemma_g7"
      ]

makeNature :: Map Text Int -> RawNature -> Nature
makeNature pkmnUniqueNameMap rn =
  Nature
    { pkmnId = pkmnUniqueNameMap ?! rnPkmnUniqueName rn,
      penny = rnPenny rn,
      jemmaSwSh = rnJemmaSwSh rn,
      jemmaBDSP = rnJemmaBDSP rn,
      jemmaG7 = rnJemmaG7 rn
    }

setupNatures :: IO ()
setupNatures = do
  rawNatures <- fromCsv "static/natures-raw.csv"
  pkmnUniqueNameMap <- M.mapKeys uniqueName . makeMapFromWithIds <$> fromIdCsvWithId "csv/pokemon.csv"
  let natures = map (makeNature pkmnUniqueNameMap) rawNatures
  toCsv "csv/natures.csv" natures
