module Setup.Alias (setupAliases) where

import qualified Data.Csv as Csv
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Setup.Pokemon (uniqueName)
import Utils (fromCsv, fromIdCsvWithId, makeMapFromWithIds, toCsv, (?!))

data RawAlias = RawAlias
  { rawAliasSearchString :: Text,
    rawAliasUniqueName :: Text
  }
  deriving (Show)

instance Csv.FromNamedRecord RawAlias where
  parseNamedRecord m =
    RawAlias
      <$> m Csv..: "alias"
      <*> m Csv..: "unique_name"

instance Csv.DefaultOrdered RawAlias where
  headerOrder _ = Csv.header ["alias", "unique_name"]

data Alias = Alias
  { aliasSearchString :: Text,
    aliasId :: Int
  }

instance Csv.ToNamedRecord Alias where
  toNamedRecord (Alias searchString id) =
    Csv.namedRecord
      [ "alias" Csv..= searchString,
        "pokemon_id" Csv..= id
      ]

instance Csv.DefaultOrdered Alias where
  headerOrder _ = Csv.header ["alias", "pokemon_id"]

makeAlias :: Map Text Int -> RawAlias -> Alias
makeAlias pkmnUniqueNameMap rawAlias =
  Alias
    { aliasSearchString = rawAliasSearchString rawAlias,
      aliasId = pkmnUniqueNameMap ?! rawAliasUniqueName rawAlias
    }

setupAliases :: IO ()
setupAliases = do
  rawAliases <- fromCsv "static/aliases.csv"
  pkmnUniqueNameMap <- M.mapKeys uniqueName . makeMapFromWithIds <$> fromIdCsvWithId "csv/pokemon.csv"
  let aliases = map (makeAlias pkmnUniqueNameMap) rawAliases
  toCsv "csv/aliases.csv" aliases
