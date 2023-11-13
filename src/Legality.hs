module Legality (RawLegality (..), Legality (..), setupLegalities) where

import qualified Data.Csv as Csv
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Pokemon (uniqueName)
import Utils (fromCsv, fromIdCsvWithId, makeMapFromWithIds, toCsv, (?!))

data RawLegality = RawLegality
  { rlPkmnUniqueName :: Text,
    rlBankBeast :: Int,
    rlBankDream :: Int,
    rlBankApri :: Int,
    rlBankSafari :: Int,
    rlBankSport :: Int,
    rlHomeBeast :: Int,
    rlHomeDream :: Int,
    rlHomeApri :: Int,
    rlHomeSafari :: Int,
    rlHomeSport :: Int
  }
  deriving (Eq, Ord, Show)

instance Csv.ToNamedRecord RawLegality where
  toNamedRecord n =
    Csv.namedRecord
      [ "unique_name" Csv..= rlPkmnUniqueName n,
        "bank_beast" Csv..= rlBankBeast n,
        "bank_dream" Csv..= rlBankDream n,
        "bank_apri" Csv..= rlBankApri n,
        "bank_safari" Csv..= rlBankSafari n,
        "bank_sport" Csv..= rlBankSport n,
        "home_beast" Csv..= rlHomeBeast n,
        "home_dream" Csv..= rlHomeDream n,
        "home_apri" Csv..= rlHomeApri n,
        "home_safari" Csv..= rlHomeSafari n,
        "home_sport" Csv..= rlHomeSport n
      ]

instance Csv.FromNamedRecord RawLegality where
  parseNamedRecord m =
    RawLegality
      <$> m Csv..: "unique_name"
      <*> m Csv..: "bank_beast"
      <*> m Csv..: "bank_dream"
      <*> m Csv..: "bank_apri"
      <*> m Csv..: "bank_safari"
      <*> m Csv..: "bank_sport"
      <*> m Csv..: "home_beast"
      <*> m Csv..: "home_dream"
      <*> m Csv..: "home_apri"
      <*> m Csv..: "home_safari"
      <*> m Csv..: "home_sport"

instance Csv.DefaultOrdered RawLegality where
  headerOrder _ =
    Csv.header
      [ "unique_name",
        "bank_beast",
        "bank_dream",
        "bank_apri",
        "bank_safari",
        "bank_sport",
        "home_beast",
        "home_dream",
        "home_apri",
        "home_safari",
        "home_sport"
      ]

data Legality = Legality
  { pkmnId :: Int,
    bankBeast :: Int,
    bankDream :: Int,
    bankApri :: Int,
    bankSafari :: Int,
    bankSport :: Int,
    homeBeast :: Int,
    homeDream :: Int,
    homeApri :: Int,
    homeSafari :: Int,
    homeSport :: Int
  }
  deriving (Eq, Ord, Show)

instance Csv.ToNamedRecord Legality where
  toNamedRecord n =
    Csv.namedRecord
      [ "pokemon_id" Csv..= pkmnId n,
        "bank_beast" Csv..= bankBeast n,
        "bank_dream" Csv..= bankDream n,
        "bank_apri" Csv..= bankApri n,
        "bank_safari" Csv..= bankSafari n,
        "bank_sport" Csv..= bankSport n,
        "home_beast" Csv..= homeBeast n,
        "home_dream" Csv..= homeDream n,
        "home_apri" Csv..= homeApri n,
        "home_safari" Csv..= homeSafari n,
        "home_sport" Csv..= homeSport n
      ]

instance Csv.FromNamedRecord Legality where
  parseNamedRecord m =
    Legality
      <$> m Csv..: "pokemon_id"
      <*> m Csv..: "bank_beast"
      <*> m Csv..: "bank_dream"
      <*> m Csv..: "bank_apri"
      <*> m Csv..: "bank_safari"
      <*> m Csv..: "bank_sport"
      <*> m Csv..: "home_beast"
      <*> m Csv..: "home_dream"
      <*> m Csv..: "home_apri"
      <*> m Csv..: "home_safari"
      <*> m Csv..: "home_sport"

instance Csv.DefaultOrdered Legality where
  headerOrder _ =
    Csv.header
      [ "pokemon_id",
        "bank_beast",
        "bank_dream",
        "bank_apri",
        "bank_safari",
        "bank_sport",
        "home_beast",
        "home_dream",
        "home_apri",
        "home_safari",
        "home_sport"
      ]

makeAndPatchLegality :: Map Text Int -> RawLegality -> Legality
makeAndPatchLegality pkmnUniqueNameMap rn =
  let uniqueName = rlPkmnUniqueName rn
   in Legality
        { pkmnId = pkmnUniqueNameMap ?! uniqueName,
          bankBeast = rlBankBeast rn,
          bankDream = case uniqueName of
            "gothita" -> 1
            _ -> rlBankDream rn,
          bankApri = rlBankApri rn,
          bankSafari = rlBankSafari rn,
          bankSport = rlBankSport rn,
          homeBeast = rlHomeBeast rn,
          homeDream = rlHomeDream rn,
          homeApri = rlHomeApri rn,
          homeSafari = case uniqueName of
            "tauros-combat" -> 1
            _ -> rlHomeSafari rn,
          homeSport = case uniqueName of
            "tauros-combat" -> 1
            _ -> rlHomeSport rn
        }

setupLegalities :: IO ()
setupLegalities = do
  rawLegalities <- fromCsv "static/legalities-raw.csv"
  pkmnUniqueNameMap <- M.mapKeys uniqueName . makeMapFromWithIds <$> fromIdCsvWithId "csv/pokemon.csv"
  let legalities = map (makeAndPatchLegality pkmnUniqueNameMap) rawLegalities
  toCsv "csv/legalities.csv" legalities
