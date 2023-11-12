{-# LANGUAGE RecordWildCards #-}

module Pokemon (PokemonFinal (..), setupPokemon) where

import qualified Ability
import qualified Data.Csv as Csv
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified EggGroup
import qualified GenderRatio
import qualified RawPokemon as Raw
import qualified Type
import Utils
  ( fromCsv,
    fromIdCsvWithId,
    makeMapFromWithIds,
    toIdCsv,
    (?!),
  )

data PokemonFinal = PokemonFinal
  { name :: Text,
    form :: Maybe Text,
    uniqueName :: Text,
    ndex :: Int,
    galarDex :: Maybe Int, -- Galar
    ioaDex :: Maybe Int, -- Isle of Armor
    ctDex :: Maybe Int, -- Crown Tundra
    paldeaDex :: Maybe Int, -- Paldea
    tmDex :: Maybe Int, -- Teal Mask
    type1Id :: Int,
    type2Id :: Maybe Int,
    hp :: Int,
    atk :: Int,
    def :: Int,
    spa :: Int,
    spd :: Int,
    spe :: Int,
    eggGroup1Id :: Int,
    eggGroup2Id :: Maybe Int,
    genderRatioId :: Int,
    ability1Id :: Int,
    ability2Id :: Maybe Int,
    hiddenAbilityId :: Maybe Int,
    eggCycles :: Int
  }
  deriving (Show)

makePokemonFinal ::
  Map Type.Type Int ->
  Map EggGroup.EggGroup Int ->
  Map GenderRatio.GenderRatio Int ->
  Map Text Int ->
  Raw.Pokemon ->
  PokemonFinal
makePokemonFinal typeMap egMap grMap abilityNameMap rp =
  PokemonFinal
    { name = Raw.name rp,
      form = Raw.form rp,
      uniqueName = Raw.uniqueName rp,
      ndex = Raw.ndex rp,
      galarDex = Raw.galarDex rp,
      ioaDex = Raw.ioaDex rp,
      ctDex = Raw.ctDex rp,
      paldeaDex = Raw.paldeaDex rp,
      tmDex = Raw.tmDex rp,
      type1Id = typeMap ?! Raw.type1 rp,
      type2Id = (typeMap ?!) <$> Raw.type2 rp,
      hp = Raw.hp rp,
      atk = Raw.atk rp,
      def = Raw.def rp,
      spa = Raw.spa rp,
      spd = Raw.spd rp,
      spe = Raw.spe rp,
      eggGroup1Id = egMap ?! Raw.eggGroup1 rp,
      eggGroup2Id = (egMap ?!) <$> Raw.eggGroup2 rp,
      genderRatioId = grMap ?! Raw.genderRatio rp,
      ability1Id = abilityNameMap ?! Raw.ability1 rp,
      ability2Id = (abilityNameMap ?!) <$> Raw.ability2 rp,
      hiddenAbilityId = (abilityNameMap ?!) <$> Raw.hiddenAbility rp,
      eggCycles = Raw.eggCycles rp
    }

instance Csv.ToNamedRecord PokemonFinal where
  toNamedRecord (PokemonFinal {..}) =
    Csv.namedRecord
      [ "name" Csv..= name,
        "form" Csv..= form,
        "unique_name" Csv..= uniqueName,
        "ndex" Csv..= ndex,
        "galar_dex" Csv..= galarDex,
        "ioa_dex" Csv..= ioaDex,
        "ct_dex" Csv..= ctDex,
        "paldea_dex" Csv..= paldeaDex,
        "tm_dex" Csv..= tmDex,
        "type1_id" Csv..= type1Id,
        "type2_id" Csv..= type2Id,
        "hp" Csv..= hp,
        "atk" Csv..= atk,
        "def" Csv..= def,
        "spa" Csv..= spa,
        "spd" Csv..= spd,
        "spe" Csv..= spe,
        "egg_group1_id" Csv..= eggGroup1Id,
        "egg_group2_id" Csv..= eggGroup2Id,
        "gr_id" Csv..= genderRatioId,
        "ability1_id" Csv..= ability1Id,
        "ability2_id" Csv..= ability2Id,
        "hidden_ability_id" Csv..= hiddenAbilityId,
        "egg_cycles" Csv..= eggCycles
      ]

instance Csv.FromNamedRecord PokemonFinal where
  parseNamedRecord m =
    PokemonFinal
      <$> m Csv..: "name"
      <*> m Csv..: "form"
      <*> m Csv..: "unique_name"
      <*> m Csv..: "ndex"
      <*> m Csv..: "galar_dex"
      <*> m Csv..: "ioa_dex"
      <*> m Csv..: "ct_dex"
      <*> m Csv..: "paldea_dex"
      <*> m Csv..: "tm_dex"
      <*> m Csv..: "type1_id"
      <*> m Csv..: "type2_id"
      <*> m Csv..: "hp"
      <*> m Csv..: "atk"
      <*> m Csv..: "def"
      <*> m Csv..: "spa"
      <*> m Csv..: "spd"
      <*> m Csv..: "spe"
      <*> m Csv..: "egg_group1_id"
      <*> m Csv..: "egg_group2_id"
      <*> m Csv..: "gr_id"
      <*> m Csv..: "ability1_id"
      <*> m Csv..: "ability2_id"
      <*> m Csv..: "hidden_ability_id"
      <*> m Csv..: "egg_cycles"

instance Csv.DefaultOrdered PokemonFinal where
  headerOrder _ =
    Csv.header
      [ "name",
        "form",
        "unique_name",
        "ndex",
        "galar_dex",
        "ioa_dex",
        "ct_dex",
        "paldea_dex",
        "tm_dex",
        "type1_id",
        "type2_id",
        "hp",
        "atk",
        "def",
        "spa",
        "spd",
        "spe",
        "egg_group1_id",
        "egg_group2_id",
        "gr_id",
        "ability1_id",
        "ability2_id",
        "hidden_ability_id",
        "egg_cycles"
      ]

setupPokemon :: IO ()
setupPokemon = do
  allRawPokemon <- fromCsv "csv/pokemon-raw.csv"
  egMap <- makeMapFromWithIds <$> fromIdCsvWithId "csv/egg-groups.csv"
  typeMap <- makeMapFromWithIds <$> fromIdCsvWithId "csv/types.csv"
  grMap <- makeMapFromWithIds <$> fromIdCsvWithId "csv/gender-ratios.csv"
  abilityNameMap <- M.mapKeys Ability.abilityName . makeMapFromWithIds <$> fromIdCsvWithId "csv/abilities.csv"
  let allPokemon = map (makePokemonFinal typeMap egMap grMap abilityNameMap) allRawPokemon
  toIdCsv "csv/pokemon.csv" allPokemon
