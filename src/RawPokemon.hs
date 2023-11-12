{-# LANGUAGE RecordWildCards #-}

module RawPokemon (PokemonPartial (..), Pokemon (..), setupRawPokemon) where

import Control.Applicative (optional, (<|>))
import Control.Monad (guard, when)
import qualified Data.Csv as Csv
import Data.List (groupBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified EggGroup
import GenderRatio (GenderRatio (..))
import Text.HTML.Scalpel
import qualified Type
import Utils (readMaybeInt, toIdCsv)

-- | A @PokemonPartial@ represents the data that we can scrape from the page
-- containing all Pokemon: https://pokemondb.net/pokedex/all
data PokemonPartial = PokemonPartial
  { pname :: Text,
    pform :: Maybe Text,
    pformNum :: Int, -- 0 for base form, 1 for first alternate form, etc.
    pmonUrl :: Text -- The URL of the page for this Pokemon.
  }
  deriving (Eq, Ord, Show)

data Pokemon = Pokemon
  { name :: Text,
    form :: Maybe Text,
    uniqueName :: Text,
    ndex :: Int,
    galarDex :: Maybe Int, -- Galar
    ioaDex :: Maybe Int, -- Isle of Armor
    ctDex :: Maybe Int, -- Crown Tundra
    paldeaDex :: Maybe Int, -- Paldea
    tmDex :: Maybe Int, -- Teal Mask
    -- dexSvDlc2 :: Maybe Int,  -- Enable later
    type1 :: Type.Type,
    type2 :: Maybe Type.Type,
    hp :: Int,
    atk :: Int,
    def :: Int,
    spa :: Int,
    spd :: Int,
    spe :: Int,
    eggGroup1 :: EggGroup.EggGroup,
    eggGroup2 :: Maybe EggGroup.EggGroup,
    genderRatio :: GenderRatio,
    ability1 :: Text,
    ability2 :: Maybe Text,
    hiddenAbility :: Maybe Text,
    eggCycles :: Int,
    url :: Text
  }
  deriving (Show)

-- | Fetch all PokemonPartial from https://pokemondb.net/pokedex/all. Note that
-- pformNum is not populated at this point (it is set to 0 here and then filled
-- in by patchPokemonPartial).
getAllPokemonPartial :: IO [PokemonPartial]
getAllPokemonPartial = do
  let url = "https://pokemondb.net/pokedex/all"
  let partialPokemonScraper :: Scraper Text [PokemonPartial]
      partialPokemonScraper = chroots ("table" @: ["id" @= "pokedex"] // "tr") $ do
        species <- text ("a" @: [hasClass "ent-name"])
        monUrl <- ("https://pokemondb.net" <>) <$> attr "href" ("a" @: [hasClass "ent-name"])
        form <- optional $ text ("small" @: [hasClass "text-muted"])
        -- Temporarily set formNum to 0
        pure $ PokemonPartial species form 0 monUrl
  maybePkmnPartial <- scrapeURL (T.unpack url) partialPokemonScraper
  case maybePkmnPartial of
    Nothing -> error $ "Failed to scrape " <> T.unpack url
    Just pkmnPartial -> pure pkmnPartial

errorPkmn :: PokemonPartial -> Text -> a
errorPkmn ppkmn msg =
  error $
    T.unpack $
      "Error scraping "
        <> pname ppkmn
        <> maybe "" ("-" <>) (pform ppkmn)
        <> ": "
        <> msg

getPokemon :: PokemonPartial -> IO Pokemon
getPokemon ppkmn = do
  T.putStrLn $ "Scraping " <> pname ppkmn <> maybe "" ("-" <>) (pform ppkmn)
  let haScraper :: Scraper Text Pokemon
      haScraper = do
        -- Check that we are looking at the correct form
        forms <- texts ("a" @: [hasClass "sv-tabs-tab"])
        let expectedForm = forms !! pformNum ppkmn
        case pform ppkmn of
          Nothing -> pure ()
          Just form ->
            when (form /= expectedForm) $
              errorPkmn ppkmn $
                "Expected form '" <> expectedForm <> "', but got " <> form
        -- National dex number
        ndexs <- chroots ("table" @: [hasClass "vitals-table"] // "tr") $ do
          heading <- text "th"
          guard $ heading == "National №"
          maybeNd <- readMaybeInt <$> text "td"
          case maybeNd of
            Nothing -> errorPkmn ppkmn "Could not parse National Dex number"
            Just nd -> pure nd
        let ndex = ndexs !! pformNum ppkmn
        -- Regional dex numbers
        let parseRegionalDexId :: Text -> Text -> Maybe Int
            parseRegionalDexId dexName tdContents =
              let dexes = map (map T.strip . T.splitOn "(") . T.splitOn ")" $ tdContents
                  thisDex =
                    filter
                      ( \case
                          [num, name] -> name == dexName
                          _ -> False
                      )
                      dexes
               in case thisDex of
                    [[num, _]] -> readMaybeInt num
                    _ -> Nothing
        let readRegionalDex :: Text -> Scraper Text (Maybe Int)
            readRegionalDex dexName = do
              allRegionalDexes <- chroots ("table" @: [hasClass "vitals-table"] // "tr") $ do
                heading <- text "th"
                guard $ heading == "Local №"
                parseRegionalDexId dexName <$> text "td"
              pure $ allRegionalDexes !! pformNum ppkmn
        galarDex <- readRegionalDex "Sword/Shield"
        ioaDex <- readRegionalDex "The Isle of Armor"
        ctDex <- readRegionalDex "The Crown Tundra"
        paldeaDex <- readRegionalDex "Scarlet/Violet"
        tmDex <- readRegionalDex "The Teal Mask"
        -- Types
        typess <- chroots ("table" @: [hasClass "vitals-table"] // "tr") $ do
          heading <- text "th"
          guard $ heading == "Type"
          ts <- map (Type.fromString . T.unpack) <$> texts ("a" @: [hasClass "type-icon"])
          case ts of
            [Just t1] -> pure (t1, Nothing)
            [Just t1, Just t2] -> pure (t1, Just t2)
            _ -> errorPkmn ppkmn "Pokemon had invalid number of types, or invalid type names"
        let (t1, t2) = typess !! pformNum ppkmn
        -- Abilities
        abils <- chroots ("table" @: [hasClass "vitals-table"] // "tr") $ do
          heading <- text "th"
          guard $ heading == "Abilities"
          nas <- texts ("span" @: [hasClass "text-muted"] // "a")
          ha <- optional $ text ("small" @: [hasClass "text-muted"] // "a")
          pure $ case nas of
            [na1] -> (na1, Nothing, ha)
            [na1, na2] -> (na1, Just na2, ha)
            _ -> errorPkmn ppkmn "No ability found"
        let (na1, na2, ha) = case pname ppkmn of
                                  -- PokemonDB lacking data on other forms
                                  "Tatsugiri" -> ("Commander", Nothing, Just "Storm Drain")
                                  _ -> abils !! pformNum ppkmn
        -- Base stats"
        let readStat :: Text -> Scraper Text Int
            readStat heading = do
              allFormStats <- chroots ("table" @: [hasClass "vitals-table"] // "tr") $ do
                th <- text "th"
                guard $ th == heading
                stat <- readMaybeInt <$> text ("td" @: [hasClass "cell-num"])
                case stat of
                  Nothing -> errorPkmn ppkmn $ "Could not parse " <> heading
                  Just s -> pure s
              pure $ allFormStats !! pformNum ppkmn
        hp <- readStat "HP"
        atk <- readStat "Attack"
        def <- readStat "Defense"
        spa <- readStat "Sp. Atk"
        spd <- readStat "Sp. Def"
        spe <- readStat "Speed"
        -- Egg groups (these don't differ by form, so we don't need the !! at
        -- the end)
        eggGroups <- chroot ("table" @: [hasClass "vitals-table"] // "tr") $ do
          heading <- text "th"
          guard $ heading == "Egg Groups"
          map (EggGroup.fromString . T.unpack) <$> texts "a"
        let (egg1, egg2) = case eggGroups of
              [Just e1] -> (e1, Nothing)
              [Just e1, Just e2] -> (e1, Just e2)
              _ -> errorPkmn ppkmn "Pokemon had invalid number of egg groups"
        -- And the gender ratio and egg cycles (which don't differ by form)
        genderRatio <- case pname ppkmn of
          -- Hardcoded for things that aren't on PokemonDB (yet?)
          s | s `elem` ["Walking Wake", "Iron Leaves"] -> pure Genderless
          _ -> chroot ("table" @: [hasClass "vitals-table"] // "tr") $ do
            heading <- text "th"
            guard $ heading == "Gender"
            ratio <- text ("span" @: [hasClass "text-blue"]) <|> text "td"
            pure $ case ratio of
              "Genderless" -> Genderless
              "—" -> Genderless
              "0% male" -> FemaleOnly
              "12.5% male" -> Female71
              "25% male" -> Female31
              "50% male" -> Equal
              "75% male" -> Male31
              "87.5% male" -> Male71
              "100% male" -> MaleOnly
              _ -> errorPkmn ppkmn ("Could not determine gender ratio: got " <> ratio)
        eggCycles <- case pname ppkmn of
          -- Hardcoded for things that aren't on PokemonDB (yet?)
          s | s `elem` ["Walking Wake", "Iron Leaves"] -> pure 50
          s | s `elem` ["Dipplin", "Poltchageist", "Sinistcha"] -> pure 20
          s | s `elem` ["Okidogi", "Munkidori", "Fezandipiti"] -> pure 120
          "Ogerpon" -> pure 10
          _ -> chroot ("table" @: [hasClass "vitals-table"] // "tr") $ do
            heading <- text ("th" // "a")
            guard $ heading == "Egg cycles"
            ec <- readMaybeInt . head . T.words <$> text "td"
            pure $ case ec of
              Just ec' -> ec'
              Nothing -> errorPkmn ppkmn "Could not determine egg cycles"
        pure $
          Pokemon
            { name = pname ppkmn,
              form = pform ppkmn,
              uniqueName = makeUniqueName (pname ppkmn) (pform ppkmn),
              ndex = ndex,
              galarDex = galarDex,
              ioaDex = ioaDex,
              ctDex = ctDex,
              paldeaDex = paldeaDex,
              tmDex = tmDex,
              type1 = t1,
              type2 = t2,
              hp = hp,
              atk = atk,
              def = def,
              spa = spa,
              spd = spd,
              spe = spe,
              eggGroup1 = egg1,
              eggGroup2 = egg2,
              genderRatio = genderRatio,
              ability1 = na1,
              ability2 = na2,
              hiddenAbility = ha,
              eggCycles = eggCycles,
              url = pmonUrl ppkmn
            }
  maybePkmn <- scrapeURL (T.unpack $ pmonUrl ppkmn) haScraper
  case maybePkmn of
    Just pkmn -> pure pkmn
    Nothing -> errorPkmn ppkmn "Could not scrape Pokemon"

instance Csv.ToNamedRecord Pokemon where
  toNamedRecord (Pokemon {..}) =
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
        "type1" Csv..= type1,
        "type2" Csv..= type2,
        "hp" Csv..= hp,
        "atk" Csv..= atk,
        "def" Csv..= def,
        "spa" Csv..= spa,
        "spd" Csv..= spd,
        "spe" Csv..= spe,
        "egg_group1" Csv..= eggGroup1,
        "egg_group2" Csv..= eggGroup2,
        "gender_ratio" Csv..= genderRatio,
        "ability1" Csv..= ability1,
        "ability2" Csv..= ability2,
        "hidden_ability" Csv..= hiddenAbility,
        "egg_cycles" Csv..= eggCycles,
        "url" Csv..= url
      ]

instance Csv.FromNamedRecord Pokemon where
  parseNamedRecord m =
    Pokemon
      <$> m Csv..: "name"
      <*> m Csv..: "form"
      <*> m Csv..: "unique_name"
      <*> m Csv..: "ndex"
      <*> m Csv..: "galar_dex"
      <*> m Csv..: "ioa_dex"
      <*> m Csv..: "ct_dex"
      <*> m Csv..: "paldea_dex"
      <*> m Csv..: "tm_dex"
      <*> m Csv..: "type1"
      <*> m Csv..: "type2"
      <*> m Csv..: "hp"
      <*> m Csv..: "atk"
      <*> m Csv..: "def"
      <*> m Csv..: "spa"
      <*> m Csv..: "spd"
      <*> m Csv..: "spe"
      <*> m Csv..: "egg_group1"
      <*> m Csv..: "egg_group2"
      <*> m Csv..: "gender_ratio"
      <*> m Csv..: "ability1"
      <*> m Csv..: "ability2"
      <*> m Csv..: "hidden_ability"
      <*> m Csv..: "egg_cycles"
      <*> m Csv..: "url"

instance Csv.DefaultOrdered Pokemon where
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
        "type1",
        "type2",
        "hp",
        "atk",
        "def",
        "spa",
        "spd",
        "spe",
        "egg_group1",
        "egg_group2",
        "gender_ratio",
        "ability1",
        "ability2",
        "hidden_ability",
        "egg_cycles",
        "url"
      ]

-- | Programmatically generate unique_name field from a Pokemon's name and form.
makeUniqueName :: Text -> Maybe Text -> Text
makeUniqueName nm' fm =
  let nm = case nm' of
        "Mr. Mime" -> "mr-mime"
        "Mr. Rime" -> "mr-rime"
        "Mime Jr." -> "mime-jr"
        "Farfetch'd" -> "farfetchd"
        "Sirfetch'd" -> "sirfetchd"
        "Nidoran♀" -> "nidoran-female"
        "Nidoran♂" -> "nidoran-male"
        "Flabébé" -> "flabebe"
        p -> p
   in T.replace ":" ""         -- Type: Null
        . T.replace "%" ""     -- Zygarde {10,50}%
        . T.replace "'" ""     -- Oricorio Pa'u
        . T.replace " " "-"
        . T.strip
        . T.toLower
        $ case fm of
          Nothing -> nm
          Just f
            -- Darmanitan is SPECIAL
            | f == "Standard Mode" -> nm
            | f == "Zen Mode" -> nm <> "-zen"
            | f == "Galarian Standard Mode" -> nm <> "-galar"
            | f == "Galarian Zen Mode" -> nm <> "-galar-zen"
            -- Catch-all cases
            | "Alolan " `T.isPrefixOf` f -> nm <> "-alola"
            | "Galarian " `T.isPrefixOf` f -> nm <> "-galar"
            | "Hisuian " `T.isPrefixOf` f -> nm <> "-hisui"
            | "Paldean " `T.isPrefixOf` f -> nm <> "-paldea"
            | "Hoopa " `T.isPrefixOf` f -> nm <> "-" <> T.replace "Hoopa " "" f
            | "Crowned " `T.isPrefixOf` f -> nm <> "-crowned"
            | "Mega " `T.isPrefixOf` f && "X" `T.isSuffixOf` f -> nm <> "-mega-x"
            | "Mega " `T.isPrefixOf` f && "Y" `T.isSuffixOf` f -> nm <> "-mega-y"
            | "Mega " `T.isPrefixOf` f -> nm <> "-mega"
            | "Primal " `T.isPrefixOf` f -> nm <> "-primal"
            | "Partner " `T.isPrefixOf` f -> nm <> "-partner"
            | " Forme" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Forme" "" f
            | " -Striped Form" `T.isSuffixOf` f -> nm <> "-" <> T.replace " -Striped Form" "" f
            | " Form" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Form" "" f
            | " Breed" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Breed" "" f
            | " Cloak" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Cloak" "" f
            | " Rider" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Rider" "" f
            | " Rotom" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Rotom" "" f
            | " Necrozma" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Necrozma" "" f
            | " Kyurem" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Kyurem" "" f
            | " Mask" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Mask" "" f
            | " Mode" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Mode" "" f
            | " Size" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Size" "" f
            | " Plumage" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Plumage" "" f
            | " Style" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Style" "" f
            | " Face" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Face" "" f
            -- Specific stuff
            | f == "Ash-Greninja" -> "greninja-ash"
            | f == "Own Tempo Rockruff" -> "rockruff-dusk"
            | f == "Male" -> nm <> "-male"
            | f == "Hero of Many Battles" -> nm -- Z/Z base forms
            | f == "Female" -> nm <> "-female"
            | f == "Eternamax" -> nm <> "-eternamax"
            | f == "Bloodmoon" -> nm <> "-bloodmoon"
            | f == "Family of Three" -> nm <> "-family-of-three"
            | f == "Family of Four" -> nm <> "-family-of-four"
            | otherwise -> error $ "Don't know what to do with: " <> show nm <> " (" <> show f <> ")"

patchPokemonPartial :: [PokemonPartial] -> IO [PokemonPartial]
patchPokemonPartial pps = do
  -- Add in formNums
  let groupedMons = groupBy (\a b -> pname a == pname b) pps
  let pps2 = concatMap (\ms -> zipWith (\m i -> m {pformNum = i}) ms [0 ..]) groupedMons
  -- Remove Mega evolutions, Partner Pikachu, Partner Eevee, Ash-Greninja,
  -- Primal Groudon/Kyogre
  let formContains :: Text -> PokemonPartial -> Bool
      formContains str pp = case pform pp of
        Nothing -> False
        Just f -> str `T.isInfixOf` f
  let pps3 =
        filter
          ( \pp ->
              not $
                any
                  (`formContains` pp)
                  ["Mega ", "Partner Pikachu", "Partner Eevee", "Ash-Greninja", "Primal ", "Eternamax"]
          )
          pps2
  -- Add in DLC Pokemon, because these aren't available on the Pokedex site yet
  let tealMaskPokemon :: [PokemonPartial]
      tealMaskPokemon =
        [ PokemonPartial "Dipplin" Nothing 0 "https://pokemondb.net/pokedex/dipplin",
          PokemonPartial "Poltchageist" Nothing 0 "https://pokemondb.net/pokedex/poltchageist",
          PokemonPartial "Sinistcha" Nothing 0 "https://pokemondb.net/pokedex/sinistcha",
          PokemonPartial "Okidogi" Nothing 0 "https://pokemondb.net/pokedex/okidogi",
          PokemonPartial "Munkidori" Nothing 0 "https://pokemondb.net/pokedex/munkidori",
          PokemonPartial "Fezandipiti" Nothing 0 "https://pokemondb.net/pokedex/fezandipiti",
          PokemonPartial "Ogerpon" (Just "Teal Mask") 0 "https://pokemondb.net/pokedex/ogerpon",
          PokemonPartial "Ogerpon" (Just "Wellspring Mask") 1 "https://pokemondb.net/pokedex/ogerpon",
          PokemonPartial "Ogerpon" (Just "Hearthflame Mask") 2 "https://pokemondb.net/pokedex/ogerpon",
          PokemonPartial "Ogerpon" (Just "Cornerstone Mask") 3 "https://pokemondb.net/pokedex/ogerpon"
        ]
  pure $ pps3 <> tealMaskPokemon

setupRawPokemon :: IO ()
setupRawPokemon = do
  getAllPokemonPartial
    >>= patchPokemonPartial
    >>= mapM getPokemon
    >>= toIdCsv "csv/pokemon-raw.csv"
