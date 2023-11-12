module RawPokemon (PokemonPartial (..), Pokemon (..), setupRawPokemon) where

import Control.Applicative (optional, (<|>))
import Control.Monad (guard)
import Data.List (groupBy)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GenderRatio (GenderRatio (..))
import Text.HTML.Scalpel
import Utils (readInt)

data PokemonPartial = PokemonPartial
  { pname :: Text,
    pform :: Maybe Text,
    pformNum :: Int, -- 0 for base form, 1 for first alternate form, etc.
    pndex :: Int,
    ptype1 :: Text,
    ptype2 :: Maybe Text,
    pmonUrl :: Text
  }
  deriving (Show)

data Pokemon = Pokemon
  { -- From Pokedex page (PokemonPartial)
    name :: Text,
    form :: Maybe Text,
    ndex :: Int,
    type1 :: Text,
    type2 :: Maybe Text,
    -- From individual Pokemon page
    eggGroup1 :: Text,
    eggGroup2 :: Maybe Text,
    genderRatio :: GenderRatio,
    hiddenAbility :: Maybe Text
  }
  deriving (Show)

getAllPokemonPartial :: IO [PokemonPartial]
getAllPokemonPartial = do
  let url = "https://pokemondb.net/pokedex/all"
  let partialPokemonScraper :: Scraper Text [PokemonPartial]
      partialPokemonScraper = chroots ("table" @: ["id" @= "pokedex"] // "tr") $ do
        ndex <-
          readInt
            <$> text ("span" @: [hasClass "infocard-cell-data"])
        species <- text ("a" @: [hasClass "ent-name"])
        monUrl <- ("https://pokemondb.net" <>) <$> attr "href" ("a" @: [hasClass "ent-name"])
        form <- optional $ text ("small" @: [hasClass "text-muted"])
        types <- texts ("a" @: [hasClass "type-icon"])
        let (t1, t2) = case types of
              [t1] -> (t1, Nothing)
              [t1, t2] -> (t1, Just t2)
              _ -> error "Pokemon had invalid number of types"
        -- Temporarily set formNum to 0
        pure $ PokemonPartial species form 0 ndex t1 t2 monUrl
  partialMons <- fromJust <$> scrapeURL (T.unpack url) partialPokemonScraper
  -- Add in formNums
  let groupedMons = groupBy (\a b -> pname a == pname b) partialMons
  pure $ concatMap (\ms -> zipWith (\m i -> m {pformNum = i}) ms [0 ..]) groupedMons

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
        guard $ case pform ppkmn of
          Nothing -> True
          Just form -> form == (forms !! pformNum ppkmn)
        -- If so, grab the HA
        has <- chroots ("table" @: [hasClass "vitals-table"] // "tr") $ do
          heading <- text "th"
          guard $ heading == "Abilities"
          optional $ text ("small" @: [hasClass "text-muted"] // "a")
        let ha = has !! pformNum ppkmn
        -- And the egg groups (which don't differ by form)
        eggGroups <- chroot ("table" @: [hasClass "vitals-table"] // "tr") $ do
          heading <- text "th"
          guard $ heading == "Egg Groups"
          texts "a"
        let (egg1, egg2) = case eggGroups of
              [e1] -> (e1, Nothing)
              [e1, e2] -> (e1, Just e2)
              _ -> errorPkmn ppkmn "Pokemon had invalid number of egg groups"
        -- And the gender ratio (which doesn't differ by form)
        genderRatio <- chroot ("table" @: [hasClass "vitals-table"] // "tr") $ do
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
        pure $
          Pokemon
            { name = pname ppkmn,
              form = pform ppkmn,
              ndex = pndex ppkmn,
              type1 = ptype1 ppkmn,
              type2 = ptype2 ppkmn,
              eggGroup1 = egg1,
              eggGroup2 = egg2,
              genderRatio = genderRatio,
              hiddenAbility = ha
            }
  fromJust <$> scrapeURL (T.unpack $ pmonUrl ppkmn) haScraper

setupRawPokemon :: IO ()
setupRawPokemon = putStrLn "Not implemented"
