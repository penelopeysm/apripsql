module Setup.RawEvolution (EdgePkmn (..), RawEvolutionTreeEdge (..), setupRawEvolutions) where

import Control.Applicative (some, (<|>))
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.Scalpel
import Utils (toCsv)

data EdgePkmn = EdgePkmn {epName :: Text, epForm :: Maybe Text}
  deriving (Eq, Ord, Show)

-- | Would be nice to use NonEmpty here, but too lazy.
data EvolutionTree = Node EdgePkmn [(Text, EvolutionTree)] | Leaf EdgePkmn
  deriving (Eq, Ord, Show)

-- | Get the prevo at the top of a tree
getPrevo :: EvolutionTree -> EdgePkmn
getPrevo (Leaf p) = p
getPrevo (Node p _) = p

data RawEvolutionTreeEdge = RawEvolutionTreeEdge
  { rFrom :: EdgePkmn,
    rTo :: EdgePkmn,
    rBaseEvo :: EdgePkmn,
    rMethod :: Text
  }
  deriving (Eq, Ord, Show)

instance Csv.ToNamedRecord RawEvolutionTreeEdge where
  toNamedRecord (RawEvolutionTreeEdge prevo evo baseEvo method) =
    Csv.namedRecord
      [ "prevo_name" Csv..= epName prevo,
        "prevo_form" Csv..= epForm prevo,
        "evo_name" Csv..= epName evo,
        "evo_form" Csv..= epForm evo,
        "base_evo_name" Csv..= epName baseEvo,
        "base_evo_form" Csv..= epForm baseEvo,
        "method" Csv..= method
      ]

instance Csv.FromNamedRecord RawEvolutionTreeEdge where
  parseNamedRecord m = do
    prevo <- EdgePkmn <$> m Csv..: "prevo_name" <*> m Csv..: "prevo_form"
    evo <- EdgePkmn <$> m Csv..: "evo_name" <*> m Csv..: "evo_form"
    baseEvo <- EdgePkmn <$> m Csv..: "base_evo_name" <*> m Csv..: "base_evo_form"
    method <- m Csv..: "method"
    pure $ RawEvolutionTreeEdge prevo evo baseEvo method

instance Csv.DefaultOrdered RawEvolutionTreeEdge where
  headerOrder _ =
    Csv.header
      [ "prevo_name",
        "prevo_form",
        "evo_name",
        "evo_form",
        "base_evo_name",
        "base_evo_form",
        "method"
      ]

-- | Get all pairs of prevos and evos
getPairs :: EvolutionTree -> [RawEvolutionTreeEdge]
getPairs (Leaf _) = []
getPairs (Node p chains) =
  let getPairsWithBase :: EdgePkmn -> EvolutionTree -> [RawEvolutionTreeEdge]
      getPairsWithBase _ (Leaf _) = []
      getPairsWithBase base (Node p chains') =
        map (\(mtd, tree) -> RawEvolutionTreeEdge p (getPrevo tree) base mtd) chains'
          ++ concatMap (getPairsWithBase base . snd) chains'
   in getPairsWithBase p (Node p chains)

getPkmnFromInfocard :: Selector -> ScraperT Text IO EdgePkmn
getPkmnFromInfocard _ =
  chroot ("span" @: [hasClass "infocard-lg-data", hasClass "text-muted"]) $ do
    name <- text $ "a" @: [hasClass "ent-name"]
    smalls <- texts "small"
    let form = case smalls of
          [_, fm, _] -> Just fm
          _ -> Nothing
    pure $ EdgePkmn name form

getEvoMethod :: Selector -> ScraperT Text IO Text
getEvoMethod _ = text "small"

unParens :: Text -> Text
unParens = T.dropWhileEnd (== ')') . T.dropWhile (== '(')

leafScraper :: SerialScraperT Text IO EvolutionTree
leafScraper = do
  pkmn <- seekNext $ getPkmnFromInfocard ("div" @: [hasClass "infocard"])
  pure $ Leaf pkmn

linearScraper :: SerialScraperT Text IO EvolutionTree
linearScraper = do
  prevo <- seekNext $ getPkmnFromInfocard ("div" @: [hasClass "infocard"])
  method <- seekNext $ getEvoMethod ("span" @: [hasClass "infocard-arrow"])
  evo <- branchedScraper <|> linearScraper <|> leafScraper
  pure $ Node prevo [(unParens method, evo)]

branchedScraper :: SerialScraperT Text IO EvolutionTree
branchedScraper = do
  let innerScraper :: Selector -> ScraperT Text IO [(Text, EvolutionTree)]
      innerScraper _ = inSerial $ do
        let inner' _ = inSerial $ do
              method <- seekNext $ getEvoMethod ("span" @: [hasClass "infocard-arrow"])
              evo <- linearScraper <|> leafScraper
              pure (unParens method, evo)
        some $ seekNext $ inner' ("div" @: [hasClass "infocard-list-evo"])
  prevo <- seekNext $ getPkmnFromInfocard ("div" @: [hasClass "infocard"])
  evos <- seekNext $ innerScraper ("span" @: [hasClass "infocard-evo-split"])
  guard $ not (null evos)
  -- Hardcode Shedinja because PokemonDB lists it as Nincada evolving into
  -- "Ninjask plus Shedinja" and only Ninjask gets parsed.
  let evos' = case epName prevo of
        "Nincada" -> map (\(mtd, evo) -> if "empty spot" `T.isInfixOf` mtd then (mtd, Leaf (EdgePkmn "Shedinja" Nothing)) else (mtd, evo)) evos
        _ -> evos
  pure $ Node prevo evos'

scraper :: ScraperT Text IO [EvolutionTree]
scraper =
  chroots ("div" @: [hasClass "infocard-filter-block"] // ("div" @: [hasClass "infocard-list-evo"]) `atDepth` 1) $
    inSerial $
      branchedScraper <|> linearScraper <|> leafScraper

getRawEvolutions :: IO [EvolutionTree]
getRawEvolutions = do
  let url = "https://pokemondb.net/evolution"
  tags <- fetchTags (T.unpack url)
  evos <- scrapeT scraper tags
  case evos of
    Just evos' -> pure evos'
    Nothing -> error "Failed to scrape evolution trees"

patch :: [RawEvolutionTreeEdge] -> [RawEvolutionTreeEdge]
patch =
  let mothim = EdgePkmn "Mothim" Nothing
      burmy t = EdgePkmn "Burmy" (Just t)
      honedge = EdgePkmn "Honedge" Nothing
      doublade = EdgePkmn "Doublade" Nothing
      aegislash t = EdgePkmn "Aegislash" (Just t)
      pumpkaboo t = EdgePkmn "Pumpkaboo" (Just t)
      gourgeist t = EdgePkmn "Gourgeist" (Just t)
   in -- Separate Burmy "All forms"
      filter (/= RawEvolutionTreeEdge (burmy "All forms") mothim (burmy "All forms") "Level 20, Male")
        . ( <>
              [ RawEvolutionTreeEdge (burmy "Plant Cloak") mothim (burmy "Plant Cloak") "Level 20, Male",
                RawEvolutionTreeEdge (burmy "Sandy Cloak") mothim (burmy "Sandy Cloak") "Level 20, Male",
                RawEvolutionTreeEdge (burmy "Trash Cloak") mothim (burmy "Trash Cloak") "Level 20, Male"
              ]
          )
        -- Separate Aegislash forms
        . filter (/= RawEvolutionTreeEdge doublade (EdgePkmn "Aegislash" Nothing) honedge "use Dusk Stone")
        . ( <>
              [ RawEvolutionTreeEdge doublade (aegislash "Shield Forme") honedge "use Dusk Stone",
                RawEvolutionTreeEdge doublade (aegislash "Blade Forme") honedge "use Dusk Stone"
              ]
          )
        -- Separate Pumpkaboo sizes forms
        . filter (/= RawEvolutionTreeEdge (EdgePkmn "Pumpkaboo" Nothing) (EdgePkmn "Gourgeist" Nothing) (EdgePkmn "Pumpkaboo" Nothing) "Trade")
        . ( <>
              [ RawEvolutionTreeEdge (pumpkaboo "Small Size") (gourgeist "Small Size") (pumpkaboo "Small Size") "Trade",
                RawEvolutionTreeEdge (pumpkaboo "Average Size") (gourgeist "Average Size") (pumpkaboo "Average Size") "Trade",
                RawEvolutionTreeEdge (pumpkaboo "Large Size") (gourgeist "Large Size") (pumpkaboo "Large Size") "Trade",
                RawEvolutionTreeEdge (pumpkaboo "Super Size") (gourgeist "Super Size") (pumpkaboo "Super Size") "Trade"
              ]
          )

setupRawEvolutions :: IO ()
setupRawEvolutions = do
  rawEvos <- getRawEvolutions
  let edges = patch $ concatMap getPairs rawEvos
  toCsv "csv/evolutions-raw.csv" edges
