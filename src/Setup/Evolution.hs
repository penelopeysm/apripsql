module Setup.Evolution (EvolutionEdgeFinal (..), setupEvolutions) where

import qualified Data.Csv as Csv
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Setup.Pokemon (PokemonFinal (..))
import Setup.RawEvolution (EdgePkmn (..), RawEvolutionTreeEdge (..))
import Utils (fromCsv, fromIdCsvWithId, makeMapFromWithIds, toCsv, (?!))

data EvolutionEdgeFinal = EvolutionEdgeFinal {prevo_id :: Int, evo_id :: Int, base_evo_id :: Int, method :: Text}
  deriving (Eq, Ord, Show)

instance Csv.ToNamedRecord EvolutionEdgeFinal where
  toNamedRecord (EvolutionEdgeFinal prevo_id evo_id base_evo_id method) =
    Csv.namedRecord
      [ ("prevo_id", Csv.toField prevo_id),
        ("evo_id", Csv.toField evo_id),
        ("base_evo_id", Csv.toField base_evo_id),
        ("method", Csv.toField method)
      ]

instance Csv.FromNamedRecord EvolutionEdgeFinal where
  parseNamedRecord m =
    EvolutionEdgeFinal
      <$> m Csv..: "prevo_id"
      <*> m Csv..: "evo_id"
      <*> m Csv..: "base_evo_id"
      <*> m Csv..: "method"

instance Csv.DefaultOrdered EvolutionEdgeFinal where
  headerOrder _ = Csv.header ["prevo_id", "evo_id", "base_evo_id", "method"]

makeEvolutionEdge :: RawEvolutionTreeEdge -> Map EdgePkmn Int -> EvolutionEdgeFinal
makeEvolutionEdge rawEdge pkmnMap =
  EvolutionEdgeFinal
    { prevo_id = pkmnMap ?! rFrom rawEdge,
      evo_id = pkmnMap ?! rTo rawEdge,
      base_evo_id = pkmnMap ?! rBaseEvo rawEdge,
      method = rMethod rawEdge
    }

setupEvolutions :: IO ()
setupEvolutions = do
  rawEvolutionEdges <- fromCsv "csv/evolutions-raw.csv"
  pkmnMap <- M.mapKeys (\p -> EdgePkmn (name p) (form p)) . makeMapFromWithIds <$> fromIdCsvWithId "csv/pokemon.csv"
  let evolutionEdges = sort $ map (`makeEvolutionEdge` pkmnMap) rawEvolutionEdges
  toCsv "csv/evolutions.csv" evolutionEdges
