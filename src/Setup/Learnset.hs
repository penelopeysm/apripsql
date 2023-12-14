module Setup.Learnset (setupLearnsets) where

import qualified Data.Csv as Csv
import Data.HashMap.Strict (union)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Setup.Game (Game (..))
import qualified Setup.LearnMethod as LM
import Setup.Move (MoveFinal (..))
import Setup.Pokemon (PokemonFinal (..))
import Setup.RawLearnset (LearnMethodWithLevel (..), LearnedMove (..), LearnsetEntry (..))
import Utils (fromCsv, fromIdCsvWithId, makeMapFromWithIds, toCsv, (?!))

data LearnsetEntryFinal = LearnsetEntryFinal
  { lefPokemonId :: Int,
    lefMoveId :: Int,
    lefGameId :: Int,
    lefMethod :: LearnMethodWithLevel
  }
  deriving (Eq, Show)

instance Ord LearnsetEntryFinal where
  compare a b =
    compare
      (lefPokemonId a, lefGameId a, lefMethod a, lefMoveId a)
      (lefPokemonId b, lefGameId b, lefMethod b, lefMoveId b)

instance Csv.ToNamedRecord LearnsetEntryFinal where
  toNamedRecord (LearnsetEntryFinal pokemonId moveId gameId method) =
    Csv.namedRecord
      [ "pokemon_id" Csv..= pokemonId,
        "move_id" Csv..= moveId,
        "game_id" Csv..= gameId,
        -- succ . fromEnum is hacky, but there isn't a good way, because this
        -- function must be pure
        "learn_method_id"
          Csv..= ( succ . fromEnum $ case method of
                     WLLevelUp _ -> LM.LevelUp
                     WLEvolution -> LM.Evolution
                     WLTutor -> LM.Tutor
                     WLEgg -> LM.Egg
                     WLTM -> LM.TM
                     WLReminder -> LM.Reminder
                 ),
        "level"
          Csv..= case method of
            WLLevelUp l -> Just l
            _ -> Nothing
      ]
      `union` Csv.toNamedRecord method

instance Csv.DefaultOrdered LearnsetEntryFinal where
  headerOrder = const $ Csv.header ["pokemon_id", "move_id", "game_id", "learn_method_id", "level"]

makeLearnsetEntryFinal :: Map Text Int -> Map Text Int -> Map Game Int -> LearnsetEntry -> LearnsetEntryFinal
makeLearnsetEntryFinal pokemonMap moveMap gameMap le =
  LearnsetEntryFinal
    { lefPokemonId = pokemonMap ?! leUniqueName le,
      lefMoveId = moveMap ?! lmName (leLearnedMove le),
      lefGameId = gameMap ?! leGame le,
      lefMethod = lmMethod (leLearnedMove le)
    }

setupLearnsets :: IO ()
setupLearnsets = do
  rawLearnsets <- fromCsv "csv/learnsets-raw.csv"
  supplLearnsets <- fromCsv "csv/learnsets-suppl.csv"
  svdlc2Learnsets <- fromCsv "static/learnsets-dlc2.csv"
  gameMap <- makeMapFromWithIds <$> fromCsv "csv/games.csv"
  pokemonMap <- M.mapKeys Setup.Pokemon.uniqueName . makeMapFromWithIds <$> fromCsv "csv/pokemon.csv"
  moveMap <- M.mapKeys Setup.Move.name . makeMapFromWithIds <$> fromCsv "csv/moves.csv"
  let finalEntries = map (makeLearnsetEntryFinal pokemonMap moveMap gameMap) (rawLearnsets ++ supplLearnsets ++ svdlc2Learnsets)
  toCsv "csv/learnsets.csv" (sort finalEntries)
