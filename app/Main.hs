module Main where

import Ability (setupAbilities)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import EggGroup (setupEggGroups)
import Evolution (setupEvolutions)
import Game (setupGames)
import GenderRatio (setupGenderRatios)
import LearnMethod (setupLearnMethods)
import Learnset (setupLearnsets)
import Legality (setupLegalities)
import Move (setupMoves)
import MoveCategory (setupMoveCategories)
import Nature (setupNatures)
import Options.Applicative
import Pokemon (setupPokemon)
import RawEvolution (setupRawEvolutions)
import RawLearnset (setupRawLearnsets)
import RawMove (setupRawMoves)
import RawPokemon (setupRawPokemon)
import SupplementaryLearnset (setupSupplementaryLearnsets)
import Type (setupTypes)

-- | To add new commands, just extend this Map. None of the rest of the code
-- needs to be touched.
commands :: Map String (IO ())
commands =
  M.fromList
    [ ("abilities", setupAbilities),
      ("egg-groups", setupEggGroups),
      ("games", setupGames),
      ("gender-ratios", setupGenderRatios),
      ("learn-methods", setupLearnMethods),
      ("learnsets-raw", setupRawLearnsets),
      ("learnsets-suppl", setupSupplementaryLearnsets),
      ("learnsets", setupLearnsets),
      ("legalities", setupLegalities),
      ("move-categories", setupMoveCategories),
      ("moves-raw", setupRawMoves),
      ("moves", setupMoves),
      ("natures", setupNatures),
      ("pokemon", setupPokemon),
      ("pokemon-raw", setupRawPokemon),
      ("types", setupTypes),
      ("evolutions-raw", setupRawEvolutions),
      ("evolutions", setupEvolutions)
    ]

commandKeys :: String
commandKeys = intercalate ", " (M.keys commands)

commandReader :: ReadM (IO ())
commandReader = maybeReader (`M.lookup` commands)

newtype Options = Options {runCommand :: IO ()}

options :: Parser Options
options = Options <$> option commandReader (long "command" <> short 'c' <> help ("Command to run. Available commands: " <> commandKeys))

main :: IO ()
main = do
  opts <- execParser $ info (options <**> helper) fullDesc
  runCommand opts
