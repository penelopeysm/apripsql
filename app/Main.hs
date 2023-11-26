module Main where

import Setup.Ability (setupAbilities)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Setup.Database (setupDatabase)
import Setup.EggGroup (setupEggGroups)
import Setup.Evolution (setupEvolutions)
import Setup.Game (setupGames)
import Setup.GenderRatio (setupGenderRatios)
import Setup.LearnMethod (setupLearnMethods)
import Setup.Learnset (setupLearnsets)
import Setup.Legality (setupLegalities)
import Setup.Move (setupMoves)
import Setup.MoveCategory (setupMoveCategories)
import Setup.Nature (setupNatures)
import Options.Applicative
import Setup.Pokemon (setupPokemon)
import Setup.RawEvolution (setupRawEvolutions)
import Setup.RawLearnset (setupRawLearnsets)
import Setup.RawMove (setupRawMoves)
import Setup.RawPokemon (setupRawPokemon)
import Setup.SupplementaryLearnset (setupSupplementaryLearnsets)
import Setup.Type (setupTypes)

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
      ("evolutions", setupEvolutions),
      ("database", setupDatabase)
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
