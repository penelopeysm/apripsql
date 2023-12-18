module Main where

import Utils (withConnString)
import Control.Exception (bracket)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple
import Options.Applicative
import Setup.Ability (setupAbilities)
import Setup.Alias (setupAliases)
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
import Setup.Pokemon (setupPokemon)
import Setup.RawEvolution (setupRawEvolutions)
import Setup.RawLearnset (setupRawLearnsets)
import Setup.RawMove (setupRawMoves)
import Setup.RawPokemon (setupRawPokemon)
import Setup.SupplementaryLearnset (setupSupplementaryLearnsets)
import Setup.Type (setupTypes)
import System.Environment (lookupEnv)

-- | Essentially withConnection, but also generates the Connection needed to run
-- the action.
wc :: (Connection -> IO a) -> IO a
wc actn = do
  connString <- lookupEnv "FLY_PG_PROXY_CONN_STRING"
  case connString of
    Nothing -> error "FLY_PG_PROXY_CONN_STRING not set"
    Just cs -> withConnString (T.pack cs) actn

-- | To add new commands, just extend this Map. None of the rest of the code
-- needs to be touched.
commands :: Map String (IO ())
commands =
  M.fromList
    [ ("abilities", setupAbilities),
      ("aliases", setupAliases),
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
      ("database", wc setupDatabase)
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
