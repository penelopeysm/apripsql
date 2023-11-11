module Main where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import EggGroup (setupEggGroups)
import Options.Applicative
import Type (setupTypes)

-- | To add new commands, just extend this Map. None of the rest of the code
-- needs to be touched.
commands :: Map String (IO ())
commands =
  M.fromList
    [ ("egg-groups", setupEggGroups),
      ("types", setupTypes)
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
