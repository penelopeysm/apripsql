module EggGroup (setupEggGroups) where

import qualified Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Utils (safeToEnum, toCsv)

data EggGroup
  = Monster
  | Water1
  | Bug
  | Flying
  | Field
  | Fairy
  | Grass
  | HumanLike
  | Water3
  | Mineral
  | Amorphous
  | Water2
  | Ditto
  | Dragon
  | Undiscovered
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Convert egg group to the actual name.
eggGroupToText :: EggGroup -> Text
eggGroupToText Water1 = "Water 1"
eggGroupToText Water2 = "Water 2"
eggGroupToText Water3 = "Water 3"
eggGroupToText HumanLike = "Human-Like"
eggGroupToText x = T.pack $ show x

instance Csv.ToNamedRecord EggGroup where
  toNamedRecord eg =
    Csv.namedRecord
      [ ("id", Csv.toField $ succ $ fromEnum eg),
        ("name", Csv.toField $ eggGroupToText eg)
      ]

instance Csv.FromNamedRecord EggGroup where
  parseNamedRecord m = do
    eggGroupId <- m Csv..: "id"
    case safeToEnum (eggGroupId - 1) of
      Nothing -> fail $ "Invalid egg group id: " <> show eggGroupId
      Just eg -> pure eg

instance Csv.DefaultOrdered EggGroup where
  headerOrder _ = Csv.header ["id", "name"]

-- | Generates csv/egg_groups.csv from internal data.
setupEggGroups :: IO ()
setupEggGroups = do
  T.putStrLn "Setting up egg groups..."
  toCsv "csv/egg_groups.csv" ([minBound ..] :: [EggGroup])
  T.putStrLn "Done."
