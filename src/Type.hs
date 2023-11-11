module Type (setupTypes) where

import qualified Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Utils (safeToEnum, toCsv)

data Type
  = Normal
  | Fire
  | Water
  | Electric
  | Grass
  | Ice
  | Fighting
  | Poison
  | Ground
  | Flying
  | Psychic
  | Bug
  | Rock
  | Ghost
  | Dragon
  | Dark
  | Steel
  | Fairy
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Csv.ToNamedRecord Type where
  toNamedRecord eg =
    Csv.namedRecord
      [ ("id", Csv.toField $ succ $ fromEnum eg),
        ("name", Csv.toField $ show eg)
      ]

instance Csv.FromNamedRecord Type where
  parseNamedRecord m = do
    typeId <- m Csv..: "id"
    case safeToEnum (typeId - 1) of
      Nothing -> fail $ "Invalid type id: " <> show typeId
      Just eg -> pure eg

instance Csv.DefaultOrdered Type where
  headerOrder _ = Csv.header ["id", "name"]

-- | Generates csv/egg_groups.csv from internal data.
setupTypes :: IO ()
setupTypes = do
  T.putStrLn "Setting up types..."
  toCsv "csv/types.csv" ([minBound ..] :: [Type])
  T.putStrLn "Done."
