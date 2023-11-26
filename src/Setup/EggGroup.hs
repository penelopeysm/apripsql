module Setup.EggGroup (fromString, toString, EggGroup (..), setupEggGroups) where

import qualified Data.Csv as Csv
import Utils (bsToString, toIdCsv)

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

toString :: EggGroup -> String
toString Monster = "Monster"
toString Water1 = "Water 1"
toString Bug = "Bug"
toString Flying = "Flying"
toString Field = "Field"
toString Fairy = "Fairy"
toString Grass = "Grass"
toString HumanLike = "Human-Like"
toString Water3 = "Water 3"
toString Mineral = "Mineral"
toString Amorphous = "Amorphous"
toString Water2 = "Water 2"
toString Ditto = "Ditto"
toString Dragon = "Dragon"
toString Undiscovered = "Undiscovered"

fromString :: String -> Maybe EggGroup
fromString "Monster" = Just Monster
fromString "Water 1" = Just Water1
fromString "Bug" = Just Bug
fromString "Flying" = Just Flying
fromString "Field" = Just Field
fromString "Fairy" = Just Fairy
fromString "Grass" = Just Grass
fromString "Human-Like" = Just HumanLike
fromString "Water 3" = Just Water3
fromString "Mineral" = Just Mineral
fromString "Amorphous" = Just Amorphous
fromString "Water 2" = Just Water2
fromString "Ditto" = Just Ditto
fromString "Dragon" = Just Dragon
fromString "Undiscovered" = Just Undiscovered
fromString _ = Nothing

instance Csv.ToField EggGroup where
  toField = Csv.toField . toString

instance Csv.FromField EggGroup where
  parseField bs = do
    let s = bsToString bs
    case fromString s of
      Just x -> return x
      Nothing -> fail $ "Invalid egg group: " ++ s

instance Csv.ToNamedRecord EggGroup where
  toNamedRecord x = Csv.namedRecord ["name" Csv..= toString x]

instance Csv.FromNamedRecord EggGroup where
  parseNamedRecord m = do
    nameString <- m Csv..: "name"
    case fromString nameString of
      Just x -> return x
      Nothing -> fail $ "Invalid egg group name: " ++ nameString

instance Csv.DefaultOrdered EggGroup where
  headerOrder = const $ Csv.header ["name"]

setupEggGroups :: IO ()
setupEggGroups = toIdCsv "csv/egg-groups.csv" ([minBound ..] :: [EggGroup])
