module Type (Type (..), setupTypes) where

import qualified Data.Csv as Csv
import Utils (toIdCsv)

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

toString :: Type -> String
toString = show

fromString :: String -> Maybe Type
fromString "Normal" = Just Normal
fromString "Fire" = Just Fire
fromString "Water" = Just Water
fromString "Electric" = Just Electric
fromString "Grass" = Just Grass
fromString "Ice" = Just Ice
fromString "Fighting" = Just Fighting
fromString "Poison" = Just Poison
fromString "Ground" = Just Ground
fromString "Flying" = Just Flying
fromString "Psychic" = Just Psychic
fromString "Bug" = Just Bug
fromString "Rock" = Just Rock
fromString "Ghost" = Just Ghost
fromString "Dragon" = Just Dragon
fromString "Dark" = Just Dark
fromString "Steel" = Just Steel
fromString "Fairy" = Just Fairy
fromString _ = Nothing

instance Csv.ToNamedRecord Type where
  toNamedRecord x = Csv.namedRecord ["name" Csv..= toString x]

instance Csv.FromNamedRecord Type where
  parseNamedRecord m = do
    nameString <- m Csv..: "name"
    case fromString nameString of
      Just x -> return x
      Nothing -> fail $ "Invalid type: " ++ nameString

instance Csv.DefaultOrdered Type where
  headerOrder = const $ Csv.header ["name"]

setupTypes :: IO ()
setupTypes = toIdCsv "csv/types.csv" ([minBound ..] :: [Type])
