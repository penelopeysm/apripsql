module Game (toString, fromString, Game (..), setupGames) where

import qualified Data.Csv as Csv
import Utils (toIdCsv)

data Game = USUM | SwSh | BDSP | SV
  deriving (Eq, Ord, Show, Bounded, Enum)

toString :: Game -> String
toString = show

fromString :: String -> Maybe Game
fromString "USUM" = Just USUM
fromString "SwSh" = Just SwSh
fromString "BDSP" = Just BDSP
fromString "SV" = Just SV
fromString _ = Nothing

instance Csv.ToNamedRecord Game where
  toNamedRecord x = Csv.namedRecord ["name" Csv..= toString x]

instance Csv.FromNamedRecord Game where
  parseNamedRecord m = do
    nameString <- m Csv..: "name"
    case fromString nameString of
      Just x -> return x
      Nothing -> fail $ "Invalid game name: " ++ nameString

instance Csv.DefaultOrdered Game where
  headerOrder = const $ Csv.header ["name"]

setupGames :: IO ()
setupGames = toIdCsv "csv/games.csv" ([minBound ..] :: [Game])
