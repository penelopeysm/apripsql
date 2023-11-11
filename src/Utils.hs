module Utils
  ( toCsv,
    fromCsv,
    safeToEnum,
  )
where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V

toCsv :: (Csv.ToNamedRecord a, Csv.DefaultOrdered a) => FilePath -> [a] -> IO ()
toCsv fname = BL.writeFile fname . Csv.encodeDefaultOrderedByName

fromCsv :: (Csv.FromNamedRecord a) => FilePath -> IO [a]
fromCsv fname = do
  csvData <- BL.readFile fname
  case Csv.decodeByName csvData of
    Left err -> error err
    Right (_, v) -> pure $ V.toList v

safeToEnum :: forall a. (Enum a, Bounded a) => Int -> Maybe a
safeToEnum i
  | i < 0 = Nothing
  | i > fromEnum (maxBound :: a) = Nothing
  | otherwise = Just $ toEnum i
