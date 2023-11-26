module Setup.SupplementaryLearnset (setupSupplementaryLearnsets) where

import Control.Monad (guard, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Setup.Game (Game (..))
import Setup.Pokemon (PokemonFinal (..))
import Setup.RawLearnset
  ( LearnMethodWithLevel (..),
    LearnedMove (..),
    LearnsetEntry (..),
  )
import Text.HTML.Scalpel
import Utils (fromIdCsvWithoutId, toCsv)

tmMoveNames :: [Text]
tmMoveNames =
  [ "Roar",
    "Charge",
    "Haze",
    "Toxic",
    "Sand Tomb",
    "Spite",
    "Gravity",
    "Smack Down",
    "Gyro Ball",
    "Knock Off",
    "Bug Bite",
    "Super Fang",
    "Vacuum Wave",
    "Lunge",
    "High Horsepower",
    "Icicle Spear",
    "Scald",
    "Heat Crash",
    "Solar Blade",
    "Uproar",
    "Focus Punch",
    "Weather Ball",
    "Grassy Glide",
    "Burning Jealousy",
    "Flip Turn",
    "Dual Wingbeat",
    "Poltergeist",
    "Lash Out",
    "Scale Shot",
    "Misty Explosion"
  ]

patchMonList :: Text -> [Text] -> [Text] -> [Text]
patchMonList serebiiForm otherForms list =
  list <> if serebiiForm `elem` list then otherForms else []

-- | PokemonDB doesn't have info on the new TMs introduced in the SV DLC. This,
-- unfortunately, means that we have to scrape it from Serebii instead.
getSupplementaryLearnsetFor :: [Text] -> Text -> IO [LearnsetEntry]
getSupplementaryLearnsetFor allUniqueNames moveName = do
  let url = "https://www.serebii.net/attackdex-sv/" <> T.replace " " "" (T.toLower moveName) <> ".shtml"
  T.putStrLn $ "Scraping " <> url <> "..."

  let findTM el = do
        tx <- text el
        guard $ "By Technical Machine" `T.isInfixOf` tx
      getMons allUniqueNames moveName = chroots "tr" $ do
        -- Get the Pokemon name from the third td
        tds <- texts "td"
        guard $ length tds >= 3
        let pkmnName = T.toLower $ T.replace " " "-" $ tds !! 2
        -- Get the form from the image. We have to parse this to match our own
        -- form names. If it fails parsing, we throw an error.
        img <- attr "src" ("td" // "a" // "img")
        let imgBaseName = last $ T.splitOn "/" img
        let pkmnIdentifier = T.dropWhile (`elem` ("1234567890" :: String)) $ T.replace ".png" "" imgBaseName
        let pkmnForm = case (pkmnName, pkmnIdentifier) of
              ("lycanroc", "") -> "-midday"
              ("lycanroc", "-m") -> "-midnight"
              ("lycanroc", "-d") -> "-dusk"
              ("ursaluna", "-b") -> "-bloodmoon"
              ("rotom", "f") -> "-frost"
              ("rotom", "h") -> "-heat"
              ("rotom", "w") -> "-wash"
              ("rotom", "s") -> "-fan"
              ("rotom", "m") -> "-mow"
              ("tauros", "-p") -> "-combat"
              ("tauros", "-b") -> "-blaze"
              ("tauros", "-a") -> "-aqua"
              ("urshifu", "") -> "-single"
              ("urshifu", "-r") -> "-rapid"
              (_, "") -> ""
              (_, "-a") -> "-alola"
              (_, "-g") -> "-galar"
              (_, "-h") -> "-hisui"
              (_, "-p") -> "-paldea"
              _ -> error $ "Unknown form: " <> T.unpack pkmnIdentifier <> " for " <> T.unpack pkmnName <> " when parsing move " <> T.unpack moveName
        let uniqueName = pkmnName <> pkmnForm
        if uniqueName `elem` allUniqueNames
          then pure [uniqueName]
          else case filter (uniqueName `T.isPrefixOf`) allUniqueNames of
                    [] -> error $ "Unknown Pokemon: " <> T.unpack uniqueName <> " when parsing move " <> T.unpack moveName
                    xs -> do
                      liftIO $ putStrLn $ "   Warning: " <> T.unpack uniqueName <> " not found, but adding " <> show xs <> " to the list"
                      pure xs

  let serebiiTmScraper :: [Text] -> Text -> ScraperT Text IO [Text]
      serebiiTmScraper allUniqueNames moveName = chroot "main" $ inSerial $ do
        _ <- seekNext $ findTM "h2"
        mons <- seekNext $ chroot ("table" @: [hasClass "dextable"]) (getMons allUniqueNames moveName)
        pure $ concat mons

  tags <- fetchTags (T.unpack url)
  mons <- scrapeT (serebiiTmScraper allUniqueNames moveName) tags
  case mons of
    Nothing -> error $ "Failed to scrape " <> T.unpack url
    Just ms -> pure $ map (\nm -> LearnsetEntry nm SV (LearnedMove moveName WLTM)) ms

setupSupplementaryLearnsets :: IO ()
setupSupplementaryLearnsets = do
  allUniqueNames <- map uniqueName <$> fromIdCsvWithoutId "csv/pokemon.csv"
  supplLearnsets <- concat <$> mapM (getSupplementaryLearnsetFor allUniqueNames) tmMoveNames
  print $ length supplLearnsets
  toCsv "csv/learnsets-suppl.csv" supplLearnsets
