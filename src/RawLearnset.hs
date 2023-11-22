module RawLearnset
  ( LearnMethodWithLevel (..),
    LearnedMove (..),
    LearnsetEntry (..),
    setupRawLearnsets,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (forM_, guard)
import qualified Data.Csv as Csv
import Data.HashMap.Strict (union)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Game (Game (..), fromString)
import qualified LearnMethod as LM
import RawPokemon (Pokemon (..))
import Text.HTML.Scalpel
import Utils (fromCsv, readInt, toCsv)

data LearnMethodWithLevel
  = WLLevelUp Int
  | WLEvolution
  | WLTutor
  | WLEgg
  | WLTM
  | WLReminder
  deriving (Eq, Ord, Show)

instance Csv.ToNamedRecord LearnMethodWithLevel where
  toNamedRecord lm =
    Csv.namedRecord
      [ "learn_method" Csv..= case lm of
          WLLevelUp _ -> LM.toString LM.LevelUp
          WLEvolution -> LM.toString LM.Evolution
          WLTutor -> LM.toString LM.Tutor
          WLEgg -> LM.toString LM.Egg
          WLTM -> LM.toString LM.TM
          WLReminder -> LM.toString LM.Reminder,
        "level" Csv..= case lm of
          WLLevelUp lvl -> Just lvl
          _ -> Nothing
      ]

instance Csv.FromNamedRecord LearnMethodWithLevel where
  parseNamedRecord m = do
    method <- m Csv..: "learn_method"
    level <- m Csv..: "level"
    pure $ case (LM.fromString method, level) of
      (Just LM.LevelUp, Just x) -> WLLevelUp x
      (Just LM.Evolution, Nothing) -> WLEvolution
      (Just LM.Tutor, Nothing) -> WLTutor
      (Just LM.Egg, Nothing) -> WLEgg
      (Just LM.TM, Nothing) -> WLTM
      (Just LM.Reminder, Nothing) -> WLReminder
      _ -> error $ "Unknown learn method: " ++ method

instance Csv.DefaultOrdered LearnMethodWithLevel where
  headerOrder _ = Csv.header ["learn_method", "level"]

data LearnedMove = LearnedMove
  { lmName :: Text,
    lmMethod :: LearnMethodWithLevel
  }
  deriving (Eq, Ord, Show)

instance Csv.ToNamedRecord LearnedMove where
  toNamedRecord lm =
    Csv.namedRecord ["move_name" Csv..= lmName lm]
      `union` Csv.toNamedRecord (lmMethod lm)

instance Csv.FromNamedRecord LearnedMove where
  parseNamedRecord m = do
    name <- m Csv..: "move_name"
    method <- Csv.parseNamedRecord m
    pure $ LearnedMove name method

instance Csv.DefaultOrdered LearnedMove where
  headerOrder _ = "move_name" `V.cons` Csv.headerOrder (undefined :: LearnMethodWithLevel)

data Learnset = Learnset
  { lsUsum :: [LearnedMove],
    lsSwSh :: [LearnedMove],
    lsBdsp :: [LearnedMove],
    lsSv :: [LearnedMove]
  }
  deriving (Eq, Ord, Show)

data LearnsetEntry = LearnsetEntry
  { leUniqueName :: Text,
    leGame :: Game,
    leLearnedMove :: LearnedMove
  }
  deriving (Eq, Ord, Show)

instance Csv.ToNamedRecord LearnsetEntry where
  toNamedRecord le =
    Csv.namedRecord
      [ "unique_name" Csv..= leUniqueName le,
        "game" Csv..= show (leGame le)
      ]
      `union` Csv.toNamedRecord (leLearnedMove le)

instance Csv.FromNamedRecord LearnsetEntry where
  parseNamedRecord m = do
    uniqueName <- m Csv..: "unique_name"
    gameStr <- m Csv..: "game"
    let game = case Game.fromString gameStr of
          Just g -> g
          Nothing -> error $ "Unknown game: " ++ gameStr
    learnedMove <- Csv.parseNamedRecord m
    pure $ LearnsetEntry uniqueName game learnedMove

instance Csv.DefaultOrdered LearnsetEntry where
  headerOrder _ =
    Csv.header ["unique_name", "game"]
      V.++ Csv.headerOrder (undefined :: LearnedMove)

getIndividualEntries :: Text -> Learnset -> [LearnsetEntry]
getIndividualEntries uniqueName ls' =
  let usumMoves = map (LearnsetEntry uniqueName USUM) (lsUsum ls')
      swshMoves = map (LearnsetEntry uniqueName SwSh) (lsSwSh ls')
      bdspMoves = map (LearnsetEntry uniqueName BDSP) (lsBdsp ls')
      svMoves = map (LearnsetEntry uniqueName SV) (lsSv ls')
   in -- remove duplicates in O(n lg n) time
      S.toList . S.fromList $ usumMoves <> swshMoves <> bdspMoves <> svMoves

isRegionalNotInUSUM :: Pokemon -> Bool
isRegionalNotInUSUM pkmn =
  case form pkmn of
    Just f -> any (`T.isInfixOf` f) ["Galarian", "Hisuian", "Paldean", "White-Striped", "Breed"]
    Nothing -> False

isRegionalNotInBDSP :: Pokemon -> Bool
isRegionalNotInBDSP pkmn =
  case form pkmn of
    Just f ->
      any
        (`T.isInfixOf` f)
        [ "Alolan",
          "Galarian",
          "Hisuian",
          "Paldean",
          "Breed",
          "White-Striped"
        ]
    Nothing -> False

isRegionalNotInSwSh :: Pokemon -> Bool
isRegionalNotInSwSh pkmn =
  case form pkmn of
    Just f -> any (`T.isInfixOf` f) ["Hisuian", "Paldean", "White-Striped", "Breed"]
    Nothing -> False

lsUrsalunaBloodmoonSV :: [LearnedMove]
lsUrsalunaBloodmoonSV =
  [ LearnedMove "Headlong Rush" (WLLevelUp 1),
    LearnedMove "Scratch" (WLLevelUp 1),
    LearnedMove "Leer" (WLLevelUp 1),
    LearnedMove "Lick" (WLLevelUp 1),
    LearnedMove "Fury Swipes" (WLLevelUp 8),
    LearnedMove "Payback" (WLLevelUp 13),
    LearnedMove "Harden" (WLLevelUp 17),
    LearnedMove "Slash" (WLLevelUp 22),
    LearnedMove "Play Nice" (WLLevelUp 25),
    LearnedMove "Scary Face" (WLLevelUp 35),
    LearnedMove "Rest" (WLLevelUp 41),
    LearnedMove "Snore" (WLLevelUp 41),
    LearnedMove "Earth Power" (WLLevelUp 48),
    LearnedMove "Moonblast" (WLLevelUp 56),
    LearnedMove "Hammer Arm" (WLLevelUp 64),
    LearnedMove "Blood Moon" (WLLevelUp 70),
    LearnedMove "Moonlight" WLReminder
  ]
    <> map
      (`LearnedMove` WLEgg)
      [ "Belly Drum",
        "Close Combat",
        "Counter",
        "Cross Chop",
        "Crunch",
        "Double-Edge",
        "Fake Tears",
        "Fury Cutter",
        "Metal Claw",
        "Night Slash",
        "Seismic Toss",
        "Yawn"
      ]
    <> map
      (`LearnedMove` WLTM)
      [ "Take Down",
        "Scary Face",
        "Protect",
        "Low Kick",
        "Thief",
        "Trailblaze",
        "Facade",
        "Bulldoze",
        "Snarl",
        "Metal Claw",
        "Swift",
        "Mud Shot",
        "Rock Tomb",
        "Fling",
        "Avalanche",
        "Endure",
        "Sunny Day",
        "Rain Dance",
        "Dig",
        "Brick Break",
        "Shadow Claw",
        "Body Slam",
        "Fire Punch",
        "Thunder Punch",
        "Ice Punch",
        "Sleep Talk",
        "Seed Bomb",
        "Stomping Tantrum",
        "Rest",
        "Rock Slide",
        "Taunt",
        "Swords Dance",
        "Body Press",
        "Gunk Shot",
        "Substitute",
        "Crunch",
        "Hyper Voice",
        "Heavy Slam",
        "Calm Mind",
        "Helping Hand",
        "Earth Power",
        "Earthquake",
        "Stone Edge",
        "Giga Impact",
        "Focus Blast",
        "Hyper Beam",
        "Tera Blast",
        "Roar",
        "Smack Down",
        "Vacuum Wave",
        "High Horsepower",
        "Uproar",
        "Focus Punch"
      ]

lsTaurosCombatSV :: [LearnedMove]
lsTaurosCombatSV =
  [ LearnedMove "Tackle" (WLLevelUp 1),
    LearnedMove "Tail Whip" (WLLevelUp 1),
    LearnedMove "Work Up" (WLLevelUp 5),
    LearnedMove "Double Kick" (WLLevelUp 10),
    LearnedMove "Assurance" (WLLevelUp 15),
    LearnedMove "Headbutt" (WLLevelUp 20),
    LearnedMove "Scary Face" (WLLevelUp 25),
    LearnedMove "Zen Headbutt" (WLLevelUp 30),
    LearnedMove "Raging Bull" (WLLevelUp 35),
    LearnedMove "Rest" (WLLevelUp 40),
    LearnedMove "Swagger" (WLLevelUp 45),
    LearnedMove "Thrash" (WLLevelUp 50),
    LearnedMove "Double-Edge" (WLLevelUp 55),
    LearnedMove "Close Combat" (WLLevelUp 60),
    LearnedMove "Take Down" WLTM,
    LearnedMove "Scary Face" WLTM,
    LearnedMove "Protect" WLTM,
    LearnedMove "Thief" WLTM,
    LearnedMove "Trailblaze" WLTM,
    LearnedMove "Facade" WLTM,
    LearnedMove "Bulldoze" WLTM,
    LearnedMove "Rock Tomb" WLTM,
    LearnedMove "Endure" WLTM,
    LearnedMove "Sunny Day" WLTM,
    LearnedMove "Rain Dance" WLTM,
    LearnedMove "Sandstorm" WLTM,
    LearnedMove "Smart Strike" WLTM,
    LearnedMove "Dig" WLTM,
    LearnedMove "Zen Headbutt" WLTM,
    LearnedMove "Bulk Up" WLTM,
    LearnedMove "Body Slam" WLTM,
    LearnedMove "Sleep Talk" WLTM,
    LearnedMove "Stomping Tantrum" WLTM,
    LearnedMove "Rest" WLTM,
    LearnedMove "Rock Slide" WLTM,
    LearnedMove "Body Press" WLTM,
    LearnedMove "Iron Head" WLTM,
    LearnedMove "Substitute" WLTM,
    LearnedMove "Drill Run" WLTM,
    LearnedMove "Surf" WLTM,
    LearnedMove "Reversal" WLTM,
    LearnedMove "Wild Charge" WLTM,
    LearnedMove "Earthquake" WLTM,
    LearnedMove "Stone Edge" WLTM,
    LearnedMove "Giga Impact" WLTM,
    LearnedMove "Outrage" WLTM,
    LearnedMove "Hyper Beam" WLTM,
    LearnedMove "Close Combat" WLTM,
    LearnedMove "Tera Blast" WLTM,
    LearnedMove "High Horsepower" WLTM,
    LearnedMove "Lash Out" WLTM,
    LearnedMove "Curse" WLEgg,
    LearnedMove "Endeavor" WLEgg
  ]

lsTaurosBlazeSV :: [LearnedMove]
lsTaurosBlazeSV =
  [ LearnedMove "Tackle" (WLLevelUp 1),
    LearnedMove "Tail Whip" (WLLevelUp 1),
    LearnedMove "Work Up" (WLLevelUp 5),
    LearnedMove "Double Kick" (WLLevelUp 10),
    LearnedMove "Flame Charge" (WLLevelUp 15),
    LearnedMove "Headbutt" (WLLevelUp 20),
    LearnedMove "Scary Face" (WLLevelUp 25),
    LearnedMove "Zen Headbutt" (WLLevelUp 30),
    LearnedMove "Raging Bull" (WLLevelUp 35),
    LearnedMove "Rest" (WLLevelUp 40),
    LearnedMove "Swagger" (WLLevelUp 45),
    LearnedMove "Thrash" (WLLevelUp 50),
    LearnedMove "Flare Blitz" (WLLevelUp 55),
    LearnedMove "Close Combat" (WLLevelUp 60),
    LearnedMove "Take Down" WLTM,
    LearnedMove "Scary Face" WLTM,
    LearnedMove "Protect" WLTM,
    LearnedMove "Thief" WLTM,
    LearnedMove "Trailblaze" WLTM,
    LearnedMove "Fire Spin" WLTM,
    LearnedMove "Facade" WLTM,
    LearnedMove "Bulldoze" WLTM,
    LearnedMove "Rock Tomb" WLTM,
    LearnedMove "Flame Charge" WLTM,
    LearnedMove "Endure" WLTM,
    LearnedMove "Sunny Day" WLTM,
    LearnedMove "Rain Dance" WLTM,
    LearnedMove "Sandstorm" WLTM,
    LearnedMove "Smart Strike" WLTM,
    LearnedMove "Dig" WLTM,
    LearnedMove "Zen Headbutt" WLTM,
    LearnedMove "Bulk Up" WLTM,
    LearnedMove "Body Slam" WLTM,
    LearnedMove "Sleep Talk" WLTM,
    LearnedMove "Stomping Tantrum" WLTM,
    LearnedMove "Rest" WLTM,
    LearnedMove "Rock Slide" WLTM,
    LearnedMove "Body Press" WLTM,
    LearnedMove "Iron Head" WLTM,
    LearnedMove "Substitute" WLTM,
    LearnedMove "Drill Run" WLTM,
    LearnedMove "Will-O-Wisp" WLTM,
    LearnedMove "Flamethrower" WLTM,
    LearnedMove "Reversal" WLTM,
    LearnedMove "Fire Blast" WLTM,
    LearnedMove "Wild Charge" WLTM,
    LearnedMove "Earthquake" WLTM,
    LearnedMove "Stone Edge" WLTM,
    LearnedMove "Giga Impact" WLTM,
    LearnedMove "Outrage" WLTM,
    LearnedMove "Overheat" WLTM,
    LearnedMove "Hyper Beam" WLTM,
    LearnedMove "Flare Blitz" WLTM,
    LearnedMove "Close Combat" WLTM,
    LearnedMove "Tera Blast" WLTM,
    LearnedMove "High Horsepower" WLTM,
    LearnedMove "Lash Out" WLTM,
    LearnedMove "Curse" WLEgg,
    LearnedMove "Endeavor" WLEgg
  ]

lsTaurosAquaSV :: [LearnedMove]
lsTaurosAquaSV =
  [ LearnedMove "Tackle" (WLLevelUp 1),
    LearnedMove "Tail Whip" (WLLevelUp 1),
    LearnedMove "Work Up" (WLLevelUp 5),
    LearnedMove "Double Kick" (WLLevelUp 10),
    LearnedMove "Aqua Jet" (WLLevelUp 15),
    LearnedMove "Headbutt" (WLLevelUp 20),
    LearnedMove "Scary Face" (WLLevelUp 25),
    LearnedMove "Zen Headbutt" (WLLevelUp 30),
    LearnedMove "Raging Bull" (WLLevelUp 35),
    LearnedMove "Rest" (WLLevelUp 40),
    LearnedMove "Swagger" (WLLevelUp 45),
    LearnedMove "Thrash" (WLLevelUp 50),
    LearnedMove "Wave Crash" (WLLevelUp 55),
    LearnedMove "Close Combat" (WLLevelUp 60),
    LearnedMove "Take Down" WLTM,
    LearnedMove "Scary Face" WLTM,
    LearnedMove "Protect" WLTM,
    LearnedMove "Water Pulse" WLTM,
    LearnedMove "Thief" WLTM,
    LearnedMove "Trailblaze" WLTM,
    LearnedMove "Chilling Water" WLTM,
    LearnedMove "Facade" WLTM,
    LearnedMove "Bulldoze" WLTM,
    LearnedMove "Rock Tomb" WLTM,
    LearnedMove "Endure" WLTM,
    LearnedMove "Rain Dance" WLTM,
    LearnedMove "Sandstorm" WLTM,
    LearnedMove "Smart Strike" WLTM,
    LearnedMove "Dig" WLTM,
    LearnedMove "Zen Headbutt" WLTM,
    LearnedMove "Bulk Up" WLTM,
    LearnedMove "Body Slam" WLTM,
    LearnedMove "Sleep Talk" WLTM,
    LearnedMove "Stomping Tantrum" WLTM,
    LearnedMove "Rest" WLTM,
    LearnedMove "Rock Slide" WLTM,
    LearnedMove "Body Press" WLTM,
    LearnedMove "Iron Head" WLTM,
    LearnedMove "Substitute" WLTM,
    LearnedMove "Drill Run" WLTM,
    LearnedMove "Liquidation" WLTM,
    LearnedMove "Surf" WLTM,
    LearnedMove "Reversal" WLTM,
    LearnedMove "Hydro Pump" WLTM,
    LearnedMove "Wild Charge" WLTM,
    LearnedMove "Earthquake" WLTM,
    LearnedMove "Stone Edge" WLTM,
    LearnedMove "Giga Impact" WLTM,
    LearnedMove "Outrage" WLTM,
    LearnedMove "Hyper Beam" WLTM,
    LearnedMove "Close Combat" WLTM,
    LearnedMove "Tera Blast" WLTM,
    LearnedMove "High Horsepower" WLTM,
    LearnedMove "Lash Out" WLTM,
    LearnedMove "Curse" WLEgg,
    LearnedMove "Endeavor" WLEgg
  ]

lsWooperPaldeaSV :: [LearnedMove]
lsWooperPaldeaSV =
  [ LearnedMove "Mud Shot" (WLLevelUp 1),
    LearnedMove "Tail Whip" (WLLevelUp 1),
    LearnedMove "Tackle" (WLLevelUp 4),
    LearnedMove "Poison Tail" (WLLevelUp 8),
    LearnedMove "Toxic Spikes" (WLLevelUp 12),
    LearnedMove "Slam" (WLLevelUp 16),
    LearnedMove "Yawn" (WLLevelUp 21),
    LearnedMove "Poison Jab" (WLLevelUp 24),
    LearnedMove "Sludge Wave" (WLLevelUp 28),
    LearnedMove "Amnesia" (WLLevelUp 32),
    LearnedMove "Toxic" (WLLevelUp 36),
    LearnedMove "Earthquake" (WLLevelUp 40),
    LearnedMove "Take Down" WLTM,
    LearnedMove "Mud-Slap" WLTM,
    LearnedMove "Protect" WLTM,
    LearnedMove "Water Pulse" WLTM,
    LearnedMove "Low Kick" WLTM,
    LearnedMove "Acid Spray" WLTM,
    LearnedMove "Trailblaze" WLTM,
    LearnedMove "Chilling Water" WLTM,
    LearnedMove "Facade" WLTM,
    LearnedMove "Poison Tail" WLTM,
    LearnedMove "Bulldoze" WLTM,
    LearnedMove "Mud Shot" WLTM,
    LearnedMove "Rock Tomb" WLTM,
    LearnedMove "Venoshock" WLTM,
    LearnedMove "Endure" WLTM,
    LearnedMove "Rain Dance" WLTM,
    LearnedMove "Sandstorm" WLTM,
    LearnedMove "Dig" WLTM,
    LearnedMove "Body Slam" WLTM,
    LearnedMove "Sleep Talk" WLTM,
    LearnedMove "Waterfall" WLTM,
    LearnedMove "Poison Jab" WLTM,
    LearnedMove "Stomping Tantrum" WLTM,
    LearnedMove "Rest" WLTM,
    LearnedMove "Rock Slide" WLTM,
    LearnedMove "Body Press" WLTM,
    LearnedMove "Spikes" WLTM,
    LearnedMove "Toxic Spikes" WLTM,
    LearnedMove "Gunk Shot" WLTM,
    LearnedMove "Substitute" WLTM,
    LearnedMove "Liquidation" WLTM,
    LearnedMove "Stealth Rock" WLTM,
    LearnedMove "Surf" WLTM,
    LearnedMove "Amnesia" WLTM,
    LearnedMove "Helping Hand" WLTM,
    LearnedMove "Earth Power" WLTM,
    LearnedMove "Hydro Pump" WLTM,
    LearnedMove "Sludge Bomb" WLTM,
    LearnedMove "Earthquake" WLTM,
    LearnedMove "Stone Edge" WLTM,
    LearnedMove "Tera Blast" WLTM,
    LearnedMove "Haze" WLTM,
    LearnedMove "Toxic" WLTM,
    LearnedMove "Acid Spray" WLEgg,
    LearnedMove "After You" WLEgg,
    LearnedMove "Ancient Power" WLEgg,
    LearnedMove "Counter" WLEgg,
    LearnedMove "Curse" WLEgg,
    LearnedMove "Double Kick" WLEgg,
    LearnedMove "Haze" WLEgg,
    LearnedMove "Mist" WLEgg,
    LearnedMove "Recover" WLEgg,
    LearnedMove "Spit Up" WLEgg,
    LearnedMove "Stockpile" WLEgg,
    LearnedMove "Swallow" WLEgg
  ]

fixMoveName :: Text -> Text
fixMoveName "Vice Grip" = "Vise Grip"
fixMoveName t = t

getLearnsetIn :: Pokemon -> Game -> IO [LearnedMove]
getLearnsetIn pkmn game
  | game == BDSP && isRegionalNotInBDSP pkmn = pure []
  | game == SwSh && isRegionalNotInSwSh pkmn = pure []
  | game == USUM && isRegionalNotInUSUM pkmn = pure []
  | otherwise = do
      -- Step 1: Figure out which is the tab id corresponding to the correct game
      let (gen, targetTagName) = case game of
            USUM -> (7, "Ultra Sun/Ultra Moon")
            SwSh -> (8, "Sword/Shield")
            BDSP -> (8, "Brilliant Diamond/Shining Pearl")
            SV -> (9, "Scarlet/Violet")
          url = RawPokemon.url pkmn <> "/moves/" <> T.pack (show gen)
      tags <- fetchTags (T.unpack url)
      -- If we get a 404, then the Pokemon doesn't exist in this game. Scalpel
      -- doesn't expose any HTTP request errors and I can't be bothered to
      -- actually parse the 404 page, so this will have to suffice
      if "Error 404: Page Not Found" `T.isInfixOf` T.pack (show tags)
        then pure []
        else do
          let tabListScraper :: Scraper Text [(Text, Text)]
              tabListScraper = chroot ("div" @: [hasClass "sv-tabs-tab-list"]) $ do
                ts <- texts ("a" @: [hasClass "sv-tabs-tab"])
                ids <- attrs "href" ("a" @: [hasClass "sv-tabs-tab"])
                pure $ zip ts (map (T.replace "#" "") ids)
          let tabTitles = fromJust $ scrape tabListScraper tags
          tabId <- case filter ((== targetTagName) . fst) tabTitles of
            [(_, elemId)] -> pure elemId
            _ -> throwIO $ userError $ T.unpack ("Could not find " <> targetTagName <> " tab for Pokemon named " <> RawPokemon.name pkmn)

          -- Step 2: Define convenience functions which can be reused
          let chrootSerialTabId :: SerialScraper Text a -> Scraper Text a
              chrootSerialTabId = chroot ("div" @: ["id" @= T.unpack tabId] // "div" @: [hasClass "grid-col"]) . inSerial
              findNextH3With :: [Text] -> SerialScraper Text ()
              findNextH3With snippets = seekNext $ do
                h3 <- text "h3"
                forM_ snippets $ \snip -> guard (snip `T.isInfixOf` h3)
              findNextPWith :: [Text] -> SerialScraper Text ()
              findNextPWith snippets = seekNext $ do
                p <- text "p"
                forM_ snippets $ \snip -> guard (snip `T.isInfixOf` p)
              findNextPTextSmallWith :: [Text] -> SerialScraper Text ()
              findNextPTextSmallWith snippets = seekNext $ do
                p <- text ("p" @: [hasClass "text-small"])
                forM_ snippets $ \snip -> guard (snip `T.isInfixOf` p)
              -- Fetch names of the forms and the div IDs of the corresponding tabs
              -- to search in
              getFormsAndTabIds :: SerialScraper Text [(Text, Text)]
              getFormsAndTabIds = do
                untilNext (matches "h3") $ seekNext $ chroot ("div" @: [hasClass "tabset-moves-game-form"] // "div" @: [hasClass "sv-tabs-tab-list"]) $ do
                  ts <- texts ("a" @: [hasClass "sv-tabs-tab"])
                  ids <- attrs "href" ("a" @: [hasClass "sv-tabs-tab"])
                  pure $ zip ts (map (T.replace "#" "") ids)
              -- Get a move table when no Form tabs are present
              withMoveTableNoForms :: Scraper Text a -> SerialScraper Text a
              withMoveTableNoForms scraper = do
                -- stepBack $ pure () -- Not sure if needed?
                seekNext $ chroot ("div" @: [hasClass "resp-scroll"] // "table" @: [hasClass "data-table"]) scraper

              -- Get a move table from a particular Form tab. This requires that the
              -- Form tabs are present and that the specific Form being searched for is
              -- present.
              withMoveTableFormFound :: Text -> Scraper Text a -> SerialScraper Text a
              withMoveTableFormFound tabId scraper = do
                -- Rewind first so that we can seekNext again
                stepBack $ pure ()
                seekNext $ chroot ("div" @: ["id" @= T.unpack tabId] // "table" @: [hasClass "data-table"]) scraper

              -- Get a move table searching for Forms, and run the given scraper on the
              -- table found. This generalises across all types of moves.
              runScraperWithForms :: Scraper Text [LearnedMove] -> SerialScraper Text [LearnedMove]
              runScraperWithForms scraper = do
                formsTabids <- getFormsAndTabIds <|> pure []
                case formsTabids of
                  [] -> withMoveTableNoForms scraper -- No forms, or moves are shared between forms
                  formsIds -> do
                    case filter
                      ( \(fm, _) ->
                          RawPokemon.form pkmn == Just fm
                            || (isNothing (RawPokemon.form pkmn) && RawPokemon.name pkmn == fm)
                      )
                      formsIds of
                      [] -> pure [] -- No moves for the given form
                      [(_, id')] -> withMoveTableFormFound id' scraper -- Form-specific moves

          -- Step 3: actual parsing
          let pkmnFullName = T.unpack $ case RawPokemon.form pkmn of
                Nothing -> RawPokemon.name pkmn
                Just fm -> RawPokemon.name pkmn <> " (" <> fm <> ")"

          -- Level up
          let lvlupScraper :: Scraper Text [LearnedMove]
              lvlupScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by level up"]
                findNextPTextSmallWith ["learns the following moves", "at the levels specified"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    level <- readInt <$> text ("td" @: [hasClass "cell-num"])
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnedMove (fixMoveName name) (WLLevelUp level)
              noLvlupScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by level up"]
                findNextPWith ["does not learn any level up moves"]
                pure []
          lvlup <- case scrape (lvlupScraper <|> noLvlupScraper) tags of
            Just moves -> pure moves
            Nothing -> throwIO $ userError $ "Could not parse level up move section for " <> pkmnFullName

          -- Evolution
          let evolutionScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt on evolution"]
                findNextPTextSmallWith ["learns the following moves when it evolves"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnedMove (fixMoveName name) WLEvolution
              -- Just use fromMaybe [] here because not every Pokemon page has the
              -- evolution section
              evolution = fromMaybe [] $ scrape evolutionScraper tags

          -- Egg moves
          let hasEmsScraper = chrootSerialTabId $ do
                findNextH3With ["Egg moves"]
                findNextPTextSmallWith ["learns the following moves via breeding"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnedMove (fixMoveName name) WLEgg
              noEmsScraper = chrootSerialTabId $ do
                findNextH3With ["Egg moves"]
                findNextPWith ["does not learn any moves by breeding"]
                pure []
          eggMoves <- case (RawPokemon.uniqueName pkmn, game) of
            ("grimer-alola", SV) -> pure $ map (`LearnedMove` WLEgg) ["Assurance", "Clear Smog", "Curse", "Mean Look", "Recycle", "Shadow Sneak", "Spite", "Spit Up", "Stockpile", "Swallow"]
            ("muk-alola", SV) -> pure $ map (`LearnedMove` WLEgg) ["Assurance", "Clear Smog", "Curse", "Mean Look", "Recycle", "Shadow Sneak", "Spite", "Spit Up", "Stockpile", "Swallow"]
            ("luvdisc", SV) -> pure $ map (`LearnedMove` WLEgg) ["Aqua Jet", "Entrainment", "Splash", "Supersonic"]
            ("dipplin", SV) -> pure $ map (`LearnedMove` WLEgg) ["Defense Curl", "Rollout", "Recycle", "Sucker Punch"]
            ("cacnea", SV) -> pure $ map (`LearnedMove` WLEgg) ["Acid", "Belch", "Block", "Counter", "Disable", "Fell Stinger", "Switcheroo", "Teeter Dance"]
            ("cacturne", SV) -> pure $ map (`LearnedMove` WLEgg) ["Acid", "Belch", "Block", "Counter", "Disable", "Fell Stinger", "Switcheroo", "Teeter Dance"]
            ("poltchageist", SV) -> pure []
            ("sinistcha", SV) -> pure []
            ("okidogi", SV) -> pure []
            ("munkidori", SV) -> pure []
            ("fezandipiti", SV) -> pure []
            ("ogerpon-teal", SV) -> pure []
            ("ogerpon-wellspring", SV) -> pure []
            ("ogerpon-hearthflame", SV) -> pure []
            ("ogerpon-cornerstone", SV) -> pure []
            _ -> case scrape (hasEmsScraper <|> noEmsScraper) tags of
              Just moves -> pure moves
              Nothing -> throwIO $ userError $ "Could not parse egg move section for " <> pkmnFullName

          -- Tutor moves
          let tutorScraper = chrootSerialTabId $ do
                findNextH3With ["Move Tutor moves"]
                findNextPTextSmallWith ["can be taught these attacks", "from move tutors"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnedMove (fixMoveName name) WLTutor
              tutorMoves = fromMaybe [] $ scrape tutorScraper tags

          -- Reminder moves
          let reminderScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by reminder"]
                findNextPTextSmallWith ["learns the following extra moves when using the"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnedMove (fixMoveName name) WLReminder
              reminderMoves = fromMaybe [] $ scrape reminderScraper tags

          -- TM moves
          let tmScraper :: Scraper Text [LearnedMove]
              tmScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TM"]
                findNextPTextSmallWith ["is compatible with these Technical Machines"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnedMove (fixMoveName name) WLTM
              noTmScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TM"]
                findNextPWith ["cannot be taught any TM moves"]
                pure []
          tmMoves <- case scrape (tmScraper <|> noTmScraper) tags of
            Just moves -> pure moves
            Nothing -> throwIO $ userError $ "Could not parse TM move section for " <> pkmnFullName

          -- TR moves
          let trScraper :: Scraper Text [LearnedMove]
              trScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TR"]
                findNextPTextSmallWith ["is compatible with these Technical Records"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnedMove (fixMoveName name) WLTM
              noTrScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TR"]
                findNextPWith ["cannot be taught any TR moves"]
                pure []
          trMoves <- case scrape (trScraper <|> noTrScraper) tags of
            Just moves -> pure moves
            Nothing ->
              if game == SwSh
                then throwIO $ userError $ "Could not parse TR move section for " <> pkmnFullName
                else pure []

          -- Put it all together
          pure $ lvlup <> evolution <> eggMoves <> tutorMoves <> tmMoves <> trMoves <> reminderMoves

getLearnset :: Pokemon -> IO (Text, Learnset)
getLearnset pkmn = do
  T.putStrLn $ "Getting learnset for " <> RawPokemon.uniqueName pkmn
  usum <- getLearnsetIn pkmn USUM
  swsh <- getLearnsetIn pkmn SwSh
  bdsp <- getLearnsetIn pkmn BDSP
  sv <- getLearnsetIn pkmn SV
  pure (RawPokemon.uniqueName pkmn, Learnset usum swsh bdsp sv)

takeEMsFrom :: Game -> Text -> Text -> Map Text Learnset -> Map Text Learnset
takeEMsFrom game child parent learnsets =
  let filterEMs :: [LearnedMove] -> [LearnedMove]
      filterEMs = filter (\(LearnedMove _ wl) -> wl == WLEgg)
   in let filterNotEMs :: [LearnedMove] -> [LearnedMove]
          filterNotEMs = filter (\(LearnedMove _ wl) -> wl /= WLEgg)
       in let childEMs = case M.lookup child learnsets of
                Nothing -> []
                Just ls -> filterEMs $ case game of
                  USUM -> lsUsum ls
                  SwSh -> lsSwSh ls
                  BDSP -> lsBdsp ls
                  SV -> lsSv ls
              parentNotEMs = case M.lookup parent learnsets of
                Nothing -> []
                Just x -> filterNotEMs $ case game of
                  USUM -> lsUsum x
                  SwSh -> lsSwSh x
                  BDSP -> lsBdsp x
                  SV -> lsSv x
           in case game of
                USUM -> M.adjust (\ls -> ls {lsUsum = parentNotEMs <> childEMs}) parent learnsets
                SwSh -> M.adjust (\ls -> ls {lsSwSh = parentNotEMs <> childEMs}) parent learnsets
                BDSP -> M.adjust (\ls -> ls {lsBdsp = parentNotEMs <> childEMs}) parent learnsets
                SV -> M.adjust (\ls -> ls {lsSv = parentNotEMs <> childEMs}) parent learnsets

fixOinkologneMoveset :: Map Text Learnset -> Map Text Learnset
fixOinkologneMoveset m =
  let maleOinkologneLs = lsSv $ m M.! "oinkologne-male"
      maleOinkologneAllExceptLvUp =
        filter
          ( \(LearnedMove _ wl) -> case wl of
              WLLevelUp _ -> False
              _ -> True
          )
          maleOinkologneLs
      femaleOinkologneLvUp =
        [ LearnedMove "Tackle" (WLLevelUp 1),
          LearnedMove "Tail Whip" (WLLevelUp 1),
          LearnedMove "Disarming Voice" (WLLevelUp 3),
          LearnedMove "Echoed Voice" (WLLevelUp 6),
          LearnedMove "Mud Shot" (WLLevelUp 9),
          LearnedMove "Covet" (WLLevelUp 12),
          LearnedMove "Dig" (WLLevelUp 15),
          LearnedMove "Headbutt" (WLLevelUp 17),
          LearnedMove "Yawn" (WLLevelUp 23),
          LearnedMove "Take Down" (WLLevelUp 28),
          LearnedMove "Work Up" (WLLevelUp 30),
          LearnedMove "Uproar" (WLLevelUp 34),
          LearnedMove "Double-Edge" (WLLevelUp 39),
          LearnedMove "Earth Power" (WLLevelUp 45),
          LearnedMove "Belch" (WLLevelUp 51)
        ]
   in M.adjust (\ls -> ls {lsSv = femaleOinkologneLvUp <> maleOinkologneAllExceptLvUp}) "oinkologne-female" m

patchLearnsets :: Map Text Learnset -> Map Text Learnset
patchLearnsets =
  M.adjust (\ls -> ls {lsSv = lsUrsalunaBloodmoonSV}) "ursaluna-bloodmoon"
    . M.adjust (\ls -> ls {lsSv = lsTaurosCombatSV}) "tauros-combat"
    . M.adjust (\ls -> ls {lsSv = lsTaurosBlazeSV}) "tauros-blaze"
    . M.adjust (\ls -> ls {lsSv = lsTaurosAquaSV}) "tauros-aqua"
    . M.adjust (\ls -> ls {lsSv = lsWooperPaldeaSV}) "wooper-paldea"
    . fixOinkologneMoveset
    . takeEMsFrom SwSh "espurr" "meowstic-male"
    . takeEMsFrom SwSh "smoochum" "jynx"
    . takeEMsFrom SwSh "elekid" "electabuzz"
    . takeEMsFrom SwSh "elekid" "electivire"
    . takeEMsFrom SwSh "magby" "magmar"
    . takeEMsFrom SwSh "magby" "magmortar"
    . takeEMsFrom SwSh "omanyte" "omastar"
    . takeEMsFrom SwSh "kabuto" "kabutops"
    . takeEMsFrom SwSh "dratini" "dragonair"
    . takeEMsFrom SwSh "dratini" "dragonite"
    . takeEMsFrom SwSh "zubat" "golbat"
    . takeEMsFrom SwSh "zubat" "crobat"
    . takeEMsFrom SwSh "treecko" "grovyle"
    . takeEMsFrom SwSh "treecko" "sceptile"
    . takeEMsFrom SwSh "torchic" "combusken"
    . takeEMsFrom SwSh "torchic" "blaziken"
    . takeEMsFrom SwSh "mudkip" "marshtomp"
    . takeEMsFrom SwSh "mudkip" "swampert"
    . takeEMsFrom SwSh "aron" "lairon"
    . takeEMsFrom SwSh "aron" "aggron"
    . takeEMsFrom SwSh "swablu" "altaria"
    . takeEMsFrom SwSh "nidoran-female" "nidorina"
    . takeEMsFrom SwSh "nidoran-female" "nidoqueen"
    . takeEMsFrom SwSh "nidoran-male" "nidorino"
    . takeEMsFrom SwSh "nidoran-male" "nidoking"
    . takeEMsFrom SwSh "roselia" "roserade"
    . takeEMsFrom SwSh "lileep" "cradily"
    . takeEMsFrom SwSh "anorith" "armaldo"
    . takeEMsFrom SwSh "spheal" "sealeo"
    . takeEMsFrom SwSh "spheal" "walrein"
    . takeEMsFrom SwSh "bagon" "shelgon"
    . takeEMsFrom SwSh "bagon" "salamence"
    . takeEMsFrom SwSh "gible" "gabite"
    . takeEMsFrom SwSh "gible" "garchomp"
    . takeEMsFrom SwSh "espurr" "meowstic-male"
    . takeEMsFrom SwSh "tirtouga" "carracosta"
    . takeEMsFrom SwSh "archen" "archeops"
    . takeEMsFrom SwSh "tyrunt" "tyrantrum"
    . takeEMsFrom SwSh "amaura" "aurorus"
    . takeEMsFrom SwSh "rockruff" "lycanroc-dusk"
    . takeEMsFrom SV "spinarak" "ariados"
    . takeEMsFrom SV "yanma" "yanmega"
    . takeEMsFrom SV "poochyena" "mightyena"
    . takeEMsFrom SV "corphish" "crawdaunt"
    . takeEMsFrom SV "sewaddle" "swadloon"
    . takeEMsFrom SV "sewaddle" "leavanny"
    . takeEMsFrom SV "cutiefly" "ribombee"
    . takeEMsFrom SV "ekans" "arbok"
    . takeEMsFrom SV "bellsprout" "weepinbell"
    . takeEMsFrom SV "bellsprout" "victreebel"
    . takeEMsFrom SV "sentret" "furret"
    . takeEMsFrom SV "poliwag" "poliwhirl"
    . takeEMsFrom SV "poliwag" "poliwrath"
    . takeEMsFrom SV "poliwag" "politoed"
    . takeEMsFrom SV "hoothoot" "noctowl"
    . takeEMsFrom SV "aipom" "ambipom"
    . takeEMsFrom SV "swinub" "piloswine"
    . takeEMsFrom SV "swinub" "mamoswine"
    . takeEMsFrom SV "seedot" "nuzleaf"
    . takeEMsFrom SV "seedot" "shiftry"
    . takeEMsFrom SV "phantump" "trevenant"
    . takeEMsFrom SV "timburr" "gurdurr"
    . takeEMsFrom SV "timburr" "conkeldurr"
    . takeEMsFrom SV "munchlax" "snorlax"
    . takeEMsFrom SV "lotad" "lombre"
    . takeEMsFrom SV "lotad" "ludicolo"
    . takeEMsFrom SV "nosepass" "probopass"
    . takeEMsFrom SV "grubbin" "charjabug"
    . takeEMsFrom SV "grubbin" "vikavolt"
    . takeEMsFrom SV "gligar" "gliscor"
    . takeEMsFrom SV "vullaby" "mandibuzz"
    . takeEMsFrom SV "jangmo-o" "hakamo-o"
    . takeEMsFrom SV "jangmo-o" "kommo-o"
    . takeEMsFrom SV "mienfoo" "mienshao"
    . takeEMsFrom SV "duskull" "dusclops"
    . takeEMsFrom SV "duskull" "dusknoir"
    . takeEMsFrom SV "chingling" "chimecho"
    . takeEMsFrom SV "slugma" "magcargo"
    . takeEMsFrom SV "litwick" "lampent"
    . takeEMsFrom SV "litwick" "chandelure"
    . takeEMsFrom SV "cleffa" "clefairy"
    . takeEMsFrom SV "cleffa" "clefable"
    . takeEMsFrom SV "feebas" "milotic"
    . takeEMsFrom SV "ducklett" "swanna"
    . takeEMsFrom SV "turtwig" "grotle"
    . takeEMsFrom SV "turtwig" "torterra"
    . takeEMsFrom SV "chimchar" "monferno"
    . takeEMsFrom SV "chimchar" "infernape"
    . takeEMsFrom SV "piplup" "prinplup"
    . takeEMsFrom SV "piplup" "empoleon"
    . takeEMsFrom SV "rockruff" "lycanroc-dusk"

setupRawLearnsets :: IO ()
setupRawLearnsets = do
  -- read in all PokemonPartial
  allPkmn <- fromCsv "csv/pokemon-raw.csv"
  -- scrape all learnsets
  allLearnsets <- M.fromList <$> mapM getLearnset allPkmn
  -- patch
  let patchedLearnsets = patchLearnsets allLearnsets
  -- write to csv
  let individualEntries = concatMap (uncurry getIndividualEntries) (M.toList patchedLearnsets)
  toCsv "csv/learnsets-raw.csv" individualEntries
