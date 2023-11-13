module RawLearnset (setupRawLearnsets) where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (forM_, guard)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Game (Game (..))
import RawPokemon (Pokemon (..), PokemonPartial (..))
import Text.HTML.Scalpel
import Utils (readInt)

data LearnMethodWithLevel
  = LevelUp Int
  | Evolution
  | Tutor
  | Egg
  | TM
  | Reminder
  deriving (Eq, Ord, Show)

-- TODO
data LearnsetEntry = LearnsetEntry
  { learnsetEntryName :: Text,
    learnsetEntryMethod :: LearnMethodWithLevel
  }
  deriving (Eq, Ord, Show)

isRegionalNotInUSUM :: PokemonPartial -> Bool
isRegionalNotInUSUM pkmn =
  case pform pkmn of
    Just form -> any (`T.isInfixOf` form) ["Galarian", "Hisuian", "Paldean", "White-Striped"]
    Nothing -> False

isRegionalNotInBDSP :: PokemonPartial -> Bool
isRegionalNotInBDSP pkmn =
  case pform pkmn of
    Just form ->
      any
        (`T.isInfixOf` form)
        [ "Alolan",
          "Galarian",
          "Hisuian",
          "Paldean",
          "White-Striped"
        ]
    Nothing -> False

isRegionalNotInSwSh :: PokemonPartial -> Bool
isRegionalNotInSwSh pkmn =
  case pform pkmn of
    Just form -> any (`T.isInfixOf` form) ["Hisuian", "Paldean", "White-Striped"]
    Nothing -> False

getLearnset :: PokemonPartial -> Game -> IO [LearnsetEntry]
getLearnset pkmn game =
  if (game == BDSP && isRegionalNotInBDSP pkmn) || (game == SwSh && isRegionalNotInSwSh pkmn) || (game == USUM && isRegionalNotInUSUM pkmn)
    then pure []
    else do
      -- Step 1: Figure out which is the tab id corresponding to the correct game
      let (gen, targetTagName) = case game of
            USUM -> (7, "Ultra Sun/Ultra Moon")
            SwSh -> (8, "Sword/Shield")
            BDSP -> (8, "Brilliant Diamond/Shining Pearl")
            SV -> (9, "Scarlet/Violet")
          url = pmonUrl pkmn <> "/moves/" <> T.pack (show gen)
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
            _ -> throwIO $ userError $ T.unpack ("Could not find " <> targetTagName <> " tab for Pokemon named " <> pname pkmn)

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
              runScraperWithForms :: Scraper Text [LearnsetEntry] -> SerialScraper Text [LearnsetEntry]
              runScraperWithForms scraper = do
                formsTabids <- getFormsAndTabIds <|> pure []
                case formsTabids of
                  [] -> withMoveTableNoForms scraper -- No forms, or moves are shared between forms
                  formsIds -> do
                    case filter
                      ( \(fm, _) ->
                          pform pkmn == Just fm
                            || (isNothing (pform pkmn) && pname pkmn == fm)
                      )
                      formsIds of
                      [] -> pure [] -- No moves for the given form
                      [(_, id')] -> withMoveTableFormFound id' scraper -- Form-specific moves

          -- Step 3: actual parsing
          let pkmnFullName = T.unpack $ case pform pkmn of
                Nothing -> pname pkmn
                Just fm -> pname pkmn <> " (" <> fm <> ")"

          -- Level up
          let lvlupScraper :: Scraper Text [LearnsetEntry]
              lvlupScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by level up"]
                findNextPTextSmallWith ["learns the following moves", "at the levels specified"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    level <- readInt <$> text ("td" @: [hasClass "cell-num"])
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnsetEntry name (LevelUp level)
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
                    pure $ LearnsetEntry name Evolution
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
                    pure $ LearnsetEntry name Egg
              noEmsScraper = chrootSerialTabId $ do
                findNextH3With ["Egg moves"]
                findNextPWith ["does not learn any moves by breeding"]
                pure []
          eggMoves <- case (pname pkmn, game) of
            ("Dipplin", SV) -> pure $ map (`LearnsetEntry` Egg) ["Defense Curl", "Rollout", "Recycle", "Sucker Punch"]
            ("Poltchageist", SV) -> pure []
            ("Sinistcha", SV) -> pure []
            ("Okidogi", SV) -> pure []
            ("Munkidori", SV) -> pure []
            ("Fezandipiti", SV) -> pure []
            ("Ogerpon", SV) -> pure []
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
                    pure $ LearnsetEntry name Tutor
              tutorMoves = fromMaybe [] $ scrape tutorScraper tags

          -- Reminder moves
          let reminderScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by reminder"]
                findNextPTextSmallWith ["learns the following extra moves when using the"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnsetEntry name Reminder
              reminderMoves = fromMaybe [] $ scrape reminderScraper tags

          -- TM moves
          let tmScraper :: Scraper Text [LearnsetEntry]
              tmScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TM"]
                findNextPTextSmallWith ["is compatible with these Technical Machines"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnsetEntry name TM
              noTmScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TM"]
                findNextPWith ["cannot be taught any TM moves"]
                pure []
          tmMoves <- case scrape (tmScraper <|> noTmScraper) tags of
            Just moves -> pure moves
            Nothing -> throwIO $ userError $ "Could not parse TM move section for " <> pkmnFullName

          -- TR moves
          let trScraper :: Scraper Text [LearnsetEntry]
              trScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TR"]
                findNextPTextSmallWith ["is compatible with these Technical Records"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ LearnsetEntry name TM
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

setupRawLearnsets :: IO ()
setupRawLearnsets = putStrLn "Not implemented"
