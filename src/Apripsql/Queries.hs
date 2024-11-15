{-# LANGUAGE QuasiQuotes #-}

module Apripsql.Queries
  ( GetPokemonResult (..),
    getPokemon,
    DBPokemon (..),
    getPokemonWithSameNdex,

    -- * Evolution families and crossbreeding info
    getBaseForm,
    getAllParents,
    getAllEvolutionTreeMembers,
    getAllCrossbreedableForms,
    isPokemonUnbreedable,

    -- * Abilities
    randomAbility,
    getAbility,

    -- * Egg moves
    Parent (..),
    EggMove (..),
    EggMoveParents (..),
    randomMoves,
    getEMs,
    getEMParents,
  )
where

import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics (Generic)
import Setup.Game (Game (..))
import Setup.Pokemon (PokemonFinal (..))

-- | For more descriptive type signatures.
type PkmnId = Int

data DBPokemon = DBPokemon
  { dbId :: PkmnId,
    dbName :: Text,
    dbForm :: Maybe Text,
    dbUniqueName :: Text,
    dbNdex :: Int,
    dbGalarDex :: Maybe Int, -- Galar
    dbIoaDex :: Maybe Int, -- Isle of Armor
    dbCtDex :: Maybe Int, -- Crown Tundra
    dbPaldeaDex :: Maybe Int, -- Paldea
    dbTmDex :: Maybe Int, -- Teal Mask
    dbType1Id :: Int,
    dbType2Id :: Maybe Int,
    dbHp :: Int,
    dbAtk :: Int,
    dbDef :: Int,
    dbSpa :: Int,
    dbSpd :: Int,
    dbSpe :: Int,
    dbEggGroup1Id :: Int,
    dbEggGroup2Id :: Maybe Int,
    dbGenderRatioId :: Int,
    dbAbility1Id :: Int,
    dbAbility2Id :: Maybe Int,
    dbHiddenAbilityId :: Maybe Int,
    dbEggCycles :: Int
  }
  deriving (Eq, Ord, Show)

instance FromRow DBPokemon where
  fromRow =
    DBPokemon
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

makeName :: Text -> Maybe Text -> Text
makeName name form = case form of
  Just f
    | "Alolan" `T.isPrefixOf` f
        || "Galarian" `T.isPrefixOf` f
        || "Hisuian" `T.isPrefixOf` f
        || "Paldean" `T.isPrefixOf` f ->
        f
  Just f | otherwise -> name <> " (" <> f <> ")"
  Nothing -> name

-- * Look up a Pokemon by name.

-- | Data type representing the return type when searching for a Pokemon by
-- name.
data GetPokemonResult
  = NoneFound
  | NoneFoundButSuggesting (NonEmpty Text) -- Suggestions for uniqueNames
  | FoundOne DBPokemon
  | AliasedToAndSuggesting DBPokemon (NonEmpty Text)
  deriving (Eq, Ord, Show)

_getPokemonFromId :: Int -> Connection -> IO DBPokemon
_getPokemonFromId id' conn = do
  [x] <-
    query
      conn
      [sql|SELECT id, name, form, unique_name, ndex, galar_dex, ioa_dex, ct_dex, paldea_dex,
                  tm_dex, type1_id, type2_id, hp, atk, def, spa, spd, spe, eg1_id, eg2_id,
                  gr_id, ability1_id, ability2_id, ha_id, egg_cycles
             FROM pokemon
             WHERE id = ?;|]
      (Only id')
  pure x

-- | Get a Pokemon's ID and details from its name.
getPokemon :: Text -> Connection -> IO GetPokemonResult
getPokemon name conn = do
  let hyphenatedName =
        T.replace "farfetch'd" "farfetchd"
          . T.replace "sirfetch'd" "sirfetchd"
          . T.replace "mr.-mime" "mr-mime"
          . T.replace "mime-jr." "mime-jr"
          . T.replace "mr.-rime" "mr-rime"
          . T.replace "flabébé" "flabebe"
          . T.toLower
          . T.intercalate "-"
          . T.words
          $ name
  -- Function to search exact match for an alias
  let getAliasedIds :: IO (Maybe Int)
      getAliasedIds = do
        matchingIds <- query conn [sql|SELECT pokemon_id FROM aliases WHERE alias ILIKE ?;|] (Only hyphenatedName)
        case map fromOnly matchingIds of
          [] -> pure Nothing
          [n] -> pure $ Just n
          _ -> error $ "getPokemon: multiple aliases found for " <> T.unpack hyphenatedName
  -- Look for results with the search string as a prefix
  potentialIdsAndNames :: [(Int, Text)] <-
    query conn [sql|SELECT id, unique_name FROM pokemon WHERE unique_name ILIKE ?;|] (Only $ hyphenatedName <> "%")
  case potentialIdsAndNames of
    [] -> do
      -- No match in unique_name, check aliases
      aliasedIds <- getAliasedIds
      case aliasedIds of
        Nothing -> pure NoneFound
        Just n -> FoundOne <$> _getPokemonFromId n conn
    [n] -> do
      -- Found one prefix. Check if an alias exists
      maybeAliasedId <- getAliasedIds
      case maybeAliasedId of
        Nothing ->
          -- If no alias, just use the prefix match
          FoundOne <$> _getPokemonFromId (fst n) conn
        Just aliasedId ->
          -- If the alias matches, no problem, else we override the
          -- prefix match with the alias. This can happen e.g. with
          -- 'mime' directing to 'mr-mime' vs 'mime-jr'.
          if fst n == aliasedId
            then FoundOne <$> _getPokemonFromId aliasedId conn
            else do
              aliasedPokemon <- _getPokemonFromId aliasedId conn
              pure $ AliasedToAndSuggesting aliasedPokemon (dbName aliasedPokemon :| [snd n])
    ns@(h : t) ->
      -- Check for an exact match
      case filter ((== hyphenatedName) . snd) ns of
        [] -> do
          -- No exact match, check for an alias
          maybeAliasedId <- getAliasedIds
          case maybeAliasedId of
            Nothing -> pure $ NoneFoundButSuggesting (NE.map snd (h :| t))
            Just n -> do
              aliasedPokemon <- _getPokemonFromId n conn
              pure $ AliasedToAndSuggesting aliasedPokemon (NE.map snd (h :| t))
        [(n, _)] -> FoundOne <$> _getPokemonFromId n conn
        _ -> error $ "getPokemon: multiple entries in pokemon table found for unique_name " <> T.unpack hyphenatedName

-- * Get the first form of any Pokemon with the same National Dex number

getPokemonWithSameNdex :: Text -> Connection -> IO GetPokemonResult
getPokemonWithSameNdex name conn = do
  results <- getPokemon name conn
  case results of
    NoneFound -> pure NoneFound
    FoundOne x -> pure $ FoundOne x
    AliasedToAndSuggesting x _ -> pure $ FoundOne x
    NoneFoundButSuggesting uniqueNames -> do
      (nDexes :: [Int]) <-
        map fromOnly
          <$> query
            conn
            [sql|SELECT ndex FROM pokemon WHERE unique_name IN ?;|]
            (Only $ In $ NE.toList uniqueNames)
      if all ((T.toLower name <> "-") `T.isPrefixOf`) uniqueNames
        && all (== head nDexes) nDexes
        then getPokemon (NE.head uniqueNames) conn
        else pure $ NoneFoundButSuggesting uniqueNames

-- * Evolution families and crossbreeding info

-- | Get the ID corresponding to the base form of a Pokemon.
getBaseForm :: PkmnId -> Connection -> IO PkmnId
getBaseForm pkmnId conn = do
  baseForm <-
    query
      conn
      [sql|SELECT base_evo_id FROM evolutions
           WHERE evo_id = ? OR prevo_id = ?
           LIMIT 1;|]
      (pkmnId, pkmnId)
  case baseForm of
    [] -> pure pkmnId -- No prevos.
    (Only baseFormId : _) -> pure baseFormId -- Found prevo.

-- | Get all parents of a Pokemon recursively. Note that this function should be
-- called on the base form of an evolution tree.
getAllParents :: PkmnId -> Connection -> IO [PkmnId]
getAllParents pkmnId conn = do
  evos <-
    query
      conn
      [sql|SELECT evo_id FROM evolutions
           WHERE base_evo_id = ?|]
      (Only pkmnId)
  pure $ pkmnId : map fromOnly evos

-- | Get all members of an evolution tree.
getAllEvolutionTreeMembers :: PkmnId -> Connection -> IO [PkmnId]
getAllEvolutionTreeMembers pkmnId conn = do
  baseForm <- getBaseForm pkmnId conn
  getAllParents baseForm conn

-- | Get all Pokemon forms which can be crossbred from a given list of Pokemon.
getAllCrossbreedableForms :: [PkmnId] -> Connection -> IO [PkmnId]
getAllCrossbreedableForms pkmnIds conn = do
  ns <-
    query
      conn
      [sql|
      SELECT p.id
        FROM pokemon as p
       WHERE (p.ndex IN (SELECT DISTINCT ndex FROM pokemon WHERE id IN ?) AND p.gr_id IN (3, 4, 5, 6, 7))
          OR p.id IN ?
      |]
      (In pkmnIds, In pkmnIds)
  pure $ map fromOnly ns

-- | Returns whether a Pokemon is unbreedable. This is true iff all members of
-- the Pokemon's evolution tree belong to unbreedable egg groups.
isPokemonUnbreedable :: PkmnId -> Connection -> IO Bool
isPokemonUnbreedable pkmnId conn = do
  evoFamilyIds <- getAllEvolutionTreeMembers pkmnId conn
  result <-
    query
      conn
      [sql|
      SELECT NOT EXISTS(
          SELECT * FROM pokemon as p2
          WHERE p2.id IN ?
          AND p2.eg1_id NOT IN (13, 15)
      );
      |]
      (Only $ In evoFamilyIds)
  case result of
    [Only True] -> pure True
    [Only False] -> pure False
    _ -> error "isPokemonUnbreedable: expected exactly one Boolean result"

-- * Abilities

-- | Generate a random ability.
randomAbility :: Connection -> IO (Text, Text)
randomAbility conn = do
  abty <-
    query_
      conn
      [sql|SELECT name, flavor_text FROM abilities ORDER BY RANDOM() LIMIT 1;|]
  case abty of
    [(name, ft)] -> pure (name, ft)
    _ -> error "randomAbility: could not get random ability from database"

getAbility :: Int -> Connection -> IO (Text, Text)
getAbility abilityId conn = do
  result <-
    query
      conn
      [sql|SELECT name, flavor_text FROM abilities WHERE id = ?;|]
      (Only abilityId)
  case result of
    [(name, ft)] -> pure (name, ft)
    _ -> error "getAbility: could not get ability from database"

-- * Egg moves

data Parent
  = LevelUpParent {lupPkmnName :: Text, lupLevel :: Int}
  | EvolutionParent {epPkmnName :: Text}
  | BreedParent {bpPkmnName :: Text}
  deriving (Eq, Ord, Show)

data EggMove = EggMove
  { emName :: Text,
    emFlavorText :: Text
  }
  deriving (Eq, Ord, Show)

data EggMoveParents = EggMoveParents
  { empMove :: EggMove,
    empParents :: [Parent]
  }
  deriving (Eq, Ord, Show)

getEggGroupsForBreeding :: PkmnId -> Connection -> IO [Text]
getEggGroupsForBreeding pkmnId conn = do
  eggGroups <-
    query
      conn
      [sql|SELECT eg1.name, eg2.name FROM pokemon as p
           LEFT JOIN egg_groups as eg1 ON p.eg1_id = eg1.id
           LEFT JOIN egg_groups as eg2 ON p.eg2_id = eg2.id
           WHERE p.id = ?;|]
      (Only pkmnId)
  case eggGroups of
    [(Just "Undiscovered", Nothing)] -> do
      -- Check if there is a parent with non-Undiscovered egg groups
      maybeParentId <-
        query
          conn
          [sql|SELECT evo_id FROM evolutions WHERE prevo_id = ? LIMIT 1;|]
          (Only pkmnId)
      case maybeParentId of
        [Only parentId] -> getEggGroupsForBreeding parentId conn
        _ -> pure ["Undiscovered"]
    [(Just eg1, Nothing)] -> pure [eg1]
    [(Just eg1, Just eg2)] -> pure [eg1, eg2]
    _ -> error $ "getEggGroupsForBreeding: got more than one result for pokemonId" <> show pkmnId

isMaleOnly :: PkmnId -> Connection -> IO Bool
isMaleOnly pkmnId conn = do
  res <- query conn [sql|SELECT gr_id FROM pokemon WHERE id = ?;|] (Only pkmnId)
  pure $ res == [Only (8 :: Int)]

getParentsGen78 :: [Text] -> [PkmnId] -> Text -> Game -> Connection -> IO [Parent]
getParentsGen78 eggGroups evoPokemonIds moveName game conn = do
  learnParents :: [Parent] <-
    map
      ( \(n, f, maybel) -> case maybel of
          Just l -> LevelUpParent (makeName n f) l
          Nothing -> EvolutionParent (makeName n f)
      )
      <$> query
        conn
        [sql|SELECT p.name, p.form, l.level FROM learnsets as l
                 LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                 LEFT JOIN moves as m ON l.move_id = m.id
                 LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                 LEFT JOIN games as g ON l.game_id = g.id
                 LEFT JOIN gender_ratios as gr ON p.gr_id = gr.id
                 LEFT JOIN egg_groups as eg1 ON p.eg1_id = eg1.id
                 LEFT JOIN egg_groups as eg2 ON p.eg2_id = eg2.id
                 WHERE
                   -- The egg move we're interested in
                   m.name = ?
                   -- The game we're looking in
                   AND g.name = ?
                   -- The type of parent we're looking for
                   AND (lm.name = 'Level up' OR lm.name = 'Evolution')
                   -- Remove parents that cannot breed
                   AND eg1.name != 'Undiscovered'
                   AND ((p.id IN ?) OR (gr.name != 'Genderless' AND gr.name != 'Female only'))
                   -- Shares egg groups with the desired parents
                   AND (eg1.name IN ? OR eg2.name IN ?)
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
        (moveName, show game, In evoPokemonIds, In eggGroups, In eggGroups)
  breedParents :: [Parent] <-
    map (\(n, f) -> BreedParent (makeName n f))
      <$> query
        conn
        [sql|SELECT p.name, p.form FROM learnsets as l
                 LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                 LEFT JOIN moves as m ON l.move_id = m.id
                 LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                 LEFT JOIN games as g ON l.game_id = g.id
                 LEFT JOIN gender_ratios as gr ON p.gr_id = gr.id
                 LEFT JOIN egg_groups as eg1 ON p.eg1_id = eg1.id
                 LEFT JOIN egg_groups as eg2 ON p.eg2_id = eg2.id
                 WHERE
                   -- The egg move we're interested in
                   m.name = ?
                   -- The game we're looking in
                   AND g.name = ?
                   -- The type of parent we're looking for
                   AND lm.name = 'Egg'
                   -- Remove parents that cannot breed
                   AND eg1.name != 'Undiscovered'
                   AND ((p.id IN ?) OR (gr.name != 'Genderless' AND gr.name != 'Female only'))
                   -- Shares egg groups with the desired parents
                   AND (eg1.name IN ? OR eg2.name IN ?)
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
        (moveName, show game, In evoPokemonIds, In eggGroups, In eggGroups)
  pure $ learnParents <> breedParents

-- | The same as above, but for Pokemon which are male only (meaning that the
-- only mons which can pass down egg moves are those in its evolution family).
getParentsGen78MaleOnly :: [Text] -> [PkmnId] -> Text -> Game -> Connection -> IO [Parent]
getParentsGen78MaleOnly eggGroups evoPokemonIds moveName game conn = do
  learnParents :: [Parent] <-
    map
      ( \(n, f, maybel) -> case maybel of
          Just l -> LevelUpParent (makeName n f) l
          Nothing -> EvolutionParent (makeName n f)
      )
      <$> query
        conn
        [sql|SELECT p.name, p.form, l.level FROM learnsets as l
                 LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                 LEFT JOIN moves as m ON l.move_id = m.id
                 LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                 LEFT JOIN games as g ON l.game_id = g.id
                 LEFT JOIN gender_ratios as gr ON p.gr_id = gr.id
                 LEFT JOIN egg_groups as eg1 ON p.eg1_id = eg1.id
                 LEFT JOIN egg_groups as eg2 ON p.eg2_id = eg2.id
                 WHERE
                   -- The egg move we're interested in
                   m.name = ?
                   -- The game we're looking in
                   AND g.name = ?
                   -- The type of parent we're looking for
                   AND (lm.name = 'Level up' OR lm.name = 'Evolution')
                   -- Remove parents that cannot breed
                   AND eg1.name != 'Undiscovered'
                   AND (p.id IN ?)
                   -- Shares egg groups with the desired parents
                   AND (eg1.name IN ? OR eg2.name IN ?)
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
        (moveName, show game, In evoPokemonIds, In eggGroups, In eggGroups)
  breedParents :: [Parent] <-
    map (\(n, f) -> BreedParent (makeName n f))
      <$> query
        conn
        [sql|SELECT p.name, p.form FROM learnsets as l
                 LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                 LEFT JOIN moves as m ON l.move_id = m.id
                 LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                 LEFT JOIN games as g ON l.game_id = g.id
                 LEFT JOIN gender_ratios as gr ON p.gr_id = gr.id
                 LEFT JOIN egg_groups as eg1 ON p.eg1_id = eg1.id
                 LEFT JOIN egg_groups as eg2 ON p.eg2_id = eg2.id
                 WHERE
                   -- The egg move we're interested in
                   m.name = ?
                   -- The game we're looking in
                   AND g.name = ?
                   -- The type of parent we're looking for
                   AND lm.name = 'Egg'
                   -- Remove parents that cannot breed
                   AND eg1.name != 'Undiscovered'
                   AND p.id IN ?
                   -- Shares egg groups with the desired parents
                   AND (eg1.name IN ? OR eg2.name IN ?)
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
        (moveName, show game, In evoPokemonIds, In eggGroups, In eggGroups)
  pure $ learnParents <> breedParents

getParentsGen9 :: Text -> Game -> Connection -> IO [Parent]
getParentsGen9 moveName game conn = do
  learnParents :: [Parent] <-
    map
      ( \(n, f, maybel) -> case maybel of
          Just l -> LevelUpParent (makeName n f) l
          Nothing -> EvolutionParent (makeName n f)
      )
      <$> query
        conn
        [sql|SELECT p.name, p.form, l.level FROM learnsets as l
                 LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                 LEFT JOIN moves as m ON l.move_id = m.id
                 LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                 LEFT JOIN games as g ON l.game_id = g.id
                 WHERE
                   -- The egg move we're interested in
                   m.name = ?
                   -- The game we're looking in
                   AND g.name = ?
                   -- The type of parent we're looking for
                   AND (lm.name = 'Level up' OR lm.name = 'Evolution')
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
        (moveName, show game)
  breedParents :: [Parent] <-
    map (\(n, f) -> BreedParent (makeName n f))
      <$> query
        conn
        [sql|SELECT p.name, p.form FROM learnsets as l
                 LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                 LEFT JOIN moves as m ON l.move_id = m.id
                 LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                 LEFT JOIN games as g ON l.game_id = g.id
                 WHERE
                   -- The egg move we're interested in
                   m.name = ?
                   -- The game we're looking in
                   AND g.name = ?
                   -- The type of parent we're looking for
                   AND lm.name = 'Egg'
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
        (moveName, show game)
  pure $ learnParents <> breedParents

-- | Generate a random set of moves.
randomMoves :: Int -> Connection -> IO [(Text, Text)]
randomMoves nMoves conn = do
  query
    conn
    [sql|SELECT name, flavor_text
             FROM moves
             ORDER BY RANDOM()
             LIMIT ?;|]
    (Only nMoves)

getEMs :: Game -> PkmnId -> Connection -> IO [EggMove]
getEMs game pkmnId conn = do
  movesAndFlavorTexts :: [(Text, Text)] <-
    query
      conn
      [sql|SELECT m.name, m.flavor_text FROM learnsets as l
                   LEFT JOIN moves as m ON l.move_id = m.id
                   LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                   LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                   LEFT JOIN games as g ON l.game_id = g.id
                   WHERE p.id = ? AND lm.name = 'Egg' AND g.name = ?;|]
      (pkmnId, show game)
  pure $ map (uncurry EggMove) movesAndFlavorTexts

getEMParents :: Game -> Int -> Connection -> IO [EggMoveParents]
getEMParents game pkmnId conn = do
  movesAndFlavorTexts :: [(Text, Text)] <-
    query
      conn
      [sql|SELECT m.name, m.flavor_text FROM learnsets as l
                   LEFT JOIN moves as m ON l.move_id = m.id
                   LEFT JOIN pokemon as p ON l.pokemon_id = p.id
                   LEFT JOIN learn_methods as lm ON l.learn_method_id = lm.id
                   LEFT JOIN games as g ON l.game_id = g.id
                   WHERE p.id = ? AND lm.name = 'Egg' AND g.name = ?;|]
      (pkmnId, T.pack (show game))
  forM movesAndFlavorTexts $ \(nm, ft) -> do
    parents <-
      if game `elem` [USUM, SwSh, BDSP]
        then do
          eggGroupNames <- getEggGroupsForBreeding pkmnId conn
          evoParentIds <- getAllEvolutionTreeMembers pkmnId conn
          isMaleOnly <- isMaleOnly pkmnId conn
          if isMaleOnly
            then getParentsGen78MaleOnly eggGroupNames evoParentIds nm game conn
            else getParentsGen78 eggGroupNames evoParentIds nm game conn
        else getParentsGen9 nm game conn
    print parents
    pure $ EggMoveParents (EggMove nm ft) parents
