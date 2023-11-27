{-# LANGUAGE QuasiQuotes #-}

module Apripsql.Queries
  ( GetPokemonResult (..),
    getPokemon,
    DBPokemon (..),

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
  | NoneFoundButSuggesting [Text] -- Suggestions for uniqueNames
  | FoundOne DBPokemon
  deriving (Eq, Ord, Show)

-- | Get a Pokemon's ID and details from its name.
getPokemon :: Text -> Connection -> IO GetPokemonResult
getPokemon name conn = do
  let hyphenatedName =
        T.replace "farfetch'd" "farfetchd"
          . T.replace "sirfetch'd" "sirfetchd"
          . T.replace "mr.-mime" "mr-mime"
          . T.replace "mime-jr." "mime-jr"
          . T.replace "mr.-rime" "mr-rime"
          . T.toLower
          . T.intercalate "-"
          . T.words
          $ name
  results <-
    query
      conn
      [sql|SELECT id, name, form, unique_name, ndex, galar_dex, ioa_dex, ct_dex, paldea_dex,
                  tm_dex, type1_id, type2_id, hp, atk, def, spa, spd, spe, eg1_id, eg2_id,
                  gr_id, ability1_id, ability2_id, ha_id, egg_cycles
             FROM pokemon
             WHERE unique_name ILIKE ?;|]
      (Only $ hyphenatedName <> "%")
  case results of
    [] -> pure NoneFound
    [x] -> pure $ FoundOne x
    xs -> case filter (\ps -> dbUniqueName ps == hyphenatedName) xs of
      [] -> pure $ NoneFoundButSuggesting (map dbUniqueName xs)
      [x] -> pure $ FoundOne x
      _ -> error "getPokemonIdsAndDetails: multiple results returned"

-- * Evolution families and crossbreeding info

-- | Get the ID corresponding to the base form of a Pokemon.
getBaseForm :: PkmnId -> Connection -> IO PkmnId
getBaseForm pkmnId conn = do
  baseForm <-
    query
      conn
      [sql|WITH RECURSIVE cte_base AS (
        SELECT prevo_id, evo_id, 0 as level FROM evolutions
        WHERE evo_id = ?
        UNION
        SELECT e.prevo_id, e.evo_id, level + 1 FROM evolutions e
        INNER JOIN cte_base c ON c.prevo_id = e.evo_id
      )
      SELECT prevo_id
      FROM (
        -- This gets all real prevos
        SELECT * FROM cte_base
        -- This adds the current form being searched for to the list
        UNION SELECT * FROM (VALUES (?, 0, -1)) AS t (prevo_id, evo_id, level)
        ORDER BY level DESC LIMIT 1
      ) AS t2;|]
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
      [sql|WITH RECURSIVE evos AS (
          SELECT prevo_id, evo_id FROM evolutions
          WHERE prevo_id = ?
          UNION
          SELECT e.prevo_id, e.evo_id FROM evolutions e
          INNER JOIN evos e2 ON e2.evo_id = e.prevo_id
        )
        SELECT evo_id FROM evos;|]
      (Only pkmnId)
  pure $ pkmnId : map fromOnly evos

-- | Get all members of an evolution tree. In principle, this could be
-- implemented as _getBaseForm >=> _getAllParents, but that would require two
-- database calls. Here we've merged it into one single query. I'm not actually
-- sure if this is faster, though.
getAllEvolutionTreeMembers :: PkmnId -> Connection -> IO [PkmnId]
getAllEvolutionTreeMembers pkmnId conn = do
  ns <-
    query
      conn
      [sql|
      WITH RECURSIVE
      cte_base AS (
        SELECT prevo_id, evo_id, 0 as level FROM evolutions
        WHERE evo_id = ?
        UNION
        SELECT e.prevo_id, e.evo_id, level + 1 FROM evolutions e
        INNER JOIN cte_base c ON c.prevo_id = e.evo_id
      ),
      base_form_id AS (
        SELECT prevo_id
        FROM (SELECT *
            FROM cte_base
            UNION SELECT * FROM (VALUES (?, 0, -1)) AS t (prevo_id, evo_id, level)
            ORDER BY level DESC LIMIT 1)
        AS t2
      ),
      cte_evos AS (
        SELECT prevo_id, evo_id, 0 as level FROM evolutions
        WHERE prevo_id = (SELECT prevo_id FROM base_form_id)
        UNION
        SELECT e.prevo_id, e.evo_id, level + 1 FROM evolutions e
        INNER JOIN cte_evos c ON c.evo_id = e.prevo_id
      )
      SELECT evo_id FROM cte_evos
      UNION (SELECT prevo_id FROM base_form_id)
      ORDER BY evo_id ASC;|]
      (pkmnId, pkmnId)
  pure $ map fromOnly ns

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

-- * TODO THIS IS NOT WORKING YET

getParentsGen78 :: [Text] -> Maybe Int -> Text -> Game -> Connection -> IO [Parent]
getParentsGen78 eggGroups evoFamilyId moveName game conn = do
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
                   AND ((p.evolution_family_id IS NOT NULL AND p.evolution_family_id = ?) OR (gr.name != 'Genderless' AND gr.name != 'Female only'))
                   -- Shares egg groups with the desired parents
                   AND (eg1.name in ? OR eg2.name in ?)
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
        (moveName, show game, evoFamilyId, In eggGroups, In eggGroups)
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
                   AND ((p.evolution_family_id IS NOT NULL AND p.evolution_family_id = ?) OR (gr.name != 'Genderless' AND gr.name != 'Female only'))
                   -- Shares egg groups with the desired parents
                   AND (eg1.name in ? OR eg2.name in ?)
                 ORDER BY p.ndex ASC, p.form ASC NULLS FIRST;|]
        (moveName, show game, evoFamilyId, In eggGroups, In eggGroups)
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
          eggGroups <- do
            (eg1, eg2) <-
              head
                <$> query
                  conn
                  [sql|SELECT eg1.name, eg2.name FROM pokemon as p
                               LEFT JOIN egg_groups as eg1 ON p.eg1_id = eg1.id
                               LEFT JOIN egg_groups as eg2 ON p.eg2_id = eg2.id
                               WHERE p.id = ?;|]
                  (Only pkmnId)
            case eg2 of
              Just eg2' -> pure [eg1, eg2']
              Nothing -> pure [eg1]
          familyId <-
            fromOnly . head
              <$> query
                conn
                [sql|SELECT evolution_family_id FROM pokemon WHERE id = ?;|]
                (Only pkmnId)
          getParentsGen78 eggGroups familyId nm game conn
        else getParentsGen9 nm game conn
    pure $ EggMoveParents (EggMove nm ft) parents
