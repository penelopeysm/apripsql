{-# LANGUAGE QuasiQuotes #-}

module Apripsql.Queries
  ( GetPokemonResult (..),
    getPokemon,
    DBPokemon (..),
    getBaseForm,
    getAllParents,
    getAllEvolutionTreeMembers,
    getAllCrossbreedableForms,
    isPokemonUnbreedable,
    getHiddenAbility,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics (Generic)
import Setup.Pokemon (PokemonFinal (..))

-- | For more descriptive type signatures.
type PkmnId = Int

data DBPokemon = DBPokemon
  { pkmnId :: PkmnId,
    pkmn :: PokemonFinal
  }
  deriving (Eq, Ord, Show)

instance FromRow DBPokemon where
  fromRow =
    DBPokemon
      <$> field
      <*> ( PokemonFinal
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
          )

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
    xs -> case filter (\ps -> uniqueName (pkmn ps) == hyphenatedName) xs of
      [] -> pure $ NoneFoundButSuggesting (map (uniqueName . pkmn) xs)
      [x] -> pure $ FoundOne x
      _ -> error "getPokemonIdsAndDetails: multiple results returned"

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

getHiddenAbility :: PkmnId -> Connection -> IO (Maybe (Text, Text))
getHiddenAbility pkmnId conn = do
  result <-
    query
      conn
      [sql|SELECT a.name, a.flavor_text
             FROM pokemon p LEFT OUTER JOIN abilities a
             ON p.ha_id = a.id
             WHERE p.id = ?;|]
      (Only pkmnId)
  case result of
    [(Just name, Just flavorText)] -> pure $ Just (name, flavorText)
    [(Nothing, Nothing)] -> pure Nothing
    _ -> error $ "HA query returned invalid results (this should not happen!): " <> show result
