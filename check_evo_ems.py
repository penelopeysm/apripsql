import psycopg2
import os

conn = psycopg2.connect(dsn=os.getenv('FLY_PG_PROXY_CONN_STRING'))
cur = conn.cursor()

cur.execute("SELECT DISTINCT evolution_family_id FROM pokemon")
evo_family_ids = [t[0] for t in cur.fetchall() if t[0] is not None]

def get_pkmn_ids_in_evo_family(evo_family_id):
    cur.execute("SELECT id FROM pokemon WHERE evolution_family_id = %s", (evo_family_id,))
    return [t[0] for t in cur.fetchall()]

families_by_pkmn_ids = [get_pkmn_ids_in_evo_family(evo_family_id) for evo_family_id in evo_family_ids]

families_with_multiple_pkmn = [family for family in families_by_pkmn_ids if len(family) > 1]

def get_egg_moves(pkmn_id, game_id):
    cur.execute("SELECT move_id FROM learnsets WHERE pokemon_id = %s AND game_id = %s AND learn_method_id = 4", (pkmn_id, game_id))
    return set(t[0] for t in cur.fetchall())

for family in families_with_multiple_pkmn:
    for game_id in range(1, 5):
        print("Checking family {} in game {}".format(family, game_id))
        egg_moves = [get_egg_moves(pkmn_id, game_id) for pkmn_id in family]
        # check that all elements of egg_moves are equal
        if not all(egg_moves[0] == egg_move for egg_move in egg_moves):
            print("DIFFERENT EGG MOVES FOUND")
