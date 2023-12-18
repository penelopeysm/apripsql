import requests
from bs4 import BeautifulSoup
from pathlib import Path

# canonicalise levels
def canon(s):
    try:
        return int(s)
    except ValueError as e:
        if s == '—':
            return 1
        elif s == 'Evolve':
            return None
        else:
            raise e

SV_GAME_NAME = 'SV'
LEVEL_UP_MOVE_LEARN_METHOD_NAME = 'Level up'
EGG_MOVE_LEARN_METHOD_NAME = 'Egg'
TUTOR_MOVE_LEARN_METHOD_NAME = 'Tutor'
TM_MOVE_LEARN_METHOD_NAME = 'TM'

def get_soup(pokemon):
    if pokemon.startswith('minior'):
        pokemon = 'minior'
    pokemon = pokemon.replace('-alola', '').replace('-galar', '').replace('-bloodmoon', '')
    url = f'https://www.serebii.net/pokedex-sv/{pokemon}/'
    r = requests.get(url)
    return BeautifulSoup(r.text, "html.parser")

def fix_move_name(move):
    if move == 'Vise Grip':
        return 'Vice Grip'
    if move == 'Baby-doll Eyes':
        return 'Baby-Doll Eyes'
    if move == 'Mud-slap':
        return 'Mud-Slap'
    if move == 'X-scissor':
        return 'X-Scissor'
    return move

# Normal mons

def get_ems(soup):
    # Reminder moves are classified as 'EMs' in pokeapi (...)
    def get_true_ems(soup):
        try:
            tbl = soup.find('h3', string='Egg Moves').find_parent('table')
            rows = tbl.find_all('tr')
            first_tds = [td for td in [row.find('td') for row in rows][1:] if td]
            moves = [a.text for a in [td.find('a') for td in first_tds] if a]
            lowercased_moves = [fix_move_name(move) for move in moves]
            return lowercased_moves
        except AttributeError:
            return []
    def get_reminder_moves(soup):
        try:
            tbl = soup.find('h3', string='Move Reminder Only Attacks').find_parent('table')
            rows = tbl.find_all('tr')
            first_tds = [td for td in [row.find('td') for row in rows][1:] if td]
            moves = [a.text for a in [td.find('a') for td in first_tds] if a]
            lowercased_moves = [fix_move_name(move) for move in moves]
            return lowercased_moves
        except AttributeError:
            return []
    return get_true_ems(soup) + get_reminder_moves(soup)

def get_level_up_moves(soup):
    try:
        tbl = soup.find('a', {'name': 'standardlevel'}).find_parent('table')
        rows = tbl.find_all('tr')
        first_tds = [tds[0] for tds in [row.find_all('td') for row in rows][1:] if len(tds) > 0]
        levels = [canon(td.text) for td in first_tds if td.has_attr('rowspan') and td['rowspan'] == '2']
        second_tds = [tds[1] for tds in [row.find_all('td') for row in rows][1:] if len(tds) > 1]
        moves = [a.text for a in [td.find('a') for td in second_tds] if a]
        lowercased_moves = [fix_move_name(move) for move in moves]
        return list(zip(lowercased_moves, levels))
    except AttributeError:
        return []

def get_tm_moves(soup):
    try:
        tbl = soup.find('h3', string='Technical Machine Attacks').find_parent('table')
        rows = tbl.find_all('tr')
        first_tds = [tds[1] for tds in [row.find_all('td') for row in rows][1:] if len(tds) > 1]
        moves = [a.text for a in [td.find('a') for td in first_tds] if a]
        lowercased_moves = [fix_move_name(move) for move in moves]
        return lowercased_moves
    except AttributeError:
        return []

# Mons with regional forms

def get_level_up_moves_orig(soup):
    return get_level_up_moves(soup)

def get_level_up_moves_regional(soup):
    # Since there's no Meowth, we don't need to differentiate between
    # alolalevel and galarlevel
    try:
        anchor = soup.find('a', {'name': 'alolalevel'})
        if anchor is None:
            anchor = soup.find('a', {'name': 'galarianlevel'})
        if anchor is None:
            raise ValueError("No anchor for regional level up table found")
        tbl = anchor.find_parent('table')
        rows = tbl.find_all('tr')
        first_tds = [tds[0] for tds in [row.find_all('td') for row in rows][1:] if len(tds) > 0]
        levels = [canon(td.text) for td in first_tds if td.has_attr('rowspan') and td['rowspan'] == '2']
        second_tds = [tds[1] for tds in [row.find_all('td') for row in rows][1:] if len(tds) > 1]
        moves = [a.text for a in [td.find('a') for td in second_tds] if a]
        lowercased_moves = [fix_move_name(move) for move in moves]
        return list(zip(lowercased_moves, levels))
    except AttributeError:
        return []

def get_ems_orig(soup):
    def get_real_ems(soup):
        try:
            tbl = soup.find('h3', string='Egg Moves').find_parent('table')
            rows = tbl.find_all('tr')
            first_and_last_tds = [(tds[0], tds[-1])
                                  for tds in [row.find_all('td') for row in rows][1:]
                                  if len(tds) > 1]
            moves = [first.find('a').text
                     for (first, last) in first_and_last_tds
                     if last.find('img', {'alt': 'Normal'}) is not None]
            lowercased_moves = [fix_move_name(move) for move in moves]
            return lowercased_moves
        except AttributeError:
            return []
    def get_reminder_moves(soup):
        try:
            tbl = soup.find('h3', string='Move Reminder Only Attacs').find_parent('table')
            rows = tbl.find_all('tr')
            first_and_last_tds = [(tds[0], tds[-1])
                                  for tds in [row.find_all('td') for row in rows][1:]
                                  if len(tds) > 1]
            moves = [first.find('a').text
                     for (first, last) in first_and_last_tds
                     if last.find('img', {'alt': 'Normal'}) is not None]
            lowercased_moves = [fix_move_name(move) for move in moves]
            return lowercased_moves
        except AttributeError:
            return []
    return get_real_ems(soup) + get_reminder_moves(soup)

def get_ems_regional(soup):
    def get_real_ems(soup):
        try:
            tbl = soup.find('h3', string='Egg Moves').find_parent('table')
            rows = tbl.find_all('tr')
            first_and_last_tds = [(tds[0], tds[-1])
                                  for tds in [row.find_all('td') for row in rows][1:]
                                  if len(tds) > 1]
            moves = [first.find('a').text
                     for (first, last) in first_and_last_tds
                     if last.find(lambda elem: elem.name == 'img' and 'alt' in elem.attrs and "Form" in elem['alt'])]
            lowercased_moves = [fix_move_name(move) for move in moves]
            return lowercased_moves
        except AttributeError:
            return []
    def get_reminder_moves(soup):
        try:
            tbl = soup.find('h3', string='Move Reminder Only Attacks').find_parent('table')
            rows = tbl.find_all('tr')
            first_and_last_tds = [(tds[0], tds[-1])
                                  for tds in [row.find_all('td') for row in rows][1:]
                                  if len(tds) > 1]
            moves = [first.find('a').text
                     for (first, last) in first_and_last_tds
                     if last.find(lambda elem: elem.name == 'img' and 'alt' in elem.attrs and "Form" in elem['alt'])]
            lowercased_moves = [fix_move_name(move) for move in moves]
            return lowercased_moves
        except AttributeError:
            return []
    return get_real_ems(soup) + get_reminder_moves(soup)

def get_tm_moves_orig(soup):
    try:
        tbl = soup.find('h3', string='Technical Machine Attacks').find_parent('table')
        rows = tbl.find_all('tr')
        first_and_last_tds = [(tds[1], tds[-2], tds[-1])
                              for tds in [row.find_all('td') for row in rows][1:]
                              if len(tds) > 2]
        moves = [first.find('a').text
                 for (first, secondlast, last) in first_and_last_tds
                 if (last.find('img', {'alt': 'Normal'}) is not None
                     or secondlast.find('img', {'alt': 'Normal'}) is not None)]
        lowercased_moves = [fix_move_name(move) for move in moves]
        return lowercased_moves
    except AttributeError:
        return []

def get_tm_moves_regional(soup):
    try:
        tbl = soup.find('h3', string='Technical Machine Attacks').find_parent('table')
        rows = tbl.find_all('tr')
        first_and_last_tds = [(tds[1], tds[-1])
                              for tds in [row.find_all('td') for row in rows][1:]
                              if len(tds) > 2]
        moves = [first.find('a').text
                 for (first, last) in first_and_last_tds
                 if last.find(lambda elem: elem.name == 'img' and 'alt' in elem.attrs and "Form" in elem['alt'])]
        lowercased_moves = [fix_move_name(move) for move in moves]
        return lowercased_moves
    except AttributeError:
        return []

# Generate CSVs from scraped data

def make_em_csv(pokemon_name, egg_moves):
    def csv_one(pokemon_name, move_name):
        return ','.join([
            str(pokemon_name),
            str(SV_GAME_NAME),
            str(move_name),
            str(EGG_MOVE_LEARN_METHOD_NAME),
            ''
        ])
    return '\n'.join([csv_one(pokemon_name, move_name) for move_name in sorted(egg_moves)])

def make_learn_csv(pokemon_name, level_up_moves: list[tuple[str, int]]):
    def csv_one(pokemon_name, move_name, level):
        if level is None:  # Evolution
            return ','.join([
                str(pokemon_name),
                str(SV_GAME_NAME),
                str(move_name),
                'Evolution',
                ''
            ])
        else:
            return ','.join([
                str(pokemon_name),
                str(SV_GAME_NAME),
                str(move_name),
                str(LEVEL_UP_MOVE_LEARN_METHOD_NAME),
                str(level)
            ])
    s = ""
    for (move_name, level) in level_up_moves:
        s += csv_one(pokemon_name, move_name, level) + '\n'
    return s.strip()

def make_tm_csv(pokemon_name, move_names):
    def csv_one(pokemon_name, move_name):
        return ','.join([
            str(pokemon_name),
            str(SV_GAME_NAME),
            str(move_name),
            str(TM_MOVE_LEARN_METHOD_NAME),
            ''
        ])
    return '\n'.join([csv_one(pokemon_name, move_name) for move_name in sorted(move_names)])


## ---------------------------------------------------------------------------

def get_line_number_to_insert_after(lines, species_id):
    try:
        i = 0
        while lines[i].split(',')[0] != species_id:
            i += 1
        while lines[i].split(',')[0] == species_id:
            i += 1
        return i
    except IndexError:
        return len(lines)

NEW_MONS = """
doduo dodrio
rhyhorn rhydon rhyperior
elekid electabuzz electivire
magby magmar magmortar
happiny chansey blissey
blitzle zebstrika
smeargle
milcery alcremie
trapinch vibrava flygon
pikipek trumbeak toucannon
tentacool tentacruel
horsea seadra kingdra
cottonee whimsicott
comfey
oddish gloom vileplume bellossom
chinchou lanturn
inkay malamar
dewpider araquanid
tyrogue hitmonlee hitmonchan hitmontop
drilbur excadrill
espurr
minior-core minior-meteor
cranidos rampardos
shieldon bastiodon
minccino cinccino
skarmory
plusle minun
scraggy scrafty
golett golurk
porygon porygon2 porygon-z
joltik galvantula
beldum metang metagross
seel dewgong
lapras
solosis duosion reuniclus
snubbull granbull
bulbasaur ivysaur venusaur
charmander charmeleon charizard
squirtle wartortle blastoise
chikorita bayleef meganium
cyndaquil quilava typhlosion
totodile croconaw feraligatr
treecko grovyle sceptile
torchic combusken blaziken
mudkip marshtomp swampert
turtwig grotle torterra
chimchar monferno infernape
piplup prinplup empoleon
snivy servine serperior
tepig pignite emboar
oshawott dewott samurott
chespin quilladin chesnaught
fennekin braixen delphox
froakie frogadier greninja
rowlet dartrix decidueye
litten torracat incineroar
popplio brionne primarina
grookey thwackey rillaboom
scorbunny raboot cinderace
sobble drizzile inteleon
""".split()

# Meowstic has male and female ...

NEW_MONS_WITH_REGIONALS = """
exeggutor exeggutor-alola
""".split()

lines = []

for pokemon_name in NEW_MONS:
    print(pokemon_name)

    soup = get_soup(pokemon_name)
    lup_csv = make_learn_csv(pokemon_name, get_level_up_moves(soup))
    ems_csv = make_em_csv(pokemon_name, get_ems(soup))
    tm_csv = make_tm_csv(pokemon_name, get_tm_moves(soup))

    csv_lines = "\n".join([lup_csv, ems_csv, tm_csv]).split('\n')
    lines += csv_lines

for pokemon_name in NEW_MONS_WITH_REGIONALS:
    print(pokemon_name)

    if pokemon_name.endswith('-galar') or pokemon_name.endswith('-alola'):
        pokemon_name_serebii = pokemon_name[:-6]
        regional = True
        soup = get_soup(pokemon_name_serebii)
        lup_csv = make_learn_csv(pokemon_name, get_level_up_moves_regional(soup))
        ems_csv = make_em_csv(pokemon_name, get_ems_regional(soup))
        tm_csv = make_tm_csv(pokemon_name, get_tm_moves_regional(soup))
    else:
        pokemon_name_serebii = pokemon_name
        regional = False
        soup = get_soup(pokemon_name_serebii)
        lup_csv = make_learn_csv(pokemon_name, get_level_up_moves_orig(soup))
        ems_csv = make_em_csv(pokemon_name, get_ems_orig(soup))
        tm_csv = make_tm_csv(pokemon_name, get_tm_moves_orig(soup))

    csv_lines = "\n".join([lup_csv, ems_csv, tm_csv]).split('\n')
    lines += csv_lines

lines = [l for l in lines if l.strip() != '']
lines.insert(0, 'unique_name,game,move_name,learn_method,level')
with open(Path(__file__).parent / 'learnsets-dlc2.csv', 'w') as f:
    f.writelines(l.strip() + "\n" for l in lines)
