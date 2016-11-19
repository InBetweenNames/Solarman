{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude #-}
module XSaiga.Interactive where

import XSaiga.SolarmanTriplestore
import XSaiga.Getts
import Control.Monad

a $ b = a b
infixr 0 $
a . b = \x -> a (b (x))
infixr 9 .

discover tmph = make_filtered_relation dataStore "discover_ev" [(["object"],tmph)]
discover' tmph preps = make_filtered_relation dataStore "discover_ev" $ (["object"], tmph):preps
discover_ = make_inverted_filtered_relation dataStore "discover_ev"

discovers tmph = make_filtered_relation dataStore "discover_ev" [(["object"],tmph)]
discovers' tmph preps = make_filtered_relation dataStore "discover_ev" $ (["object"], tmph):preps
discovers_ = make_inverted_filtered_relation dataStore "discover_ev"

discovered tmph = make_filtered_relation dataStore "discover_ev" [(["object"],tmph)]
discovered' tmph preps = make_filtered_relation dataStore "discover_ev" $ (["object"], tmph):preps
discovered_ = make_inverted_filtered_relation dataStore "discover_ev"

orbit tmph = make_filtered_relation dataStore "orbit_ev" [(["object"],tmph)]
orbit' tmph preps = make_filtered_relation dataStore "orbit_ev" $ (["object"], tmph):preps
orbit_ = make_inverted_filtered_relation dataStore "orbit_ev"

orbits tmph = make_filtered_relation dataStore "orbit_ev" [(["object"],tmph)]
orbits' tmph preps = make_filtered_relation dataStore "orbit_ev" $ (["object"], tmph):preps
orbits_ = make_inverted_filtered_relation dataStore "orbit_ev"

orbited tmph = make_filtered_relation dataStore "orbit_ev" [(["object"],tmph)]
orbited' tmph preps = make_filtered_relation dataStore "orbit_ev" $ (["object"], tmph):preps
orbited_ = make_inverted_filtered_relation dataStore "orbit_ev"

discoverer = get_subjs_of_event_type dataStore "discover_ev"
discoverers = get_subjs_of_event_type dataStore "discover_ev"
anyone = a person
anything = a thing
anybody = a person
someone = a person
something = a thing
somebody = a person
everyone = every person
everything = every thing
everybody = every person
telescopes = telescope
places = place
thing = get_members dataStore "thing"
things = get_members dataStore "thing"
planets = get_members dataStore "planet"
planet = get_members dataStore "planet"
person = get_members dataStore "person"
sun = get_members dataStore "sun"
moon = get_members dataStore "moon"
moons = get_members dataStore "moon"
satellite = get_members dataStore "moon"
satellites = get_members dataStore "moon"
atmospheric = get_members dataStore "atmospheric"
blue = get_members dataStore "blue"
solid = get_members dataStore "solid"
brown = get_members dataStore "brown"
gaseous = get_members dataStore "gaseous"
green = get_members dataStore "green"
red = get_members dataStore "red"
ringed = get_members dataStore "ringed"
vacuumous = get_members dataStore "vacuumous"
exist = get_members dataStore "thing"
exists = get_members dataStore "thing"
spin = get_members dataStore "spin"
spins = get_members dataStore "spin"
bernard = make_pnoun "bernard"
bond = make_pnoun "bond"
venus = make_pnoun "venus"
cassini = make_pnoun "cassini"
dollfus = make_pnoun "dollfus"
galileo = make_pnoun "galileo"
hall = make_pnoun "hall"
herschel = make_pnoun "herschel"
huygens = make_pnoun "huygens"
kowal = make_pnoun "kowal"
kuiper = make_pnoun "kuiper"
larsen = make_pnoun "larsen"
lassell = make_pnoun "lassell"
melotte = make_pnoun "melotte"
nicholson = make_pnoun "nicholson"
perrine = make_pnoun "perrine"
pickering = make_pnoun "pickering"
almathea = make_pnoun "almathea"
ariel = make_pnoun "ariel"
callisto = make_pnoun "callisto"
charon = make_pnoun "charon"
deimos = make_pnoun "deimos"
dione = make_pnoun "dione"
earth = make_pnoun "earth"
enceladus = make_pnoun "enceladus"
europa = make_pnoun "europa"
ganymede = make_pnoun "ganymede"
hyperion = make_pnoun "hyperion"
iapetus = make_pnoun "iapetus"
io = make_pnoun "io"
janus = make_pnoun "janus"
jupiter = make_pnoun "jupiter"
jupitereighth = make_pnoun "jupitereighth"
jupitereleventh = make_pnoun "jupitereleventh"
jupiterfourteenth = make_pnoun "jupiterfourteenth"
jupiterninth = make_pnoun "jupiterninth"
jupiterseventh = make_pnoun "jupiterseventh"
jupitersixth = make_pnoun "jupitersixth"
jupitertenth = make_pnoun "jupitertenth"
jupiterthirteenth = make_pnoun "jupiterthirteenth"
jupitertwelfth = make_pnoun "jupitertwelfth"
luna = make_pnoun "luna"
mars = make_pnoun "mars"
mercury = make_pnoun "mercury"
mimas = make_pnoun "mimas"
miranda = make_pnoun "miranda"
neptune = make_pnoun "neptune"
nereid = make_pnoun "nereid"
oberon = make_pnoun "oberon"
phobos = make_pnoun "phobos"
phoebe = make_pnoun "phoebe"
pluto = make_pnoun "pluto"
rhea = make_pnoun "rhea"
saturn = make_pnoun "saturn"
saturnfirst = make_pnoun "saturnfirst"
sol = make_pnoun "sol"
tethys = make_pnoun "tethys"
titan = make_pnoun "titan"
titania = make_pnoun "titania"
triton = make_pnoun "triton"
umbriel = make_pnoun "umbriel"
uranus = make_pnoun "uranus"
human = get_members dataStore "person"
humans = get_members dataStore "person"
people = get_members dataStore "person"
telescope = get_members dataStore "telescope"
cassegrain_telescope = make_pnoun "cassegrain_telescope"
hooker_telescope = make_pnoun "hooker_telescope"
schmidt_telescope = make_pnoun "schmidt_telescope"
subaru_reflector_telescope = make_pnoun "subaru_reflector_telescope"
canada_france_hawaii_telescope = make_pnoun "canada-france-hawaii_telescope"
aerial_telescope_1 = make_pnoun "aerial_telescope_1"
bruce_astrograph = make_pnoun "bruce_astrograph"
telescope_1 = make_pnoun "telescope_1"
hale_telescope = make_pnoun "hale_telescope"
hubble_space_telescope = make_pnoun "hubble_space_telescope"
blanco_telescope = make_pnoun "blanco_telescope"
crossley_reflector_telescope = make_pnoun "crossley_reflector_telescope"
refractor_telescope_1 = make_pnoun "refractor_telescope_1"
refractor_telescope_2 = make_pnoun "refractor_telescope_2"
refractor_telescope_3 = make_pnoun "refractor_telescope_3"
refractor_telescope_4 = make_pnoun "refractor_telescope_4"
reflector_telescope_1 = make_pnoun "reflector_telescope_1"
reflector_telescope_2 = make_pnoun "reflector_telescope_2"
reflector_telescope_3 = make_pnoun "reflector_telescope_3"
reflector_telescope_4 = make_pnoun "reflector_telescope_4"
ground_based_telescope_1 = make_pnoun "ground_based_telescope_1"
ground_based_telescope_2 = make_pnoun "ground_based_telescope_2"
ground_based_telescope_3 = make_pnoun "ground_based_telescope_3"
galilean_telescope_1 = make_pnoun "galilean_telescope_1"
team = get_members dataStore "science_team"
teams = get_members dataStore "science_team"
voyager_science_team = make_pnoun "voyager_science_team"
cassini_imaging_science_team = make_pnoun "cassini_imaging_science_team"
science_team_1 = make_pnoun "science_team_1"
science_team_2 = make_pnoun "science_team_2"
science_team_3 = make_pnoun "science_team_3"
science_team_4 = make_pnoun "science_team_4"
science_team_5 = make_pnoun "science_team_5"
science_team_6 = make_pnoun "science_team_6"
science_team_7 = make_pnoun "science_team_7"
science_team_8 = make_pnoun "science_team_8"
science_team_9 = make_pnoun "science_team_9"
science_team_10 = make_pnoun "science_team_10"
science_team_11 = make_pnoun "science_team_11"
science_team_12 = make_pnoun "science_team_12"
science_team_13 = make_pnoun "science_team_13"
science_team_14 = make_pnoun "science_team_14"
science_team_15 = make_pnoun "science_team_15"
science_team_16 = make_pnoun "science_team_16"
science_team_17 = make_pnoun "science_team_17"
science_team_18 = make_pnoun "science_team_18"
science_team_19 = make_pnoun "science_team_19"
science_team_20 = make_pnoun "science_team_20"
science_team_21 = make_pnoun "science_team_21"
larson = make_pnoun "larson"
fountain = make_pnoun "fountain"
scotti = make_pnoun "scotti"
spahr = make_pnoun "spahr"
mcmillan = make_pnoun "mcmillan"
montani = make_pnoun "montani"
gleason = make_pnoun "gleason"
gehrels = make_pnoun "gehrels"
roemer = make_pnoun "roemer"
sheppard = make_pnoun "sheppard"
jewitt = make_pnoun "jewitt"
fernandez = make_pnoun "fernandez"
magnier = make_pnoun "magnier"
kleyna = make_pnoun "kleyna"
gladman = make_pnoun "gladman"
kavelaars = make_pnoun "kavelaars"
petit = make_pnoun "petit"
allen = make_pnoun "allen"
laques = make_pnoun "laques"
lecacheux = make_pnoun "lecacheux"
smith = make_pnoun "smith"
reitsema = make_pnoun "reitsema"
pascu = make_pnoun "pascu"
seidelmann = make_pnoun "seidelmann"
baum = make_pnoun "baum"
currie = make_pnoun "currie"
showalter = make_pnoun "showalter"
scholl = make_pnoun "scholl"
holman = make_pnoun "holman"
marsden = make_pnoun "marsden"
burns = make_pnoun "burns"
milisavljevic = make_pnoun "milisavljevic"
grav = make_pnoun "grav"
karkoschka = make_pnoun "karkoschka"
lissauer = make_pnoun "lissauer"
fraser = make_pnoun "fraser"
christy = make_pnoun "christy"
weaver = make_pnoun "weaver"
stern = make_pnoun "stern"
mutchler = make_pnoun "mutchler"
steffl = make_pnoun "steffl"
buie = make_pnoun "buie"
merline = make_pnoun "merline"
spencer = make_pnoun "spencer"
young_e_f = make_pnoun "young_e_f"
young_l_a = make_pnoun "young_l_a"
hamilton = make_pnoun "hamilton"
soummer = make_pnoun "soummer"
throop = make_pnoun "throop"
spacecraft = get_members dataStore "spacecraft"
spacecrafts = get_members dataStore "spacecrafts"
voyager_1 = make_pnoun "voyager_1"
voyager_2 = make_pnoun "voyager_2"
place = get_members dataStore "place"
mt_hopkins = make_pnoun "mt_hopkins"
fort_davis = make_pnoun "fort_davis"
cerro_tololo = make_pnoun "cerro_tololo"
us_naval_observatory = make_pnoun "us_naval_observatory"
padua = make_pnoun "padua"
mt_hamilton = make_pnoun "mt_hamilton"
greenwich = make_pnoun "greenwich"
mt_wilson = make_pnoun "mt_wilson"
mt_palomar = make_pnoun "mt_palomar"
kitt_peak = make_pnoun "kitt_peak"
mauna_kea = make_pnoun "mauna_kea"
slough = make_pnoun "slough"
paris = make_pnoun "paris"
the_hague = make_pnoun "the_hague"
cambridge = make_pnoun "cambridge"
liverpool = make_pnoun "liverpool"
arequipa = make_pnoun "arequipa"
pic_du_midi = make_pnoun "pic_du_midi"
flagstaff = make_pnoun "flagstaff"
la_silla = make_pnoun "la_silla"
himalia = make_pnoun "himalia"
elara = make_pnoun "elara"
pasiphae = make_pnoun "pasiphae"
sinope = make_pnoun "sinope"
lysithea = make_pnoun "lysithea"
carme = make_pnoun "carme"
ananke = make_pnoun "ananke"
leda = make_pnoun "leda"
thebe = make_pnoun "thebe"
adrastea = make_pnoun "adrastea"
metis = make_pnoun "metis"
callirrhoe = make_pnoun "callirrhoe"
themisto = make_pnoun "themisto"
megaclite = make_pnoun "megaclite"
taygete = make_pnoun "taygete"
chaldene = make_pnoun "chaldene"
harpalyke = make_pnoun "harpalyke"
kalyke = make_pnoun "kalyke"
iocaste = make_pnoun "iocaste"
erinome = make_pnoun "erinome"
isonoe = make_pnoun "isonoe"
praxidike = make_pnoun "praxidike"
autonoe = make_pnoun "autonoe"
thyone = make_pnoun "thyone"
hermippe = make_pnoun "hermippe"
aitne = make_pnoun "aitne"
eurydome = make_pnoun "eurydome"
euanthe = make_pnoun "euanthe"
euporie = make_pnoun "euporie"
orthosie = make_pnoun "orthosie"
sponde = make_pnoun "sponde"
kale = make_pnoun "kale"
pasithee = make_pnoun "pasithee"
hegemone = make_pnoun "hegemone"
mneme = make_pnoun "mneme"
aoede = make_pnoun "aoede"
thelxinoe = make_pnoun "thelxinoe"
arche = make_pnoun "arche"
kallichore = make_pnoun "kallichore"
helike = make_pnoun "helike"
carpo = make_pnoun "carpo"
eukelade = make_pnoun "eukelade"
cyllene = make_pnoun "cyllene"
kore = make_pnoun "kore"
herse = make_pnoun "herse"
epimetheus = make_pnoun "epimetheus"
helene = make_pnoun "helene"
telesto = make_pnoun "telesto"
calypso = make_pnoun "calypso"
atlas = make_pnoun "atlas"
prometheus = make_pnoun "prometheus"
pandora = make_pnoun "pandora"
pan = make_pnoun "pan"
ymir = make_pnoun "ymir"
paaliaq = make_pnoun "paaliaq"
tarvos = make_pnoun "tarvos"
ijiraq = make_pnoun "ijiraq"
suttungr = make_pnoun "suttungr"
kiviuq = make_pnoun "kiviuq"
mundilfari = make_pnoun "mundilfari"
albiorix = make_pnoun "albiorix"
skathi = make_pnoun "skathi"
erriapus = make_pnoun "erriapus"
siarnaq = make_pnoun "siarnaq"
thrymr = make_pnoun "thrymr"
narvi = make_pnoun "narvi"
methone = make_pnoun "methone"
pallene = make_pnoun "pallene"
polydeuces = make_pnoun "polydeuces"
daphnis = make_pnoun "daphnis"
aegir = make_pnoun "aegir"
bebhionn = make_pnoun "bebhionn"
bergelmir = make_pnoun "bergelmir"
bestla = make_pnoun "bestla"
farbauti = make_pnoun "farbauti"
fenrir = make_pnoun "fenrir"
fornjot = make_pnoun "fornjot"
hati = make_pnoun "hati"
hyrrokkin = make_pnoun "hyrrokkin"
kari = make_pnoun "kari"
loge = make_pnoun "loge"
skoll = make_pnoun "skoll"
surtur = make_pnoun "surtur"
anthe = make_pnoun "anthe"
jarnsaxa = make_pnoun "jarnsaxa"
greip = make_pnoun "greip"
tarqeq = make_pnoun "tarqeq"
aegaeon = make_pnoun "aegaeon"
cordelia = make_pnoun "cordelia"
ophelia = make_pnoun "ophelia"
bianca = make_pnoun "bianca"
cressida = make_pnoun "cressida"
desdemona = make_pnoun "desdemona"
juliet = make_pnoun "juliet"
portia = make_pnoun "portia"
rosalind = make_pnoun "rosalind"
belinda = make_pnoun "belinda"
puck = make_pnoun "puck"
caliban = make_pnoun "caliban"
sycorax = make_pnoun "sycorax"
prospero = make_pnoun "prospero"
setebos = make_pnoun "setebos"
stephano = make_pnoun "stephano"
trinculo = make_pnoun "trinculo"
francisco = make_pnoun "francisco"
margaret = make_pnoun "margaret"
ferdinand = make_pnoun "ferdinand"
perdita = make_pnoun "perdita"
mab = make_pnoun "mab"
cupid = make_pnoun "cupid"
naiad = make_pnoun "naiad"
thalassa = make_pnoun "thalassa"
despina = make_pnoun "despina"
galatea = make_pnoun "galatea"
larissa = make_pnoun "larissa"
proteus = make_pnoun "proteus"
halimede = make_pnoun "halimede"
psamathe = make_pnoun "psamathe"
sao = make_pnoun "sao"
laomedeia = make_pnoun "laomedeia"
neso = make_pnoun "neso"
nix = make_pnoun "nix"
hydra = make_pnoun "hydra"
kerberos = make_pnoun "kerberos"
styx = make_pnoun "styx"
