const wordleWords = [
  "aback", "abase", "abate", "abbey", "abbot", "abhor", "abide", "abled", "abode", "abort",
  "about", "above", "abuse", "abyss", "acorn", "acrid", "actor", "acute", "adage", "adapt",
  "adept", "admin", "admit", "adobe", "adopt", "adore", "adorn", "adult", "affix", "afire",
  "afoot", "after", "again", "agape", "agate", "agent", "agile", "aging", "aglow", "agony",
  "agree", "ahead", "aisle", "alarm", "album", "alert", "algae", "alibi", "alien", "align",
  "alike", "alive", "allay", "alley", "allot", "allow", "alloy", "aloft", "alone", "along",
  "aloof", "aloud", "alpha", "altar", "alter", "amass", "amaze", "amber", "amble", "amend",
  "amiss", "amity", "among", "ample", "amply", "amuse", "angel", "anger", "angle", "angry",
  "angst", "anime", "ankle", "annex", "annoy", "annul", "anode", "antic", "anvil", "aorta",
  "apart", "aphid", "aping", "apnea", "apple", "apply", "apron", "aptly", "arbor", "ardor",
  "arena", "argue", "arise", "armor", "aroma", "arose", "array", "arrow", "arson", "artsy",
  "ascot", "ashen", "aside", "askew", "assay", "asset", "atoll", "atone", "attic", "audio",
  "audit", "avail", "avert", "avian", "avoid", "await", "awake", "award", "aware", "awash",
  "awful", "awoke", "axial", "axiom", "azure", "bacon", "badge", "badly", "bagel", "baggy",
  "baker", "baler", "balmy", "banal", "banjo", "barge", "baron", "basal", "basic", "basil",
  "basin", "basis", "baste", "batch", "bathe", "baton", "batty", "bawdy", "bayou", "beach",
  "beady", "beard", "beast", "beech", "beefy", "befit", "began", "begat", "beget", "begin",
  "begun", "being", "belch", "belie", "belle", "belly", "below", "bench", "beret", "berry",
  "berth", "beset", "betel", "bevel", "bezel", "bible", "bicep", "biddy", "bigot", "bilge",
  "billy", "binge", "bingo", "biome", "birch", "birth", "bison", "bitty", "black", "blade",
  "blame", "bland", "blank", "blare", "blast", "blaze", "bleak", "bleat", "bleed", "bleep",
  "blend", "bless", "blimp", "blind", "blink", "bliss", "blitz", "bloat", "block", "bloke",
  "blond", "blood", "bloom", "blown", "bluer", "bluff", "blunt", "blurb", "blurt", "blush",
  "board", "boast", "bobby", "boney", "bongo", "bonus", "booby", "boost", "booth", "booty",
  "booze", "boozy", "borax", "borne", "bosom", "bossy", "botch", "bough", "boule", "bound",
  "bowel", "boxer", "brace", "braid", "brain", "brake", "brand", "brash", "brass", "brave",
  "bravo", "brawl", "brawn", "bread", "break", "breed", "briar", "bribe", "brick", "bride",
  "brief", "brine", "bring", "brink", "briny", "brisk", "broad", "broil", "broke", "brood",
  "brook", "broom", "broth", "brown", "brunt", "brush", "brute", "buddy", "budge", "buggy",
  "bugle", "build", "built", "bulge", "bulky", "bully", "bunch", "bunny", "burly", "burnt",
  "burst", "bused", "bushy", "butch", "butte", "buxom", "buyer", "bylaw", "cabal", "cabby",
  "cabin", "cable", "cacao", "cache", "cacti", "caddy", "cadet", "cagey", "cairn", "camel",
  "cameo", "canal", "candy", "canny", "canoe", "canon", "caper", "caput", "carat", "cargo",
  "carol", "carry", "carve", "caste", "catch", "cater", "catty", "caulk", "cause", "cavil",
  "cease", "cedar", "cello", "chafe", "chaff", "chain", "chair", "chalk", "champ", "chant",
  "chaos", "chard", "charm", "chart", "chase", "chasm", "cheap", "cheat", "check", "cheek",
  "cheer", "chess", "chest", "chick", "chide", "chief", "child", "chili", "chill", "chime",
  "china", "chipm", "chirp", "chock", "choir", "choke", "chord", "chore", "chose", "chuck",
  "chump", "chunk", "churn", "chute", "cider", "cigar", "cinch", "circa", "civic", "civil",
  "clack", "claim", "clamp", "clang", "clank", "clash", "clasp", "class", "clean", "clear",
  "cleat", "cleft", "clerk", "click", "cliff", "climb", "cling", "clink", "cloak", "clock",
  "clone", "close", "cloth", "cloud", "clout", "clove", "clown", "cluck", "clued", "clump",
  "clung", "coach", "coast", "cobra", "cocoa", "colon", "color", "comet", "comfy", "comic",
  "comma", "conch", "condo", "conic", "copse", "coral", "corer", "corny", "couch", "cough",
  "could", "count", "coupe", "court", "coven", "cover", "covet", "covey", "cower", "coyly",
  "crack", "craft", "cramp", "crane", "crank", "crash", "crass", "crate", "crave", "crawl",
  "craze", "crazy", "creak", "cream", "credo", "creed", "creek", "creep", "creme", "crepe",
  "crept", "cress", "crest", "crick", "cried", "crier", "crime", "crimp", "crisp", "croak",
  "crock", "crone", "crony", "crook", "cross", "croup", "crowd", "crown", "crude", "cruel",
  "crumb", "crump", "crush", "crust", "crypt", "cubic", "cumin", "curio", "curly", "curry",
  "curse", "curve", "curvy", "cutie", "cyber", "cycle", "cynic", "daddy", "daily", "dairy",
  "daisy", "dally", "dance", "dandy", "datum", "daunt", "dealt", "death", "debar", "debit",
  "debug", "debut", "decal", "decay", "decor", "decoy", "decry", "defer", "deign", "deity",
  "delay", "delta", "delve", "demon", "demur", "denim", "dense", "depot", "depth", "derby",
  "deter", "detox", "deuce", "devil", "diary", "dicey", "digit", "dilly", "dimly", "diner",
  "dingo", "dingy", "diode", "dirge", "dirty", "disco", "ditch", "ditto", "ditty", "diver",
  "dizzy", "dodge", "dodgy", "dogma", "doing", "dolly", "donor", "donut", "dopey", "doubt",
  "dough", "dowdy", "dowel", "downy", "dowry", "dozen", "draft", "drain", "drake", "drama",
  "drank", "drape", "drawl", "drawn", "dread", "dream", "dress", "dried", "drier", "drift",
  "drill", "drink", "drive", "droit", "droll", "drone", "drool", "droop", "dross", "drove",
  "drown", "druid", "drunk", "dryer", "dryly", "duchy", "dully", "dummy", "dumpy", "dunce",
  "dusky", "dusty", "dutch", "duvet", "dwarf", "dwell", "dwelt", "dying", "eager", "eagle",
  "early", "earth", "easel", "eaten", "eater", "ebony", "eclat", "edict", "edify", "eerie",
  "egret", "eight", "eject", "eking", "elate", "elbow", "elder", "elect", "elegy", "elfin",
  "elide", "elite", "elope", "elude", "email", "embed", "ember", "emcee", "empty", "enact",
  "endow", "enema", "enemy", "enjoy", "ennui", "ensue", "enter", "entry", "envoy", "epoch",
  "epoxy", "equal", "equip", "erase", "erect", "erode", "error", "erupt", "essay", "ester",
  "ether", "ethic", "ethos", "etude", "evade", "event", "every", "evict", "evoke", "exact",
  "exalt", "excel", "exert", "exile", "exist", "expel", "extol", "extra", "exult", "eying",
  "fable", "facet", "faint", "fairy", "faith", "false", "fancy", "fanny", "farce", "fatal",
  "fatty", "fault", "fauna", "favor", "feast", "fecal", "feign", "fella", "felon", "femme",
  "femur", "fence", "feral", "ferry", "fetal", "fetch", "fetid", "fetus", "fever", "fewer",
  "fiber", "fibre", "ficus", "field", "fiend", "fiery", "fifth", "fifty", "fight", "filer",
  "filet", "filly", "filmy", "filth", "final", "finch", "finer", "first", "fishy", "fixer",
  "fizzy", "fjord", "flack", "flail", "flair", "flake", "flaky", "flame", "flank", "flare",
  "flash", "flask", "fleck", "fleet", "flesh", "flick", "flier", "fling", "flint", "flirt",
  "float", "flock", "flood", "floor", "flora", "floss", "flour", "flout", "flown", "fluff",
  "fluid", "fluke", "flume", "flung", "flunk", "flush", "flute", "flyer", "foamy", "focal",
  "focus", "foggy", "foist", "folio", "folly", "foray", "force", "forge", "forgo", "forte",
  "forth", "forty", "forum", "found", "foyer", "frail", "frame", "frank", "fraud", "freak",
  "freed", "freer", "fresh", "friar", "fried", "frill", "frisk", "fritz", "frock", "frond",
  "front", "frost", "froth", "frown", "froze", "fruit", "fudge", "fugue", "fully", "fungi",
  "funky", "funny", "furor", "furry", "fussy", "fuzzy", "gaffe", "gaily", "gamer", "gamma",
  "gamut", "gassy", "gaudy", "gauge", "gaunt", "gauze", "gavel", "gawky", "gayer", "gayly",
  "gazer", "gecko", "geeky", "geese", "genie", "genre", "ghost", "ghoul", "giant", "giddy",
  "gipsy", "girly", "girth", "given", "giver", "glade", "gland", "glare", "glass", "glaze",
  "gleam", "glean", "glide", "glint", "gloat", "globe", "gloom", "glory", "gloss", "glove",
  "glyph", "gnash", "gnome", "godly", "going", "golem", "golly", "gonad", "goner", "goody",
  "gooey", "goofy", "goose", "gorge", "gouge", "gourd", "grace", "grade", "graft", "grail",
  "grain", "grand", "grant", "grape", "graph", "grasp", "grass", "grate", "grave", "gravy",
  "graze", "great", "greed", "green", "greet", "grief", "grill", "grime", "grimy", "grind",
  "gripe", "groan", "groin", "groom", "grope", "gross", "group", "grout", "grove", "growl",
  "grown", "gruel", "gruff", "grunt", "guard", "guava", "guess", "guest", "guide", "guild",
  "guile", "guilt", "guise", "gulch", "gully", "gumbo", "gummy", "guppy", "gusto", "gusty",
  "gypsy", "habit", "hairy", "halve", "handy", "happy", "hardy", "harem", "harpy", "harry",
  "harsh", "haste", "hasty", "hatch", "hater", "haunt", "haute", "haven", "havoc", "hazel",
  "heady", "heath", "heave", "heavy", "hedge", "hefty", "heist", "helix", "hello", "hence",
  "heron", "hilly", "hinge", "hippo", "hippy", "hitch", "hoard", "hobby", "hoist", "holly",
  "homer", "honey", "honor", "horde", "horny", "horse", "hotel", "hotly", "hound", "house",
  "hovel", "hover", "howdy", "human", "humid", "humor", "humph", "humus", "hunch", "hunky",
  "hurry", "husky", "hussy", "hutch", "hydro", "hyena", "hymen", "hyper", "icily", "icing",
  "ideal", "idiom", "idiot", "idler", "idyll", "igloo", "iliac", "image", "imbue", "impel",
  "imply", "inane", "inbox", "incur", "index", "inept", "inert", "infer", "ingot", "inlay",
  "inlet", "inner", "input", "inter", "intro", "ionic", "irate", "irony", "islet", "issue",
  "itchy", "ivory", "jaunt", "jazzy", "jelly", "jerky", "jetty", "jewel", "jiffy", "joint",
  "joist", "joker", "jolly", "joust", "judge", "juice", "juicy", "jumbo", "jumpy", "junta",
  "junto", "juror", "kappa", "karma", "kayak", "kebab", "khaki", "kinky", "kiosk", "kitty",
  "knack", "knave", "knead", "kneed", "kneel", "knelt", "knife", "knock", "knoll", "known",
  "koala", "krill", "label", "labor", "laden", "ladle", "lager", "lance", "lanky", "lapel",
  "lapse", "large", "larva", "lasso", "latch", "later", "lathe", "latte", "laugh", "layer",
  "leach", "leafy", "leaky", "leant", "leapt", "learn", "lease", "leash", "least", "leave",
  "ledge", "leech", "leery", "lefty", "legal", "leggy", "lemon", "lemur", "leper", "level",
  "lever", "libel", "liege", "light", "liken", "lilac", "limbo", "limit", "linen", "liner",
  "lingo", "lipid", "lithe", "liver", "livid", "llama", "loamy", "loath", "lobby", "local",
  "locus", "lodge", "lofty", "logic", "login", "loopy", "loose", "lorry", "loser", "louse",
  "lousy", "lover", "lower", "lowly", "loyal", "lucid", "lucky", "lumen", "lumpy", "lunar",
  "lunch", "lunge", "lupus", "lurch", "lurid", "lusty", "lying", "lymph", "lynch", "lyric",
  "macaw", "macho", "macro", "madam", "madly", "mafia", "magic", "magma", "maize", "major",
  "maker", "mambo", "mamma", "mammy", "manga", "mange", "mango", "mangy", "mania", "manic",
  "manly", "manor", "maple", "march", "marry", "marsh", "mason", "masse", "match", "matey",
  "mauve", "maxim", "maybe", "mayor", "mealy", "meant", "meaty", "mecca", "medal", "media",
  "medic", "melee", "melon", "mercy", "merge", "merit", "merry", "metal", "meter", "metro",
  "micro", "midge", "midst", "might", "milky", "mimic", "mince", "miner", "minim", "minor",
  "minty", "minus", "mirth", "miser", "missy", "mocha", "modal", "model", "modem", "mogul",
  "moist", "molar", "moldy", "money", "month", "moody", "moose", "moral", "moron", "morph",
  "mossy", "motel", "motif", "motor", "motto", "moult", "mound", "mount", "mourn", "mouse",
  "mouth", "mover", "movie", "mower", "mucky", "mucus", "muddy", "mulch", "mummy", "munch",
  "mural", "murky", "mushy", "music", "musky", "musty", "myrrh", "nadir", "naive", "nanny",
  "nasal", "nasty", "natal", "naval", "navel", "needy", "neigh", "nerdy", "nerve", "never",
  "newer", "newly", "nicer", "niche", "niece", "night", "ninja", "ninny", "ninth", "noble",
  "nobly", "noise", "noisy", "nomad", "noose", "north", "nosey", "notch", "novel", "nudge",
  "nurse", "nutty", "nylon", "nymph", "oaken", "obese", "occur", "ocean", "octal", "octet",
  "odder", "oddly", "offal", "offer", "often", "olden", "older", "olive", "ombre", "omega",
  "onion", "onset", "opera", "opine", "opium", "optic", "orbit", "order", "organ", "other",
  "otter", "ought", "ounce", "outdo", "outer", "outgo", "ovary", "ovate", "overt", "ovine",
  "ovoid", "owing", "owner", "oxide", "ozone", "paddy", "pagan", "paint", "paler", "palsy",
  "panel", "panic", "pansy", "papal", "paper", "parer", "parka", "parry", "parse", "party",
  "pasta", "paste", "pasty", "patch", "patio", "patsy", "patty", "pause", "payee", "payer",
  "peace", "peach", "pearl", "pecan", "pedal", "penal", "pence", "penne", "penny", "perch",
  "peril", "perky", "pesky", "pesto", "petal", "petty", "phase", "phone", "phony", "photo",
  "piano", "picky", "piece", "piety", "piggy", "pilot", "pinch", "piney", "pinky", "pinto",
  "piper", "pique", "pitch", "pithy", "pivot", "pixel", "pixie", "pizza", "place", "plaid",
  "plain", "plait", "plane", "plank", "plant", "plate", "plaza", "plead", "pleat", "plied",
  "plier", "pluck", "plumb", "plume", "plump", "plunk", "plush", "poesy", "point", "poise",
  "poker", "polar", "polka", "polyp", "pooch", "poppy", "porch", "poser", "posit", "posse",
  "pouch", "pound", "pouty", "power", "prank", "prawn", "preen", "press", "price", "prick",
  "pride", "pried", "prime", "primo", "print", "prior", "prism", "privy", "prize", "probe",
  "prone", "prong", "proof", "prose", "proud", "prove", "prowl", "proxy", "prude", "prune",
  "psalm", "pubic", "pudgy", "puffy", "pulpy", "pulse", "punch", "pupal", "pupil", "puppy",
  "puree", "purer", "purge", "purse", "pushy", "putty", "pygmy", "quack", "quail", "quake",
  "qualm", "quark", "quart", "quash", "quasi", "queen", "queer", "quell", "query", "quest",
  "queue", "quick", "quiet", "quill", "quilt", "quirk", "quite", "quota", "quote", "quoth",
  "rabbi", "rabid", "racer", "radar", "radii", "radio", "rainy", "raise", "rajah", "rally",
  "ralph", "ramen", "ranch", "randy", "range", "rapid", "rarer", "raspy", "ratio", "ratty",
  "raven", "rayon", "razor", "reach", "react", "ready", "realm", "rearm", "rebar", "rebel",
  "rebus", "rebut", "recap", "recur", "recut", "reedy", "refer", "refit", "regal", "rehab",
  "reign", "relax", "relay", "relic", "remit", "renal", "renew", "repay", "repel", "reply",
  "rerun", "reset", "resin", "retch", "retro", "retry", "reuse", "revel", "revue", "rhino",
  "rhyme", "rider", "ridge", "rifle", "right", "rigid", "rigor", "rinse", "ripen", "riper",
  "risen", "riser", "risky", "rival", "river", "rivet", "roach", "roast", "robin", "robot",
  "rocky", "rodeo", "roger", "rogue", "roomy", "roost", "rotor", "rouge", "rough", "round",
  "rouse", "route", "rover", "rowdy", "rower", "royal", "ruddy", "ruder", "rugby", "ruler",
  "rumba", "rumor", "rupee", "rural", "rusty", "sadly", "safer", "saint", "salad", "sally",
  "salon", "salsa", "salty", "salve", "salvo", "sandy", "saner", "sappy", "sassy", "satin",
  "satyr", "sauce", "saucy", "sauna", "saute", "savor", "savvy", "scald", "scale", "scalp",
  "scaly", "scamp", "scant", "scare", "scarf", "scary", "scene", "scent", "scion", "scoff",
  "scold", "scone", "scoop", "scope", "score", "scorn", "scour", "scout", "scowl", "scram",
  "scrap", "scree", "screw", "scrub", "scrum", "scuba", "sedan", "seedy", "segue", "seize",
  "semen", "sense", "sepia", "serif", "serum", "serve", "setup", "seven", "sever", "sewer",
  "shack", "shade", "shady", "shaft", "shake", "shaky", "shale", "shall", "shalt", "shame",
  "shank", "shape", "shard", "share", "shark", "sharp", "shave", "shawl", "shear", "sheen",
  "sheep", "sheer", "sheet", "sheik", "shelf", "shell", "shied", "shift", "shine", "shiny",
  "shire", "shirk", "shirt", "shoal", "shock", "shone", "shook", "shoot", "shore", "shorn",
  "short", "shout", "shove", "shown", "showy", "shrew", "shrub", "shrug", "shuck", "shunt",
  "shush", "shyly", "siege", "sieve", "sight", "sigma", "silky", "silly", "since", "sinew",
  "singe", "siren", "sissy", "sixth", "sixty", "skate", "skier", "skiff", "skill", "skimp",
  "skirt", "skulk", "skull", "skunk", "slack", "slain", "slang", "slant", "slash", "slate",
  "slave", "sleek", "sleep", "sleet", "slept", "slice", "slick", "slide", "slime", "slimy",
  "sling", "slink", "sloop", "slope", "slosh", "sloth", "slump", "slung", "slunk", "slurp",
  "slush", "slyly", "smack", "small", "smart", "smash", "smear", "smell", "smelt", "smile",
  "smirk", "smite", "smith", "smock", "smoke", "smoky", "smote", "snack", "snail", "snake",
  "snaky", "snare", "snarl", "sneak", "sneer", "snide", "sniff", "snipe", "snoop", "snore",
  "snort", "snout", "snowy", "snuck", "snuff", "soapy", "sober", "soggy", "solar", "solid",
  "solve", "sonar", "sonic", "sooth", "sooty", "sorry", "sound", "south", "sower", "space",
  "spade", "spank", "spare", "spark", "spasm", "spawn", "speak", "spear", "speck", "speed",
  "spell", "spelt", "spend", "spent", "sperm", "spice", "spicy", "spied", "spiel", "spike",
  "spiky", "spill", "spilt", "spine", "spiny", "spire", "spite", "splat", "split", "spoil",
  "spoke", "spoof", "spook", "spool", "spoon", "spore", "sport", "spout", "spray", "spree",
  "sprig", "spunk", "spurn", "spurt", "squad", "squat", "squib", "stack", "staff", "stage",
  "staid", "stain", "stair", "stake", "stale", "stalk", "stall", "stamp", "stand", "stank",
  "stare", "stark", "start", "stash", "state", "stave", "stead", "steak", "steal", "steam",
  "steed", "steel", "steep", "steer", "stein", "stern", "stick", "stiff", "still", "stilt",
  "sting", "stink", "stint", "stock", "stoic", "stoke", "stole", "stomp", "stone", "stony",
  "stood", "stool", "stoop", "store", "stork", "storm", "story", "stout", "stove", "strap",
  "straw", "stray", "strip", "strut", "stuck", "study", "stuff", "stump", "stung", "stunk",
  "stunt", "style", "suave", "sugar", "suing", "suite", "sulky", "sully", "sumac", "sunny",
  "super", "surer", "surge", "surly", "sushi", "swami", "swamp", "swarm", "swash", "swath",
  "swear", "sweat", "sweep", "sweet", "swell", "swept", "swift", "swill", "swine", "swing",
  "swirl", "swish", "swoon", "swoop", "sword", "swore", "sworn", "swung", "synod", "syrup",
  "tabby", "table", "taboo", "tacit", "tacky", "taffy", "taint", "taken", "taker", "tally",
  "talon", "tamer", "tango", "tangy", "taper", "tapir", "tardy", "tarot", "taste", "tasty",
  "tatty", "taunt", "tawny", "teach", "teary", "tease", "teddy", "teeth", "tempo", "tenet",
  "tenor", "tense", "tenth", "tepee", "tepid", "terra", "terse", "testy", "thank", "theft",
  "their", "theme", "there", "these", "theta", "thick", "thief", "thigh", "thing", "think",
  "third", "thong", "thorn", "those", "three", "threw", "throb", "throw", "thrum", "thumb",
  "thump", "thyme", "tiara", "tibia", "tidal", "tiger", "tight", "tilde", "timer", "timid",
  "tipsy", "titan", "tithe", "title", "toast", "today", "toddy", "token", "tonal", "tonga",
  "tonic", "tooth", "topaz", "topic", "torch", "torso", "torus", "total", "totem", "touch",
  "tough", "towel", "tower", "toxic", "toxin", "trace", "track", "tract", "trade", "trail",
  "train", "trait", "tramp", "trash", "trawl", "tread", "treat", "trend", "triad", "trial",
  "tribe", "trice", "trick", "tried", "tripe", "trite", "troll", "troop", "trope", "trout",
  "trove", "truce", "truck", "truer", "truly", "trump", "trunk", "truss", "trust", "truth",
  "tryst", "tubal", "tuber", "tulip", "tulle", "tumor", "tunic", "turbo", "tutor", "twang",
  "tweak", "tweed", "tweet", "twice", "twine", "twirl", "twist", "twixt", "tying", "udder",
  "ulcer", "ultra", "umbra", "uncle", "uncut", "under", "undid", "undue", "unfed", "unfit",
  "unify", "union", "unite", "unity", "unlit", "unmet", "unset", "untie", "until", "unwed",
  "unzip", "upper", "upset", "urban", "urine", "usage", "usher", "using", "usual", "usurp",
  "utile", "utter", "vague", "valet", "valid", "valor", "value", "valve", "vapid", "vapor",
  "vault", "vaunt", "vegan", "venom", "venue", "verge", "verse", "verso", "verve", "vicar",
  "video", "vigil", "vigor", "villa", "vinyl", "viola", "viper", "viral", "virus", "visit",
  "visor", "vista", "vital", "vivid", "vixen", "vocal", "vodka", "vogue", "voice", "voila",
  "vomit", "voter", "vouch", "vowel", "vying", "wacky", "wafer", "wager", "wagon", "waist",
  "waive", "waltz", "warty", "waste", "watch", "water", "waver", "waxen", "weary", "weave",
  "wedge", "weedy", "weigh", "weird", "welch", "welsh", "wench", "whack", "whale", "wharf",
  "wheat", "wheel", "whelp", "where", "which", "whiff", "while", "whine", "whiny", "whirl",
  "whisk", "white", "whole", "whoop", "whose", "widen", "wider", "widow", "width", "wield",
  "wight", "willy", "wimpy", "wince", "winch", "windy", "wiser", "wispy", "witch", "witty",
  "woken", "woman", "women", "woody", "wooer", "wooly", "woozy", "wordy", "world", "worry",
  "worse", "worst", "worth", "would", "wound", "woven", "wrack", "wrath", "wreak", "wreck",
  "wrest", "wring", "wrist", "write", "wrong", "wrote", "wrung", "wryly", "yacht", "yearn",
  "yeast", "yield", "young", "youth", "zebra", "zesty", "zonal"
];

var WORD = "";
var MEANING = "";
const MAX_ATTEMPTS = 6;
let currentGuess = "";
let currentRow = 0;
const keyStates = {}; // Track keyboard color states
let playedWords = []; // Track played words


const grid = document.getElementById("grid");
const keyboard = document.getElementById("keyboard");
const loadingBox = document.getElementById("loading");
const rows = [];
let hintButton = null;

playedWords = getArrayFromLocalStorage("wordleWords") || [];
getRandomWord();
init();


function init() {
  // 1️⃣ Pre-build grid
for (let r = 0; r < MAX_ATTEMPTS; r++) {
  const row = document.createElement("div");
  row.className = "d-flex";
  const boxes = [];
  for (let c = 0; c < 5; c++) {
    const box = document.createElement("div");
    box.className = "box";
    row.appendChild(box);
    boxes.push(box);
  }
  grid.appendChild(row);
  rows.push(boxes);
}

// 2️⃣ Add Restart + Theme Toggle
const restartRow = document.createElement("div");
restartRow.className = "w-50 d-flex justify-content-around mb-2";

const restartBtn = document.createElement("button");
restartBtn.className = "btn btn-outline-danger";
restartBtn.innerHTML = '<i class="fas fa-redo-alt"></i>';
restartBtn.onclick = () => location.reload();

// Theme toggle button
const themeToggle = document.createElement("button");
themeToggle.id = "theme-toggle";
themeToggle.className = "btn btn-outline-secondary";
themeToggle.textContent = "🌙"; // default

themeToggle.onclick = () => {
  const isDark = document.body.classList.contains("dark");
  const newTheme = isDark ? "light" : "dark";
  setTheme(newTheme);
};

hintButton = document.createElement("button");
hintButton.className = "btn";
hintButton.innerHTML = '<i class="fas fa-question"></i>';
hintButton.onclick = () => {
  if(currentRow == 5){
    if (MEANING) {
    alert(`Definition of word is: ${MEANING}`);
  } else {
    alert("Hint not available.");
  }
  }
  else{
    alert("You can only get a hint on your last attempt.");
  }
};

restartRow.appendChild(restartBtn);
restartRow.appendChild(hintButton);
restartRow.appendChild(themeToggle);
keyboard.appendChild(restartRow);

setupTheme(); // Init theme toggle

// 3️⃣ Keyboard layout
const layout = [
  ["Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"],
  ["A", "S", "D", "F", "G", "H", "J", "K", "L"],
  [ "⌫","Z", "X", "C", "V", "B", "N", "M", "↵"]
];


// 4️⃣ Render virtual keyboard
layout.forEach(row => {
  const rowDiv = document.createElement("div");
  rowDiv.className = "d-flex justify-content-center mb-2";

  row.forEach(key => {
    const button = document.createElement("button");
    button.textContent = key;
    button.className = "btn-rounded btn-secondary m-1";
    button.style.width = "30px";
    button.style.height = "40px";
    button.onclick = () => handleKey(key);
    button.id = `key-${key}`;
    rowDiv.appendChild(button);
  });

  keyboard.appendChild(rowDiv);
});
}

// Save array to localStorage
function saveArrayToLocalStorage(key, array) {
  if (Array.isArray(array)) {
    localStorage.setItem(key, JSON.stringify(array));
  } else {
    console.error("Provided value is not an array.");
  }
}

// Fetch array from localStorage
function getArrayFromLocalStorage(key) {
  const stored = localStorage.getItem(key);
  try {
    const parsed = JSON.parse(stored);
    return Array.isArray(parsed) ? parsed : [];
  } catch (e) {
    console.error("Failed to parse stored data:", e);
    return [];
  }
}


function checkAttemptsForInfo(){
 if(currentRow == 5){
  hintButton.classList.add("btn-outline-info");
 }
}

async function isRealWord(word) {
  const res = await fetch(`https://api.dictionaryapi.dev/api/v2/entries/en/${word}`);
  const data = await res.json();
  return data.title === "No Definitions Found" ? false : true;
}

async function getMeaning(word) {
  fetch('https://api.dictionaryapi.dev/api/v2/entries/en/'+word)
  .then(response => response.json())
  .then(data => {
    MEANING = data[0].meanings[0].definitions[0].definition;
})
}

// 5️⃣ Handle keyboard input
async function handleKey(key) {
  if (currentRow >= MAX_ATTEMPTS) return;

  if (key === "↵" || key === "Enter") {
    if (currentGuess.length === 5) {
      if(!await isRealWord(currentGuess)){
        alert("The word is invalid, please try again.");
      }
      else{
        submitGuess();
      }
    }
    return;
  }

  if (key === "Back" || key === "⌫") {
    if (currentGuess.length > 0) {
      currentGuess = currentGuess.slice(0, -1);
      updateRow();
    }
    return;
  }

  if (/^[A-Z]$/.test(key) && currentGuess.length < 5) {
    currentGuess += key;
    updateRow();
  }
}

// 6️⃣ Update row boxes
function updateRow() {
  const boxes = rows[currentRow];
  for (let i = 0; i < 5; i++) {
    boxes[i].textContent = currentGuess[i] || "";
  }
}

// 7️⃣ Submit guess
function submitGuess() {
  const guess = currentGuess.toUpperCase();
  const target = WORD.split("");
  const guessLetters = guess.split("");
  const colors = Array(5).fill("red");
  const usedLetters = new Set(); // Track guessed letters

  // First pass: Green (correct position)
  for (let i = 0; i < 5; i++) {
    if (guess[i] === WORD[i]) {
      colors[i] = "green";
      target[i] = null;
      guessLetters[i] = null;
    }
  }

  // Second pass: Yellow (wrong position)
  for (let i = 0; i < 5; i++) {
    if (guessLetters[i] && target.includes(guessLetters[i])) {
      colors[i] = "yellow";
      target[target.indexOf(guessLetters[i])] = null;
      guessLetters[i] = null;
    }
  }

  // Update matrix colors
  for (let i = 0; i < 5; i++) {
    const box = rows[currentRow][i];
    box.classList.add(colors[i]);
  }

  // ✅ Update all guessed letters on the keyboard
  for (let i = 0; i < 5; i++) {
    const letter = guess[i];
    usedLetters.add(letter);
    updateKeyColor(letter, colors[i]);
  }

  // ✅ Also ensure red keys for letters guessed but not in the word at all
  usedLetters.forEach(letter => {
    if (!WORD.includes(letter)) {
      updateKeyColor(letter, "red");
    }
  });

  // Win/Loss check
  let confirmed = false;
  if (guess === WORD) {
    confirmed = confirm(`🎉 Congrats! \nThe word was ${WORD}. \n\nMeaning: ${MEANING}\n\n\nPlay again?`);
    playedWords.push(WORD);
    if (confirmed) window.location.reload();
  } else if (currentRow === MAX_ATTEMPTS - 1) {
    confirmed = confirm(`😢 Out of tries! \nThe Word was ${WORD}. \n\nMeaning ${MEANING}.\n\n\nPlay again?`);
    if (confirmed) window.location.reload();
  }

  currentRow++;
  currentGuess = "";

  checkAttemptsForInfo();
}

// 8️⃣ Update key color (preserve best state)
function updateKeyColor(letter, color) {
  const btn = document.getElementById(`key-${letter}`);
  if (!btn) return;

  const current = keyStates[letter];
  if (current === "green") return;
  if (current === "yellow" && color === "red") return;

  keyStates[letter] = color;
  btn.classList.remove("btn-secondary", "green", "yellow", "red");
  btn.classList.add(color);
}

// 9️⃣ Real keyboard input
document.addEventListener("keydown", (e) => {
  const key = e.key.toUpperCase();
  if (key === "BACKSPACE") handleKey("Back");
  else if (key === "ENTER") handleKey("Enter");
  else if (/^[A-Z]$/.test(key) && key.length === 1) handleKey(key);
});

// 🔟 Fetch word from API

function getRandomWord() {
  let w = wordleWords[Math.floor(Math.random() * wordleWords.length)].toUpperCase();
  while (playedWords.includes(w)) {
    w = wordleWords[Math.floor(Math.random() * wordleWords.length)].toUpperCase();
  }
  WORD = w;
  getMeaning(WORD);
}

// 🌙 Theme Handling
function setupTheme() {
  const stored = localStorage.getItem("theme");
  const defaultTheme = stored || "light";
  setTheme(defaultTheme);
}

function setTheme(theme) {
  if (theme === "dark") {
    document.body.classList.add("dark");
    document.getElementById("theme-toggle").textContent = "☀️";
  } else {
    document.body.classList.remove("dark");
    document.getElementById("theme-toggle").textContent = "🌙";
  }
  localStorage.setItem("theme", theme);
}


// Save data when page is closing
window.addEventListener("beforeunload", () => {
  saveArrayToLocalStorage("wordleWords", playedWords);
});

// Save data when tab is changed
window.addEventListener("visibilitychange", () => {
  saveArrayToLocalStorage("wordleWords", playedWords);
});