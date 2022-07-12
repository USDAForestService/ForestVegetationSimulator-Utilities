##############################################################
#FVS_Species_Functions.R
#
#Remember this option for future vector updating!
#options(width = 40)
#FIA
##############################################################

MIN = 1
MAX = 460

#########################################################
#Vector of recognized FIA species codes less than 1000.
#Vector is arranged in rows of 8 entries.
#########################################################

FIA=c(
"10", "11", "12", "14", "15", "16", "17", "18",
  "19", "20", "21", "22", "40", "41", "42", "43",
  "50", "51", "52", "53", "54", "55", "56", "57",
  "58", "59", "60", "61", "62", "63", "64", "65",
  "66", "67", "68", "69", "70", "71", "72", "73",
  "74", "75", "81", "90", "91", "92", "93", "94",
  "95", "96", "97", "98", "100", "101", "102", "103",
  "104", "105", "106", "107", "108", "109", "110", "111",
  "112", "113", "114", "115", "116", "117", "118", "119",
  "120", "121", "122", "123", "124", "125", "126", "127",
  "128", "129", "130", "131", "132", "133", "134", "135",
  "136", "137", "138", "139", "140", "141", "142", "143",
  "144", "200", "201", "202", "211", "212", "220", "221",
  "222", "223", "230", "231", "232", "240", "241", "242",
  "250", "251", "252", "260", "261", "262", "263", "264",
  "299", "300", "303", "304", "310", "311", "312", "313",
  "314", "315", "316", "317", "318", "319", "320", "321",
  "322", "323", "330", "331", "332", "333", "334", "336",
  "337", "341", "345", "350", "351", "352", "353", "355",
  "356", "357", "358", "360", "361", "362", "363", "367",
  "370", "371", "372", "373", "374", "375", "376", "377",
  "378", "379", "381", "391", "400", "401", "402", "403",
  "404", "405", "406", "407", "408", "409", "410", "411",
  "412", "413", "420", "421", "422", "423", "424", "431",
  "450", "451", "452", "460", "461", "462", "463", "471",
  "475", "481", "490", "491", "492", "500", "501", "502",
  "503", "504", "505", "506", "507", "508", "509", "510",
  "511", "512", "513", "514", "520", "521", "522", "523",
  "531", "540", "541", "542", "543", "544", "545", "546",
  "547", "548", "549", "550", "551", "552", "555", "561",
  "571", "580", "581", "582", "583", "591", "600", "601",
  "602", "603", "604", "605", "606", "611", "621", "631",
  "641", "650", "651", "652", "653", "654", "655", "657",
  "658", "660", "661", "662", "663", "664", "680", "681",
  "682", "683", "684", "690", "691", "692", "693", "694",
  "701", "711", "712", "715", "718", "720", "721", "722",
  "729", "730", "731", "732", "740", "741", "742", "743",
  "744", "745", "746", "747", "748", "749", "752", "753",
  "755", "756", "757", "758", "760", "761", "762", "763",
  "764", "765", "766", "768", "769", "770", "771", "772",
  "773", "774", "800", "801", "802", "803", "804", "805",
  "806", "807", "808", "809", "810", "811", "812", "813",
  "814", "815", "816", "817", "818", "819", "820", "821",
  "822", "823", "824", "825", "826", "827", "828", "829",
  "830", "831", "832", "833", "834", "835", "836", "837",
  "838", "839", "840", "841", "842", "843", "844", "845",
  "846", "847", "851", "852", "853", "854", "855", "856",
  "857", "858", "859", "860", "863", "864", "865", "866",
  "867", "868", "869", "870", "873", "874", "876", "877",
  "882", "883", "884", "885", "886", "887", "888", "890",
  "891", "895", "896", "897", "901", "902", "906", "907",
  "908", "909", "911", "912", "913", "914", "915", "919",
  "920", "921", "922", "923", "924", "925", "926", "927",
  "928", "929", "931", "934", "935", "936", "937", "940",
  "950", "951", "952", "953", "970", "971", "972", "973",
  "974", "975", "976", "977", "981", "982", "986", "987",
  "988", "989", "990", "991", "992", "993", "994", "995",
  "996", "997", "998", "999")

#############################################################################
#Vector of FIA sequence numbers (1 - 460)
#############################################################################

SEQ<-seq(from = 1, to = MAX, by = 1)

#############################################################################
#Vector of USDA plant symbols. Vector is arranged in rows of 8 entries
#############################################################################

PLANT=c(
  "ABIES", "ABAM", "ABBA", "ABBR", "ABCO", "ABFR", "ABGR", "ABLAA",
  "ABLA", "ABMA", "ABSH", "ABPR", "CHAMA4", "CHLA", "CHNO", "CHTH2",
  "CUPRE", "CUAR", "CUBA", "CUFO2", "CUMA2", "CUSA3", "CUMA", "JUNIP",
  "JUPI", "JUCO11", "JUFL", "JUAS", "JUCA7", "JUDE2", "JUOC", "JUOS",
  "JUSC2", "JUVIS", "JUVI", "JUMO", "LARIX", "LALA", "LALY", "LAOC",
  "LAKA2", "LASI3", "CADE27", "PICEA", "PIAB", "PIBR", "PIEN", "PIGL",
  "PIMA", "PIPU",  "PIRU", "PISI", "PINUS", "PIAL", "PIAR", "PIAT",
  "PIBA", "PIBA2", "PIED", "PICL",  "PICO", "PICO3", "PIEC2", "PIEL",
  "PIEN2", "PIFL2", "PIST3", "PIGL2", "PIJE", "PILA", "PILE", "PIMO3",
  "PIMU", "PIPA2", "PIPO", "PIPU5", "PIRA2", "PIRE", "PIRI", "PISA2",
  "PISE", "PIST", "PISY", "PITA", "PIVI2", "PIMO", "PIDI3", "PIAR5",
  "PINI", "PIWA", "PIQU", "PITO", "PICE", "PIRE5", "PILO", "PIMOF",
  "PIELE2", "PSEUD7", "PSMA", "PSME", "SESE3", "SEGI2", "TAXOD", "TADI2",
  "TAAS", "TAMU", "TAXUS", "TABR2", "TAFL", "THUJA", "THOC2", "THPL",
  "TORRE", "TOCA", "TOTA", "TSUGA", "TSCA", "TSCA2", "TSHE", "TSME",
  "2TE", "ACACI", "ACFA", "ACGR", "ACER", "ACBA3", "ACMA3", "ACNE2",
  "ACNI5", "ACPE", "ACRU", "ACSA2", "ACSA3", "ACSP2", "ACPL", "ACGL",
  "ACGR3", "ACLE", "AESCU", "AEGL", "AEFL", "AECA", "AEGLA", "AEPA",
  "AESY", "AIAL", "ALJU", "ALNUS", "ALRU2", "ALRH2", "ALOB2", "ALGL2",
  "AMELA", "AMAR3", "AMSA", "ARBUT", "ARME", "ARAR2", "ARXA80", "ASTR",
  "BETUL", "BEAL2", "BELE", "BENI", "BEOC2", "BEPA", "BENE4", "BEUB",
  "BEUT", "BEPO", "SILAL3", "CACA18", "CARYA", "CAAQ2", "CACO15", "CAGL8",
  "CAIL2", "CALA21", "CAMY", "CAOV2", "CATE9", "CAAL27", "CAPA24", "CAFL6",
  "CAOV3", "CACA38", "CASTA", "CADE12", "CAPU9", "CAPUO", "CAMO83", "CHCHC4",
  "CATAL", "CABI8", "CASP8", "CELTI", "CELA", "CEOC", "CELAR", "CECA4",
  "CELE3", "CLKE", "CORNU", "COFL2", "CONU4", "CRATA", "CRCR2", "CRMO2",
  "CRBR3", "CRCA", "CRCH", "CRDI", "CRFL", "CRMO3", "CRPE", "EUCAL",
  "EUGL", "EUCA2", "EUGR12", "EURO2", "DIOSP", "DIVI5", "DITE3", "EHAN",
  "FAGR", "FRAXI", "FRAM2", "FRLA", "FRNI", "FRPE", "FRPR", "FRQU",
  "FRVE2", "FRCA3", "FRTE", "GLEDI", "GLAQ", "GLTR", "GOLA", "GIBI2",
  "GYDI", "HALES", "HACA3", "HADI3", "HAPA2", "ILOP", "JUGLA", "JUCI",
  "JUNI", "JUHI", "JUCA", "JUMI", "JUMA", "LIST2", "LITU", "LIDE3",
  "MAPO", "MAGNO", "MAAC", "MAGR4", "MAVI2", "MAMA2", "MAFR", "MAPY",
  "MATR", "MALUS", "MAFU", "MAAN3", "MACO5", "MAIO", "MORUS", "MOAL",
  "MORU2", "MOMI", "MONI", "NYSSA", "NYAQ2", "NYOG", "NYSY", "NYBI",
  "OSVI", "OXAR", "PATO2", "MAPA28", "OSTR", "PERSE", "PEBO", "PLAQ",
  "PLATA", "PLRA", "PLOC", "PLWR2", "POPUL", "POBA2", "PODE3", "POGR4",
  "POHE4", "PODEM", "POTR5", "POBAT", "POFR2", "POAN3", "POAL7", "PONI",
  "PROSO", "PRGL2", "PRVE", "PRPU", "PRUNU", "PRPE2", "PRSE2", "PRVI",
  "PRPE3", "PRNI", "PRAM", "PREM", "PRAL5", "PRAN3", "PRAV", "PRCE",
  "PRDO", "PRMA", "QUERC", "QUAG", "QUAL", "QUAR", "QUBI", "QUCH2",
  "QUCO2", "QUDO", "QUSIS", "QUEL", "QUEM", "QUEN", "QUFA", "QUPA5",
  "QUGA", "QUGA4", "QUIL", "QUIM", "QUKE", "QULA2", "QULA3", "QULO",
  "QULY", "QUMA2", "QUMA3", "QUMI", "QUMU", "QUNI", "QUTE", "QUOB",
  "QUPA2", "QUPH", "QUPR2", "QURU", "QUSH", "QUST", "QUSI2", "QUVE",
  "QUVI", "QUWI2", "QUMA6", "QUMI2", "QUIN", "QUHY", "QUOG", "QUPR",
  "QUGR3", "QURU4", "QUGR", "AMEL", "ANGL4", "BUSI", "CASUA", "CAGL11",
  "CALE28", "CICA", "CIFR", "CITRU2", "CODI8", "COEL2", "COSE2", "CUAN4",
  "COHO", "EBEB", "LEPU3", "SOAF", "EURH", "EXPA", "FIAU", "FICI",
  "GUDI", "HIMA2", "LYLA3", "MAIN3", "METO3", "PIPI3", "SCAC2", "SIFO",
  "SISA6", "SIGL3", "SYCU", "TAIN2", "ROPS", "RONE", "ACWR4", "COAR",
  "CONU", "ROYST", "SAME8", "SAPA", "THMO4", "THRA2", "ARECA", "SASAD",
  "SALIX", "SAAM2", "SANI", "SABE2", "SABO", "SACA5", "SAPY", "SAAL2",
  "SASC", "SASE10", "SAAL5", "SORBU", "SOAM3", "SOAU", "SODE3", "SWMA2",
  "TILIA", "TIAM", "TIAMH", "TIAMC", "ULMUS", "ULAL", "ULAM", "ULCR",
  "ULPU", "ULRU", "ULSE", "ULTH", "UMCA", "YUBR", "AVGE", "COER2",
  "LARA2","RHMA2", "OLTE", "TAMAR2", "MEQU", "MEAZ", "TRSE6", "VEFO",
  "COOB2", "ELAN", "2TB", "2TREE")

#############################################################################
#Vector of GENUS values.TheorderofthesevaluesmatchestheorderoftheFIAvector.
#############################################################################

GENUS=c(
  "ABIES",          "ABIES",          "ABIES",          "ABIES",
  "ABIES",          "ABIES",          "ABIES",          "ABIES",
  "ABIES",          "ABIES",          "ABIES",          "ABIES",
  "CHAMAECYPARIS",  "CHAMAECYPARIS",  "CHAMAECYPARIS",  "CHAMAECYPARIS",
  "CUPRESSUS",      "CUPRESSUS",      "CUPRESSUS",      "CUPRESSUS",
  "CUPRESSUS",      "CUPRESSUS",      "CUPRESSUS",      "JUNIPERUS",
  "JUNIPERUS",      "JUNIPERUS",      "JUNIPERUS",      "JUNIPERUS",
  "JUNIPERUS",      "JUNIPERUS",      "JUNIPERUS",      "JUNIPERUS",
  "JUNIPERUS",      "JUNIPERUS",      "JUNIPERUS",      "JUNIPERUS",
  "LARIX",          "LARIX",          "LARIX",          "LARIX",
  "LARIX",          "LARIX",          "CALOCEDRUS",     "PICEA",
  "PICEA",          "PICEA",          "PICEA",          "PICEA",
  "PICEA",          "PICEA",          "PICEA",          "PICEA",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PINUS",          "PINUS",          "PINUS",
  "PINUS",          "PSEUDOTSUGA",    "PSEUDOTSUGA",    "PSEUDOTSUGA",
  "SEQUOIA",        "SEQUOIADENDRON", "TAXODIUM",       "TAXODIUM",
  "TAXODIUM",       "TAXODIUM",       "TAXUS",          "TAXUS",
  "TAXUS",          "THUJA",          "THUJA",          "THUJA",
  "TORREYA",        "TORREYA",        "TORREYA",        "TSUGA",
  "TSUGA",          "TSUGA",          "TSUGA",          "TSUGA",
  "TREE",           "ACACIA",         "ACACIA",         "ACACIA",
  "ACER",           "ACER",           "ACER",           "ACER",
  "ACER",           "ACER",           "ACER",           "ACER",
  "ACER",           "ACER",           "ACER",           "ACER",
  "ACER",           "ACER",           "AESCULUS",       "AESCULUS",
  "AESCULUS",       "AESCULUS",       "AESCULUS",       "AESCULUS",
  "AESCULUS",       "AILANTHUS",      "ALBIZIA",        "ALNUS",
  "ALNUS",          "ALNUS",          "ALNUS",          "ALNUS",
  "AMELANCHIER",    "AMELANCHIER",    "AMELANCHIER",    "ARBUTUS",
  "ARBUTUS",        "ARBUTUS",        "ARBUTUS",        "ASIMINA",
  "BETULA",         "BETULA",         "BETULA",         "BETULA",
  "BETULA",         "BETULA",         "BETULA",         "BETULA",
  "BETULA",         "BETULA",         "SIDEROXYLON",    "CARPINUS",
  "CARYA",          "CARYA",          "CARYA",          "CARYA",
  "CARYA",          "CARYA",          "CARYA",          "CARYA",
  "CARYA",          "CARYA",          "CARYA",          "CARYA",
  "CARYA",          "CARYA",          "CASTANEA",       "CASTANEA",
  "CASTANEA",       "CASTANEA",       "CASTANEA",       "CHRYSOLEPIS",
  "CATALPA",        "CATALPA",        "CATALPA",        "CELTIS",
  "CELTIS",         "CELTIS",         "CELTIS",         "CERCIS",
  "CERCOCARPUS",    "CLADRASTIS",     "CORNUS",         "CORNUS",
  "CORNUS",         "CRATAEGUS",      "CRATAEGUS",      "CRATAEGUS",
  "CRATAEGUS",      "CRATAEGUS",      "CRATAEGUS",      "CRATAEGUS",
  "CRATAEGUS",      "CRATAEGUS",      "CRATAEGUS",      "EUCALYPTUS",
  "EUCALYPTUS",     "EUCALYPTUS",     "EUCALYPTUS",     "EUCALYPTUS",
  "DIOSPYROS",      "DIOSPYROS",      "DIOSPYROS",      "EHRETIA",
  "FAGUS",          "FRAXINUS",       "FRAXINUS",       "FRAXINUS",
  "FRAXINUS",       "FRAXINUS",       "FRAXINUS",       "FRAXINUS",
  "FRAXINUS",       "FRAXINUS",       "FRAXINUS",       "GLEDITSIA",
  "GLEDITSIA",      "GLEDITSIA",      "GORDONIA",       "GINKGO",
  "GYMNOCLADUS",    "HALESIA",        "HALESIA",        "HALESIA",
  "HALESIA",        "ILEX",           "JUGLANS",        "JUGLANS",
  "JUGLANS",        "JUGLANS",        "JUGLANS",        "JUGLANS",
  "JUGLANS",        "LIQUIDAMBAR",    "LIRIODENDRON",   "LITHOCARPUS",
  "MACLURA",        "MAGNOLIA",       "MAGNOLIA",       "MAGNOLIA",
  "MAGNOLIA",       "MAGNOLIA",       "MAGNOLIA",       "MAGNOLIA",
  "MAGNOLIA",       "MALUS",          "MALUS",          "MALUS",
  "MALUS",          "MALUS",          "MORUS",          "MORUS",
  "MORUS",          "MORUS",          "MORUS",          "NYSSA",
  "NYSSA",          "NYSSA",          "NYSSA",          "NYSSA",
  "OSTRYA",         "OXYDENDRUM",     "PAULOWNIA",      "MAYTENUS",
  "OSMOXYLON",      "PERSEA",         "PERSEA",         "PLANERA",
  "PLATANUS",       "PLATANUS",       "PLATANUS",       "PLATANUS",
  "POPULUS",        "POPULUS",        "POPULUS",        "POPULUS",
  "POPULUS",        "POPULUS",        "POPULUS",        "POPULUS",
  "POPULUS",        "POPULUS",        "POPULUS",        "POPULUS",
  "PROSOPIS",       "PROSOPIS",       "PROSOPIS",       "PROSOPIS",
  "PRUNUS",         "PRUNUS",         "PRUNUS",         "PRUNUS",
  "PRUNUS",         "PRUNUS",         "PRUNUS",         "PRUNUS",
  "PRUNUS",         "PRUNUS",         "PRUNUS",         "PRUNUS",
  "PRUNUS",         "PRUNUS",         "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "QUERCUS",
  "QUERCUS",        "QUERCUS",        "QUERCUS",        "AMYRIS",
  "ANNONA",         "BURSERA",        "CASUARINA",      "CASUARINA",
  "CASUARINA",      "CINNAMOMUM",     "CITHAREXYLUM",   "CITRUS",
  "COCCOLOBA",      "COLUBRINA",      "CORDIA",         "CUPANIOPSIS",
  "CONDALIA",       "EBENOPSIS",      "LEUCAENA",       "SOPHORA",
  "EUGENIA",        "EXOTHEA",        "FICUS",          "FICUS",
  "GUAPIRA",        "HIPPOMANE",      "LYSILOMA",       "MANGIFERA",
  "METOPIUM",       "PISCIDIA",       "SCHEFFLERA",     "SIDEROXYLON",
  "SIDEROXYLON",    "SIMAROUBA",      "SYZYGIUM",       "TAMARINDUS",
  "ROBINIA",        "ROBINIA",        "ACOELORRAPHE",   "COCCOTHRINAX",
  "COCOS",          "ROYSTONEA",      "SABAL",          "SABAL",
  "THRINAX",        "THRINAX",        "FAMILYARECACEAE","SAPINDUS",
  "SALIX",          "SALIX",          "SALIX",          "SALIX",
  "SALIX",          "SALIX",          "SALIX",          "SALIX",
  "SALIX",          "SALIX",          "SASSAFRAS",      "SORBUS",
  "SORBUS",         "SORBUS",         "SORBUS",         "SWIETENIA",
  "TILIA",          "TILIA",          "TILIA",          "TILIA",
  "ULMUS",          "ULMUS",          "ULMUS",          "ULMUS",
  "ULMUS",          "ULMUS",          "ULMUS",          "ULMUS",
  "UMBELLULARIA",   "YUCCA",          "AVICENNIA",      "CONOCARPUS",
  "LAGUNCULARIA",   "RHIZOPHORA",     "OLNEYA",         "TAMARIX",
  "MELALEUCA",      "MELIA",          "TRIADICA",       "VERNICIA",
  "COTINUS",        "ELAEAGNUS",      "TREE",           "TREE")

##########################################################################################
#Vectorofscientifcnames.TheorderofthesevaluesmatchestheorderoftheFIAvector.
##########################################################################################

SCI=c("ABIES SPP.",                        "ABIES AMABILIS",
      "ABIES BALSAMEA",                    "ABIES BRACTEATA",
      "ABIES CONCOLOR",                    "ABIES FRASERI",
      "ABIES GRANDIS",                     "ABIES LASIOCARPA",
      "ABIES LASIOCARPA",                  "ABIES MAGNIFICA",
      "ABIES SHASTENSIS",                  "ABIES PROCERA",
      "CHAMAECYPARIS SPP.",                "CHAMAECYPARIS LAWSONIANA",
      "CHAMAECYPARIS NOOTKATENSIS",        "CHAMAECYPARIS THYOIDES",
      "CUPRESSUS SPP.",                    "CUPRESSUS ARIZONICA",
      "CUPRESSUS BAKERI",                  "CUPRESSUS FORBESII",
      "CUPRESSUS MACROCARPA",              "CUPRESSUS SARGENTII",
      "CUPRESSUS MACNABIANA",              "JUNIPERUS SPP.",
      "JUNIPERUS PINCHOTII",               "JUNIPERUS COAHUILENSIS",
      "JUNIPERUS FLACCIDA",                "JUNIPERUS ASHEI",
      "JUNIPERUS CALIFORNICA",             "JUNIPERUS DEPPEANA",
      "JUNIPERUS OCCIDENTALIS",            "JUNIPERUS OSTEOSPERMA",
      "JUNIPERUS SCOPULORUM",              "JUNIPERUS VIRGINIANA",
      "JUNIPERUS VIRGINIANA",              "JUNIPERUS MONOSPERMA",
      "LARIX SPP.",                        "LARIX LARICINA",
      "LARIX LYALLII",                     "LARIX OCCIDENTALIS",
      "LARIX KAEMPFERI",                   "LARIX SIBIRICA",
      "CALOCEDRUS DECURRENS",              "PICEA SPP.",
      "PICEA ABIES",                       "PICEA BREWERIANA",
      "PICEA ENGELMANNII",                 "PICEA GLAUCA",
      "PICEA MARIANA",                     "PICEA PUNGENS",
      "PICEA RUBENS",                      "PICEA SITCHENSIS",
      "PINUS SPP.",                        "PINUS ALBICAULIS",
      "PINUS ARISTATA",                    "PINUS ATTENUATA",
      "PINUS BALFOURIANA",                 "PINUS BANKSIANA",
      "PINUS EDULIS",                      "PINUS CLAUSA",
      "PINUS CONTORTA",                    "PINUS COULTERI",
      "PINUS ECHINATA",                    "PINUS ELLIOTTII",
      "PINUS ENGELMANNII",                 "PINUS FLEXILIS",
      "PINUS STROBIFORMIS",                "PINUS GLABRA",
      "PINUS JEFFREYI",                    "PINUS LAMBERTIANA",
      "PINUS LEIOPHYLLA",                  "PINUS MONTICOLA",
      "PINUS MURICATA",                    "PINUS PALUSTRIS",
      "PINUS PONDEROSA",                   "PINUS PUNGENS",
      "PINUS RADIATA",                     "PINUS RESINOSA",
      "PINUS RIGIDA",                      "PINUS SABINIANA",
      "PINUS SEROTINA",                    "PINUS STROBUS",
      "PINUS SYLVESTRIS",                  "PINUS TAEDA",
      "PINUS VIRGINIANA",                  "PINUS MONOPHYLLA",
      "PINUS DISCOLOR",                    "PINUS ARIZONICA",
      "PINUS NIGRA",                       "PINUS WASHOENSIS",
      "PINUS QUADRIFOLIA",                 "PINUS TORREYANA",
      "PINUS CEMBROIDES",                  "PINUS REMOTA",
      "PINUS LONGAEVA",                    "PINUS MONOPHYLLA",
      "PINUS ELLIOTTII",                   "PSEUDOTSUGA SPP.",
      "PSEUDOTSUGA MACROCARPA",            "PSEUDOTSUGA MENZIESII",
      "SEQUOIA SEMPERVIRENS",              "SEQUOIADENDRON GIGANTEUM",
      "TAXODIUM SPP.",                     "TAXODIUM DISTICHUM",
      "TAXODIUM ASCENDENS",                "TAXODIUM MUCRONATUM",
      "TAXUS SPP.",                        "TAXUS BREVIFOLIA",
      "TAXUS FLORIDANA",                   "THUJA SPP.",
      "THUJA OCCIDENTALIS",                "THUJA PLICATA",
      "TORREYA SPP.",                      "TORREYA CALIFORNICA",
      "TORREYA TAXIFOLIA",                 "TSUGA SPP.",
      "TSUGA CANADENSIS",                  "TSUGA CAROLINIANA",
      "TSUGA HETEROPHYLLA",                "TSUGA MERTENSIANA",
      "TREE EVERGREEN",                    "ACACIA SPP.",
      "ACACIA FARNESIANA",                 "ACACIA GREGGII",
      "ACER SPP.",                         "ACER BARBATUM",
      "ACER MACROPHYLLUM",                 "ACER NEGUNDO",
      "ACER NIGRUM",                       "ACER PENSYLVANICUM",
      "ACER RUBRUM",                       "ACER SACCHARINUM",
      "ACER SACCHARUM",                    "ACER SPICATUM",
      "ACER PLATANOIDES",                  "ACER GLABRUM",
      "ACER GRANDIDENTATUM",               "ACER LEUCODERME",
      "AESCULUS SPP.",                     "AESCULUS GLABRA",
      "AESCULUS FLAVA",                    "AESCULUS CALIFORNICA",
      "AESCULUS GLABRA",                   "AESCULUS PAVIA",
      "AESCULUS SYLVATICA",                "AILANTHUS ALTISSIMA",
      "ALBIZIA JULIBRISSIN",               "ALNUS SPP.",
      "ALNUS RUBRA",                       "ALNUS RHOMBIFOLIA",
      "ALNUS OBLONGIFOLIA",                "ALNUS GLUTINOSA",
      "AMELANCHIER SPP.",                  "AMELANCHIER ARBOREA",
      "AMELANCHIER SANGUINEA",             "ARBUTUS SPP.",
      "ARBUTUS MENZIESII",                 "ARBUTUS ARIZONICA",
      "ARBUTUS XALAPENSIS",                "ASIMINA TRILOBA",
      "BETULA SPP.",                       "BETULA ALLEGHANIENSIS",
      "BETULA LENTA",                      "BETULA NIGRA",
      "BETULA OCCIDENTALIS",               "BETULA PAPYRIFERA",
      "BETULA NEOALASKANA",                "BETULA UBER",
      "BETULA X UTAHENSIS",                "BETULA POPULIFOLIA",
      "SIDEROXYLON LANUGINOSUM",           "CARPINUS CAROLINIANA",
      "CARYA SPP.",                        "CARYA AQUATICA",
      "CARYA CORDIFORMIS",                 "CARYA GLABRA",
      "CARYA ILLINOINENSIS",               "CARYA LACINIOSA",
      "CARYA MYRISTICIFORMIS",             "CARYA OVATA",
      "CARYA TEXANA",                      "CARYA ALBA",
      "CARYA PALLIDA",                     "CARYA FLORIDANA",
      "CARYA OVALIS",                      "CARYA CAROLINAE-SEPTENTRIONALIS",
      "CASTANEA SPP.",                     "CASTANEA DENTATA",
      "CASTANEA PUMILA",                   "CASTANEA PUMILA",
      "CASTANEA MOLLISSIMA",               "CHRYSOLEPIS CHRYSOPHYLLA",
      "CATALPA SPP.",                      "CATALPA BIGNONIOIDES",
      "CATALPA SPECIOSA",                  "CELTIS SPP.",
      "CELTIS LAEVIGATA",                  "CELTIS OCCIDENTALIS",
      "CELTIS LAEVIGATA",                  "CERCIS CANADENSIS",
      "CERCOCARPUS LEDIFOLIUS",            "CLADRASTIS KENTUKEA",
      "CORNUS SPP.",                       "CORNUS FLORIDA",
      "CORNUS NUTTALLII",                  "CRATAEGUS SPP.",
      "CRATAEGUS CRUS-GALLI",              "CRATAEGUS MOLLIS",
      "CRATAEGUS BRAINERDII",              "CRATAEGUS CALPODENDRON",
      "CRATAEGUS CHRYSOCARPA",             "CRATAEGUS DILATATA",
      "CRATAEGUS FLABELLATA",              "CRATAEGUS MONOGYNA",
      "CRATAEGUS PEDICELLATA",             "EUCALYPTUS SPP.",
      "EUCALYPTUS GLOBULUS",               "EUCALYPTUS CAMALDULENSIS",
      "EUCALYPTUS GRANDIS",                "EUCALYPTUS ROBUSTA",
      "DIOSPYROS SPP.",                    "DIOSPYROS VIRGINIANA",
      "DIOSPYROS TEXANA",                  "EHRETIA ANACUA",
      "FAGUS GRANDIFOLIA",                 "FRAXINUS SPP.",
      "FRAXINUS AMERICANA",                "FRAXINUS LATIFOLIA",
      "FRAXINUS NIGRA",                    "FRAXINUS PENNSYLVANICA",
      "FRAXINUS PROFUNDA",                 "FRAXINUS QUADRANGULATA",
      "FRAXINUS VELUTINA",                 "FRAXINUS CAROLINIANA",
      "FRAXINUS TEXENSIS",                 "GLEDITSIA SPP.",
      "GLEDITSIA AQUATICA",                "GLEDITSIA TRIACANTHOS",
      "GORDONIA LASIANTHUS",               "GINKGO BILOBA",
      "GYMNOCLADUS DIOICUS",               "HALESIA SPP.",
      "HALESIA CAROLINA",                  "HALESIA DIPTERA",
      "HALESIA PARVIFLORA",                "ILEX OPACA",
      "JUGLANS SPP.",                      "JUGLANS CINEREA",
      "JUGLANS NIGRA",                     "JUGLANS HINDSII",
      "JUGLANS CALIFORNICA",               "JUGLANS MICROCARPA",
      "JUGLANS MAJOR",                     "LIQUIDAMBAR STYRACIFLUA",
      "LIRIODENDRON TULIPIFERA",           "LITHOCARPUS DENSIFLORUS",
      "MACLURA POMIFERA",                  "MAGNOLIA SPP.",
      "MAGNOLIA ACUMINATA",                "MAGNOLIA GRANDIFLORA",
      "MAGNOLIA VIRGINIANA",               "MAGNOLIA MACROPHYLLA",
      "MAGNOLIA FRASERI",                  "MAGNOLIA PYRAMIDATA",
      "MAGNOLIA TRIPETALA",                "MALUS SPP.",
      "MALUS FUSCA",                       "MALUS ANGUSTIFOLIA",
      "MALUS CORONARIA",                   "MALUS IOENSIS",
      "MORUS SPP.",                        "MORUS ALBA",
      "MORUS RUBRA",                       "MORUS MICROPHYLLA",
      "MORUS NIGRA",                       "NYSSA SPP.",
      "NYSSA AQUATICA",                    "NYSSA OGECHE",
      "NYSSA SYLVATICA",                   "NYSSA BIFLORA",
      "OSTRYA VIRGINIANA",                 "OXYDENDRUM ARBOREUM",
      "PAULOWNIA TOMENTOSA",               "MAYTENUS PALAUICA",
      "OSMOXYLON TRUNCATUM",               "PERSEA SPP.",
      "PERSEA BORBONIA",                   "PLANERA AQUATICA",
      "PLATANUS SPP.",                     "PLATANUS RACEMOSA",
      "PLATANUS OCCIDENTALIS",             "PLATANUS WRIGHTII",
      "POPULUS SPP.",                      "POPULUS BALSAMIFERA",
      "POPULUS DELTOIDES",                 "POPULUS GRANDIDENTATA",
      "POPULUS HETEROPHYLLA",              "POPULUS DELTOIDES",
      "POPULUS TREMULOIDES",               "POPULUS BALSAMIFERA",
      "POPULUS FREMONTII",                 "POPULUS ANGUSTIFOLIA",
      "POPULUS ALBA",                      "POPULUS NIGRA",
      "PROSOPIS SPP.",                     "PROSOPIS GLANDULOSA",
      "PROSOPIS VELUTINA",                 "PROSOPIS PUBESCENS",
      "PRUNUS SPP.",                       "PRUNUS PENSYLVANICA",
      "PRUNUS SEROTINA",                   "PRUNUS VIRGINIANA",
      "PRUNUS PERSICA",                    "PRUNUS NIGRA",
      "PRUNUS AMERICANA",                  "PRUNUS EMARGINATA",
      "PRUNUS ALLEGHANIENSIS",             "PRUNUS ANGUSTIFOLIA",
      "PRUNUS AVIUM",                      "PRUNUS CERASUS",
      "PRUNUS DOMESTICA",                  "PRUNUS MAHALEB",
      "QUERCUS SPP.",                      "QUERCUS AGRIFOLIA",
      "QUERCUS ALBA",                      "QUERCUS ARIZONICA",
      "QUERCUS BICOLOR",                   "QUERCUS CHRYSOLEPIS",
      "QUERCUS COCCINEA",                  "QUERCUS DOUGLASII",
      "QUERCUS SINUATA",                   "QUERCUS ELLIPSOIDALIS",
      "QUERCUS EMORYI",                    "QUERCUS ENGELMANNII",
      "QUERCUS FALCATA",                   "QUERCUS PAGODA",
      "QUERCUS GAMBELII",                  "QUERCUS GARRYANA",
      "QUERCUS ILICIFOLIA",                "QUERCUS IMBRICARIA",
      "QUERCUS KELLOGGII",                 "QUERCUS LAEVIS",
      "QUERCUS LAURIFOLIA",                "QUERCUS LOBATA",
      "QUERCUS LYRATA",                    "QUERCUS MACROCARPA",
      "QUERCUS MARILANDICA",               "QUERCUS MICHAUXII",
      "QUERCUS MUEHLENBERGII",             "QUERCUS NIGRA",
      "QUERCUS TEXANA",                    "QUERCUS OBLONGIFOLIA",
      "QUERCUS PALUSTRIS",                 "QUERCUS PHELLOS",
      "QUERCUS PRINUS",                    "QUERCUS RUBRA",
      "QUERCUS SHUMARDII",                 "QUERCUS STELLATA",
      "QUERCUS SIMILIS",                   "QUERCUS VELUTINA",
      "QUERCUS VIRGINIANA",                "QUERCUS WISLIZENI",
      "QUERCUS MARGARETTIAE",              "QUERCUS MINIMA",
      "QUERCUS INCANA",                    "QUERCUS HYPOLEUCOIDES",
      "QUERCUS OGLETHORPENSIS",            "QUERCUS PRINOIDES",
      "QUERCUS GRISEA",                    "QUERCUS RUGOSA",
      "QUERCUS GRACILIFORMIS",             "AMYRIS ELEMIFERA",
      "ANNONA GLABRA",                     "BURSERA SIMARUBA",
      "CASUARINA SPP.",                    "CASUARINA GLAUCA",
      "CASUARINA LEPIDOPHLOIA",            "CINNAMOMUM CAMPHORA",
      "CITHAREXYLUM FRUTICOSUM",           "CITRUS SPP.",
      "COCCOLOBA DIVERSIFOLIA",            "COLUBRINA ELLIPTICA",
      "CORDIA SEBESTENA",                  "CUPANIOPSIS ANACARDIOIDES",
      "CONDALIA HOOKERI",                  "EBENOPSIS EBANO",
      "LEUCAENA PULVERULENTA",             "SOPHORA AFFINIS",
      "EUGENIA RHOMBEA",                   "EXOTHEA PANICULATA",
      "FICUS AUREA",                       "FICUS CITRIFOLIA",
      "GUAPIRA DISCOLOR",                  "HIPPOMANE MANCINELLA",
      "LYSILOMA LATISILIQUUM",             "MANGIFERA INDICA",
      "METOPIUM TOXIFERUM",                "PISCIDIA PISCIPULA",
      "SCHEFFLERA ACTINOPHYLLA",           "SIDEROXYLON FOETIDISSIMUM",
      "SIDEROXYLON SALICIFOLIUM",          "SIMAROUBA GLAUCA",
      "SYZYGIUM CUMINI",                   "TAMARINDUS INDICA",
      "ROBINIA PSEUDOACACIA",              "ROBINIA NEOMEXICANA",
      "ACOELORRAPHE WRIGHTII",             "COCCOTHRINAX ARGENTATA",
      "COCOS NUCIFERA",                    "ROYSTONEA SPP.",
      "SABAL MEXICANA",                    "SABAL PALMETTO",
      "THRINAX MORRISII",                  "THRINAX RADIATA",
      "FAMILY ARECACEAE NOT LISTED ABOVE", "SAPINDUS SAPONARIA",
      "SALIX SPP.",                        "SALIX AMYGDALOIDES",
      "SALIX NIGRA",                       "SALIX BEBBIANA",
      "SALIX BONPLANDIANA",                "SALIX CAROLINIANA",
      "SALIX PYRIFOLIA",                   "SALIX ALBA",
      "SALIX SCOULERIANA",                 "SALIX X SEPULCRALIS",
      "SASSAFRAS ALBIDUM",                 "SORBUS SPP.",
      "SORBUS AMERICANA",                  "SORBUS AUCUPARIA",
      "SORBUS DECORA",                     "SWIETENIA MAHAGONI",
      "TILIA SPP.",                        "TILIA AMERICANA",
      "TILIA AMERICANA",                   "TILIA AMERICANA",
      "ULMUS SPP.",                        "ULMUS ALATA",
      "ULMUS AMERICANA",                   "ULMUS CRASSIFOLIA",
      "ULMUS PUMILA",                      "ULMUS RUBRA",
      "ULMUS SEROTINA",                    "ULMUS THOMASII",
      "UMBELLULARIA CALIFORNICA",          "YUCCA BREVIFOLIA",
      "AVICENNIA GERMINANS",               "CONOCARPUS ERECTUS",
      "LAGUNCULARIA RACEMOSA",             "RHIZOPHORA MANGLE",
      "OLNEYA TESOTA",                     "TAMARIX SPP.",
      "MELALEUCA QUINQUENERVIA",           "MELIA AZEDARACH",
      "TRIADICA SEBIFERA",                 "VERNICIA FORDII",
      "COTINUS OBOVATUS",                  "ELAEAGNUS ANGUSTIFOLIA",
      "TREE BROADLEAF",                    "TREE UNKNOWN")

#############################################################################
#Vector of hardwood or softwood indicator values
#############################################################################

HWSW<-c(0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,
         0,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1)

#############################################################################
#Start of  (sp) family functions
#
#The following functions comprise the sp-family of functions.
#- spGetData
#- spConvert
#- spGetHWSW
#- spGetGenus
#- spGetIndex
#
#Descriptions of each function are prvoided in the comment headers below.
#############################################################################

#############################################################################
#Function: spGetIndex
#
#This a function that is used to obtain an index value based on an incoming
#species code. The species code can either be a FIA code, USDA plant symbol,
#or Scientific name.
#
#Arguments:
#
#sp:   incoming species code. This can be either an FIA species code, USDA
#      plant symbol, or scientific name.
#from: Integer value signifying the type of species you are requestiong an
#      index for.
#      1 = FIA code
#      2 = USDA plant symbol
#      3 = Scientific name
#      If the value of from argument is anything other than 1 - 3, then a
#      NA value  will be returned from the function.
############################################################################

spGetIndex<- function(sp, from)
{

  if(!from %in% c(1, 2, 3))
  {
    spIndex = NA
    return(spIndex)
  }

  #Determine which species codes to search through based on from code
  #FIA code
  if(from == 1)
  {
    spIndex = match(sp, FIA)
  }

  #PLANT symbol
  else if (from == 2)
  {
    spIndex = match(toupper(sp), PLANT)
  }

  #Scientific name
  else
  {
    spIndex = match(toupper(sp), SCI)
  }

  #If spIndex returns as NA from search, then spTo is set to NA
  if(is.na(spIndex))
  {
    spIndex = NA
  }

  return(spIndex)
}

#############################################################################
#Function: spGetData
#
#This function returns a dataframe containing the following columns for all
#species less than or equal to FIA species code 999:
#
#- FIA species codes
#- USDA plant symbols
#- GENUS codes
#- Scientific names
#- Sequence number
#- Hardwood/Softwood Code
#
#This function does not have any arguments.
############################################################################

spGetData<-function()
{
  sp.data<-data.frame(FIA = FIA,
                      PLANT = PLANT,
                      SCI_NAME = SCI,
                      GENUS = GENUS,
                      SEQ = SEQ,
                      HW_SW = HWSW)
  return(sp.data)
}

#############################################################################
#Function: spConvert
#
#This function is used to convert between FIA species codes, USDA plant
#symbols, species scientific name, or sequence number (value from 1 - 460).
#
#Arguments:
#
#sp:   incoming species code. This can be either an FIA species code, USDA
#      plant symbol, or scientifc name
#from: Integer value signifying the type of species you are converting from.
#      1 = FIA code
#      2 = USDA plant symbol
#      3 = Scientific name
#      4 = FIA sequence number
#      If the value of from argument is anything other than 1 - 3, then a NA
#      value  will be returned from the function.
#to:   Integer value signifying the type of species you are converting to.
#      1 = FIA code
#      2 = USDA plant symbol
#      3 = Scientific name
#      4 = Sequence number
#      If the value of to argument  is anything other than 1 - 4, then a NA
#      value will be returned from the function.
#############################################################################

spConvert<-function(sp, from, to)
{
  #Test if from and to are anything other than 1 - 4
  #If values are anything other than 1 - 4, then code of NA is returned
  if(!from %in% c(1, 2, 3, 4) | !to %in% c(1, 2, 3, 4))
  {
    spTo = NA
    return(spTo)
  }

  #If you are using a sequence number, grab HW_SW code directly from sp and return
  if(from == 4)
  {
    #Cast sp to integer
    sp<-as.integer(sp)

    #If sp is NA or not between 1 and 460
    if(is.na(sp) | sp < MIN | sp > MAX)
    {
      code = NA
      return(spTo)
    }

    if(to == 1)
    {
      spTo = FIA[sp]
    }

    else if(to == 2)
    {
      spTo = PLANT[sp]
    }

    else if(to == 3)
    {
      spTo = SCI[sp]
    }

    else
    {
      spTo = sp
    }

    return(spTo)
  }

  #Determine which species codes to search through based on from code
  spIndex<-spGetIndex(sp, from)

  #If spIndex returns as NA from search, then spTo is set to NA
  if(is.na(spIndex))
  {
    spTo = NA
  }

  #Determine which species codes to pull value from based on to code
  else
  {
    if(to == 1)
    {
      spTo = FIA[spIndex]
    }

    else if(to == 2)
    {
      spTo = PLANT[spIndex]
    }

    else if(to == 3)
    {
      spTo = SCI[spIndex]
    }

    else
    {
      spTo = SEQ[spIndex]
    }
  }

  return(spTo)
}

#############################################################################
#Function: spHWSW
#
#This function converts a incoming species code to a hardwood (1) or
#softwood code categor. The incoming species can be a FIA code, USDA plant
#symbol, scientific name, or sequence number (value from 1 - 460).
#
#Argument list
#
#sp =  FIA species code (1 - 999). This value can be either a character or
#      integer.
#from: Integer value signifying the type of species you are converting from.
#      1 = FIA code
#      2 = USDA plant symbol
#      3 = Scientific name
#      4 = Sequence number
#      If the value of from argument is anything other than 1 - 4, then a NA
#      value  will be returned from the function.
#
#Note: using Sequence number in from argument will likely yield the fastest
#      result.
#############################################################################

spGetHWSW<-function(sp, from = 1)
{
  #Test if from is anything other than 1 - 4
  #If value is anything other than 1 - 4, then code of NA is returned
  if(!from %in% c(1, 2, 3, 4))
  {
    code = NA
    return(code)
  }

  #If you are using a sequence number, grab HW_SW code directly from sp and return
  if(from == 4)
  {
    #Cast sp to integer
    sp<-as.integer(sp)

    #If sp is NA or not between 1 and 460
    if(is.na(sp) | sp < MIN | sp > MAX)
    {
      code = NA
    }

    else
    {
      code = HWSW[sp]
    }

    return(code)
  }

  #If incoming species is a FIA code, then code is 0 if less than 300 and 1 if
  #greater than 300
  if(from == 1)
  {
    #Cast incoming FIA code to integer
    sp = suppressWarnings(as.integer(sp))

    #If coerced species code is NA, or not between 1 and 999 then sp is NA
    if(is.na(sp) | sp < 1 | sp > 999)
    {
      code = NA
    }

    #If incoming species is less than 300, it is a softwood
    else if(sp < 300)
    {
      code = 0
    }

    #Otherwise it is a hardwood
    else
    {
      code = 1
    }
  }

  #When from is 2 or 3, then species index will be obtained to pull directly
  #from HWSW array.
  else
  {
    #Determine which species codes to search through based on from code
    spIndex<-spGetIndex(sp, from)

    #If spIndex returns as NA from search, then spTo is set to NA
    if(is.na(spIndex))
    {
      code = NA
    }

    else
    {
      code = HWSW[spIndex]
    }
  }
  return(code)
}

#############################################################################
#Function: spGenus
#
#This function returns a GENUS based on a incoming FIA species code, USDA
#plant symbol, or scientific name.
#
##sp:  incoming species code. This can be either an FIA species code, USDA
#      plant symbol, or scientific name.
#from: Integer value signifying the type of species you are converting from.
#      1 = FIA code
#      2 = USDA plant symbol
#      3 = Scientific name
#      4 = FIA sequence number
#      If the value of from argument is anything other than 1 - 3, then a NA
#      value  will be returned from the function.
#      Note - Using FIA sequence number will yield the fastest results.
#
#Note: using Sequence number in from argument will likely yield the fastest
#      result.
############################################################################

spGetGenus<-function(sp, from, to)
{
  #Test if from is anything other than 1 - 4
  #If value is anything other than 1 - 4, then code of NA is returned
  if(!from %in% c(1, 2, 3, 4))
  {
    genus = NA
    return(genus)
  }

  #If you are using a sequence number, grab GENUS directly from sp and return
  if(from == 4)
  {
    #Cast sp to integer
    sp<-suppressWarnings(as.integer(sp))

    #If sp is NA or not between 1 and 460
    if(is.na(sp) | sp < MIN | sp > MAX)
    {
      genus = NA
    }

    else
    {
      genus = GENUS[sp]
    }

    return(genus)
  }

  #Determine which species codes to search through based on from code
  spIndex<-spGetIndex(sp, from)

  #If spIndex returns as NA from search, then spTo is set to NA
  if(is.na(spIndex))
  {
    genus = NA
  }

  #Determine which species codes to pull value from based on to code
  else
  {
    genus = GENUS[spIndex]
  }

  return(genus)
}

