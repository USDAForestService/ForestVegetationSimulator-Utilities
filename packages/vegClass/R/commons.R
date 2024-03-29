################################################################################
#commons.R
#
#This file contains vectors which are used by the main function in main.R.
################################################################################

MIN = 1
MAX = 460

################################################################################
#Vector of FIA sequence numbers (1 - 460)
################################################################################

SEQ<-seq(from = 1, to = MAX, by = 1)

################################################################################
#Vector of recognized FIA species codes less than 1000.
#Vector contains 460 values and is arranged in rows of 4 species codes.
################################################################################

FIA=c(
  "10", "11", "12", "14",
  "15", "16", "17", "18",
  "19", "20", "21", "22",
  "40", "41", "42", "43",
  "50", "51", "52", "53",
  "54", "55", "56", "57",
  "58", "59", "60", "61",
  "62", "63", "64", "65",
  "66", "67", "68", "69",
  "70", "71", "72", "73",
  "74", "75", "81", "90",
  "91", "92", "93", "94",
  "95", "96", "97", "98",
  "100", "101", "102", "103",
  "104", "105", "106", "107",
  "108", "109", "110", "111",
  "112", "113", "114", "115",
  "116", "117", "118", "119",
  "120", "121", "122", "123",
  "124", "125", "126", "127",
  "128", "129", "130", "131",
  "132", "133", "134", "135",
  "136", "137", "138", "139",
  "140", "141", "142", "143",
  "144", "200", "201", "202",
  "211", "212", "220", "221",
  "222", "223", "230", "231",
  "232", "240", "241", "242",
  "250", "251", "252", "260",
  "261", "262", "263", "264",
  "299", "300", "303", "304",
  "310", "311", "312", "313",
  "314", "315", "316", "317",
  "318", "319", "320", "321",
  "322", "323", "330", "331",
  "332", "333", "334", "336",
  "337", "341", "345", "350",
  "351", "352", "353", "355",
  "356", "357", "358", "360",
  "361", "362", "363", "367",
  "370", "371", "372", "373",
  "374", "375", "376", "377",
  "378", "379", "381", "391",
  "400", "401", "402", "403",
  "404", "405", "406", "407",
  "408", "409", "410", "411",
  "412", "413", "420", "421",
  "422", "423", "424", "431",
  "450", "451", "452", "460",
  "461", "462", "463", "471",
  "475", "481", "490", "491",
  "492", "500", "501", "502",
  "503", "504", "505", "506",
  "507", "508", "509", "510",
  "511", "512", "513", "514",
  "520", "521", "522", "523",
  "531", "540", "541", "542",
  "543", "544", "545", "546",
  "547", "548", "549", "550",
  "551", "552", "555", "561",
  "571", "580", "581", "582",
  "583", "591", "600", "601",
  "602", "603", "604", "605",
  "606", "611", "621", "631",
  "641", "650", "651", "652",
  "653", "654", "655", "657",
  "658", "660", "661", "662",
  "663", "664", "680", "681",
  "682", "683", "684", "690",
  "691", "692", "693", "694",
  "701", "711", "712", "715",
  "718", "720", "721", "722",
  "729", "730", "731", "732",
  "740", "741", "742", "743",
  "744", "745", "746", "747",
  "748", "749", "752", "753",
  "755", "756", "757", "758",
  "760", "761", "762", "763",
  "764", "765", "766", "768",
  "769", "770", "771", "772",
  "773", "774", "800", "801",
  "802", "803", "804", "805",
  "806", "807", "808", "809",
  "810", "811", "812", "813",
  "814", "815", "816", "817",
  "818", "819", "820", "821",
  "822", "823", "824", "825",
  "826", "827", "828", "829",
  "830", "831", "832", "833",
  "834", "835", "836", "837",
  "838", "839", "840", "841",
  "842", "843", "844", "845",
  "846", "847", "851", "852",
  "853", "854", "855", "856",
  "857", "858", "859", "860",
  "863", "864", "865", "866",
  "867", "868", "869", "870",
  "873", "874", "876", "877",
  "882", "883", "884", "885",
  "886", "887", "888", "890",
  "891", "895", "896", "897",
  "901", "902", "906", "907",
  "908", "909", "911", "912",
  "913", "914", "915", "919",
  "920", "921", "922", "923",
  "924", "925", "926", "927",
  "928", "929", "931", "934",
  "935", "936", "937", "940",
  "950", "951", "952", "953",
  "970", "971", "972", "973",
  "974", "975", "976", "977",
  "981", "982", "986", "987",
  "988", "989", "990", "991",
  "992", "993", "994", "995",
  "996", "997", "998", "999")

################################################################################
#Vector of USDA plant symbols.
#Vector contains 460 values and is arranged in rows of 4 species codes.
#index 15 (CHNO) changed to CANO9
################################################################################

PLANT=c(
  "ABIES", "ABAM", "ABBA", "ABBR",
  "ABCO", "ABFR", "ABGR", "ABLAA",
  "ABLA", "ABMA", "ABSH", "ABPR",
  "CHAMA4", "CHLA", "CANO9", "CHTH2",
  "CUPRE", "CUAR", "CUBA", "CUFO2",
  "CUMA2", "CUSA3", "CUMA", "JUNIP",
  "JUPI", "JUCO11", "JUFL", "JUAS",
  "JUCA7", "JUDE2", "JUOC", "JUOS",
  "JUSC2", "JUVIS", "JUVI", "JUMO",
  "LARIX", "LALA", "LALY", "LAOC",
  "LAKA2", "LASI3", "CADE27", "PICEA",
  "PIAB", "PIBR", "PIEN", "PIGL",
  "PIMA", "PIPU",  "PIRU", "PISI",
  "PINUS", "PIAL", "PIAR", "PIAT",
  "PIBA", "PIBA2", "PIED", "PICL",
  "PICO", "PICO3", "PIEC2", "PIEL",
  "PIEN2", "PIFL2", "PIST3", "PIGL2",
  "PIJE", "PILA", "PILE", "PIMO3",
  "PIMU", "PIPA2", "PIPO", "PIPU5",
  "PIRA2", "PIRE", "PIRI", "PISA2",
  "PISE", "PIST", "PISY", "PITA",
  "PIVI2", "PIMO", "PIDI3", "PIAR5",
  "PINI", "PIWA", "PIQU", "PITO",
  "PICE", "PIRE5", "PILO", "PIMOF",
  "PIELE2", "PSEUD7", "PSMA", "PSME",
  "SESE3", "SEGI2", "TAXOD", "TADI2",
  "TAAS", "TAMU", "TAXUS", "TABR2",
  "TAFL", "THUJA", "THOC2", "THPL",
  "TORRE", "TOCA", "TOTA", "TSUGA",
  "TSCA", "TSCA2", "TSHE", "TSME",
  "2TN", "ACACI", "ACFA", "ACGR",
  "ACER", "ACBA3", "ACMA3", "ACNE2",
  "ACNI5", "ACPE", "ACRU", "ACSA2",
  "ACSA3", "ACSP2", "ACPL", "ACGL",
  "ACGR3", "ACLE", "AESCU", "AEGL",
  "AEFL", "AECA", "AEGLA", "AEPA",
  "AESY", "AIAL", "ALJU", "ALNUS",
  "ALRU2", "ALRH2", "ALOB2", "ALGL2",
  "AMELA", "AMAR3", "AMSA", "ARBUT",
  "ARME", "ARAR2", "ARXA80", "ASTR",
  "BETUL", "BEAL2", "BELE", "BENI",
  "BEOC2", "BEPA", "BENE4", "BEUB",
  "BEUT", "BEPO", "SILAL3", "CACA18",
  "CARYA", "CAAQ2", "CACO15", "CAGL8",
  "CAIL2", "CALA21", "CAMY", "CAOV2",
  "CATE9", "CAAL27", "CAPA24", "CAFL6",
  "CAOV3", "CACA38", "CASTA", "CADE12",
  "CAPU9", "CAPUO", "CAMO83", "CHCHC4",
  "CATAL", "CABI8", "CASP8", "CELTI",
  "CELA", "CEOC", "CELAR", "CECA4",
  "CELE3", "CLKE", "CORNU", "COFL2",
  "CONU4", "CRATA", "CRCR2", "CRMO2",
  "CRBR3", "CRCA", "CRCH", "CRDI",
  "CRFL", "CRMO3", "CRPE", "EUCAL",
  "EUGL", "EUCA2", "EUGR12", "EURO2",
  "DIOSP", "DIVI5", "DITE3", "EHAN",
  "FAGR", "FRAXI", "FRAM2", "FRLA",
  "FRNI", "FRPE", "FRPR", "FRQU",
  "FRVE2", "FRCA3", "FRTE", "GLEDI",
  "GLAQ", "GLTR", "GOLA", "GIBI2",
  "GYDI", "HALES", "HACA3", "HADI3",
  "HAPA2", "ILOP", "JUGLA", "JUCI",
  "JUNI", "JUHI", "JUCA", "JUMI",
  "JUMA", "LIST2", "LITU", "LIDE3",
  "MAPO", "MAGNO", "MAAC", "MAGR4",
  "MAVI2", "MAMA2", "MAFR", "MAPY",
  "MATR", "MALUS", "MAFU", "MAAN3",
  "MACO5", "MAIO", "MORUS", "MOAL",
  "MORU2", "MOMI", "MONI", "NYSSA",
  "NYAQ2", "NYOG", "NYSY", "NYBI",
  "OSVI", "OXAR", "PATO2", "MAPA28",
  "OSTR", "PERSE", "PEBO", "PLAQ",
  "PLATA", "PLRA", "PLOC", "PLWR2",
  "POPUL", "POBA2", "PODE3", "POGR4",
  "POHE4", "PODEM", "POTR5", "POBAT",
  "POFR2", "POAN3", "POAL7", "PONI",
  "PROSO", "PRGL2", "PRVE", "PRPU",
  "PRUNU", "PRPE2", "PRSE2", "PRVI",
  "PRPE3", "PRNI", "PRAM", "PREM",
  "PRAL5", "PRAN3", "PRAV", "PRCE",
  "PRDO", "PRMA", "QUERC", "QUAG",
  "QUAL", "QUAR", "QUBI", "QUCH2",
  "QUCO2", "QUDO", "QUSIS", "QUEL",
  "QUEM", "QUEN", "QUFA", "QUPA5",
  "QUGA", "QUGA4", "QUIL", "QUIM",
  "QUKE", "QULA2", "QULA3", "QULO",
  "QULY", "QUMA2", "QUMA3", "QUMI",
  "QUMU", "QUNI", "QUTE", "QUOB",
  "QUPA2", "QUPH", "QUPR2", "QURU",
  "QUSH", "QUST", "QUSI2", "QUVE",
  "QUVI", "QUWI2", "QUMA6", "QUMI2",
  "QUIN", "QUHY", "QUOG", "QUPR",
  "QUGR3", "QURU4", "QUGR", "AMEL",
  "ANGL4", "BUSI", "CASUA", "CAGL11",
  "CALE28", "CICA", "CIFR", "CITRU2",
  "CODI8", "COEL2", "COSE2", "CUAN4",
  "COHO", "EBEB", "LEPU3", "SOAF",
  "EURH", "EXPA", "FIAU", "FICI",
  "GUDI", "HIMA2", "LYLA3", "MAIN3",
  "METO3", "PIPI3", "SCAC2", "SIFO",
  "SISA6", "SIGL3", "SYCU", "TAIN2",
  "ROPS", "RONE", "ACWR4", "COAR",
  "CONU", "ROYST", "SAME8", "SAPA",
  "THMO4", "THRA2", "ARECA", "SASAD",
  "SALIX", "SAAM2", "SANI", "SABE2",
  "SABO", "SACA5", "SAPY", "SAAL2",
  "SASC", "SASE10", "SAAL5", "SORBU",
  "SOAM3", "SOAU", "SODE3", "SWMA2",
  "TILIA", "TIAM", "TIAMH", "TIAMC",
  "ULMUS", "ULAL", "ULAM", "ULCR",
  "ULPU", "ULRU", "ULSE", "ULTH",
  "UMCA", "YUBR", "AVGE", "COER2",
  "LARA2","RHMA2", "OLTE", "TAMAR2",
  "MEQU", "MEAZ", "TRSE6", "VEFO",
  "COOB2", "ELAN", "2TB", "2TREE")

#############################################################################
#Vector of GENUS values.
#Vector contains 460 values and is arranged in rows of 4 genus values.
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
  "THRINAX",        "THRINAX",        "FAMILY ARECACEAE","SAPINDUS",
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

################################################################################
#Vector of leaf retention values from USFS R3.
#Vector contains 460 values and is arranged in rows of 4 leaf retention values.
################################################################################

LEAF_RETEN<-c("EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "EVERGREEN", "EVERGREEN", "EVERGREEN",
              "EVERGREEN", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "EVERGREEN", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "EVERGREEN", "DECIDUOUS", "EVERGREEN",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "EVERGREEN", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "EVERGREEN",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "EVERGREEN", "DECIDUOUS", "DECIDUOUS",
              "EVERGREEN", "EVERGREEN", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "EVERGREEN", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS",
              "DECIDUOUS", "DECIDUOUS", "DECIDUOUS", "DECIDUOUS")

################################################################################
#USFS R3 Shade Tolerance Values
#Vector contains 460 values and is arranged in rows of 4 shade tolerance values.
################################################################################

R3_SHADE_TOL <- c(NA,    NA,    NA,    NA,
                  "TOL", NA,    NA,    "TOL",
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    "INT", NA,    NA,
                  NA,    NA,    NA,    NA,
                  "INT", "INT", NA,    "INT",
                  NA,    "INT", NA,    "INT",
                  "INT", NA,    NA,    "INT",
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    "TOL", NA,
                  NA,    "TOL", NA,    NA,
                  NA,    NA,    "INT", NA,
                  NA,    NA,    "INT", NA,
                  NA,    NA,    NA,    NA,
                  "INT", "INT", "INT", NA,
                  NA,    NA,    "INT", NA,
                  NA,    NA,    "INT", NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    "INT", "INT", "INT",
                  NA,    NA,    NA,    NA,
                  "INT", NA,    NA,    NA,
                  NA,    NA,    NA,    "TOL",
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    "INT",
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    "INT",
                  "INT", NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    "INT", NA,
                  NA,    NA,    NA,    NA,
                  NA,    "TOL", "TOL", NA,
                  NA,    NA,    NA,    NA,
                  "INT", NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    "INT", NA,
                  NA,    "INT", "INT", NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    "INT", NA,    NA,
                  "INT", NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    "INT",
                  "INT", NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    "INT",
                  NA,    "INT", NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    "INT",
                  NA,    NA,    "INT", NA,
                  NA,    NA,    "INT", NA,
                  "INT", "INT", NA,    NA,
                  NA,    "INT", "INT", NA,
                  NA,    NA,    "INT", "INT",
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    "INT", NA,    "INT",
                  NA,    NA,    NA,    NA,
                  "INT", NA,    NA,    NA,
                  "INT", NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    "INT", NA,    NA,
                  "INT", NA,    NA,    "INT",
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    "INT", NA,    NA,
                  "INT", "INT", NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    "INT", NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    "INT", NA,
                  "INT", NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    NA,    NA,
                  NA,    NA,    "INT", NA,
                  NA,    NA,    NA,    NA,
                  NA,    "INT", NA,    NA)
