################################################################################
#Function: R1
#
#Calculates variables previously available in the legacy R1 Existing
#Vegetation Classifier post-processor to FVS.
#
#Arguments
#
#data:    Data frame containing tree records from a single stand or plot. Data
#         frame must contain a column corresponding to stand/plot ID, DBH,
#         species code (USDA plant symbol), expansion factor, and height
#         for each tree record.
#
#stand:   Character string corresponding to name of column pertaining to stand
#         or plot ID associated with tree records in data argument. By default,
#         this value is set to "StandID".
#
#species: Character string corresponding to name of column pertaining to USDA
#         plant symbols of tree records in data argument. By default, this
#         argument is set to "SpeciesPLANTS".
#
#dbh:     Character string corresponding to name of column pertaining to DBH of
#         tree records in data argument. By default, this argument is set to
#         "DBH".
#
#expf:    Character string corresponding to name of column pertaining to TPA of
#         tree records in data argument. By default, this argument is set to
#         "TPA".
#
#ht:      Character string corresponding to name of column pertaining to Ht of
#         tree records in data argument. By default, this argument is set to
#         "Ht".
#
#TPA:     TPA of stand/plot.
#
#BA:      Basal area of stand/plot.
#
#plotvals: Named list containing plot attributes for each species in plot/stand and/or
#across all species in plot/stand.
#
#debug:	  Logical variable used to specify if debug output should be printed to
#         R console. If value is TRUE, then debug output will printed to R
#         console.
#
#con:     Connection to database defined in input
#
#Return value
#
#Named list containing
# - STSIM potential vegetation type grouping (VEGTYPE)
# - R1 Cover Type (COVERTYPE_R1)
# - Subclass DOM6040
# - number of Canopy layers (VERTICAL STRUCTURE)--1,2,3 or continuous (C)
# - Basal area weighted diameter size class (SIZECLASS_NTG)
# - Size & density structure class strata (STRCLSSTR)
################################################################################
#'@export
R1<-function(data,
                  stand = "StandID",
                  species = "SpeciesPLANTS",
                  dbh = "DBH",
                  expf = "TPA",
                  ht = "Ht",
                  TPA,
                  BA,
                  plotvals,
                  debug = F,
                  InvDB=InvDB,
                  InvStandTbl=InvStandTbl){

  #Calculate the number of trees in the output treelist for the plot
  Ntrees <- length(data$SpeciesPLANTS)

  #Initialize results vector to NA
  results=list("VEGTYPE" = NA,
               "COVERTYPE_R1" = NA,
               "DOM6040" = NA,
               "VERTICAL_STRUCTURE" = NA,
               "SIZECLASS_NTG" = NA,
               "STRCLSSTR" = NA)

  #Test if value in InvDB argument is null.
  if (is.null(InvDB)){
    stop(paste("No inventory database specified in InvDB argument.",
               "The PV_CODE variable is needed for R1 classification"))
  }
  #Test if value in InvStandTbl argument is null.
  if (is.null(InvStandTbl)){
    stop(paste("No inventory stand table specified in InvDB argument.",
               "The PV_CODE variable is needed for R1 classification"))
  }
  #Test existence of input database.
  if (!(file.exists(InvDB))){
    stop(paste("Inventory database (InvDB) specified not found. Make sure",
               "directory path and file name in input are spelled correctly."))
  }
  #Extract file extension for input argument.
  fileExtInv<-sub("(.*)\\.","",InvDB)

  #Make sure input database is SQLite (.db).
  if (!fileExtInv %in% "db"){
    stop(paste("InvDB argument does not have a valid file extension. File",
               "extension must be .db."))
  }
  #Connect to inventory database
  conInv<-RSQLite::dbConnect(RSQLite::SQLite(), InvDB)
  #Establish connection to inventory database
  #check if InvStandTbl table exists
  if(RSQLite::dbExistsTable(conInv, InvStandTbl)){
    HABTYPE <- ""
    cat("Inventory stand table exists for PV_CODE query", "\n")
    query<- paste("SELECT PV_CODE", "FROM ",InvStandTbl)

    #Add quotes to stand and commas to stIDs
    stIDs<-paste0("'",data$StandID[1],"'", ",")
    #Collapse stIDs into a single string
    stIDs<-paste(stIDs, collapse = "")
    #Remove last comma from stIDs
    stIDs<-substr(stIDs,1, nchar(stIDs)-1)
    #Add parentheses around stIDs
    stIDs<-paste0("(", stIDs, ")")
    #Create WHERE clause with stIDs
    standQuery<-paste0("WHERE STAND_ID IN", stIDs)
    #Add standQuery to query
    query<-paste(query, standQuery)

    habquery <- try(RSQLite::dbGetQuery(conInv, query))
    if(class(habquery) != "try-error") {
      cat("PV_CODE exists as",habquery[[1]][1], "\n")
      HABTYPE <- habquery[[1]][1]
    }
    else{
      stop(paste("PV_CODE value needed for R1 classification is
          missing from the inventory stand table.","\n"))
    }
  }

  #Check for missing columns in data
  missing <- c(stand, species, dbh, expf, ht) %in% colnames(data)

  #If name of columns provided in stand, species, dbh, expf, and ht  are not
  #found in data warning message is issued and NA values are returned.
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(results)
  }

  #Print stand
  if(debug)
  {
    cat("In function R1", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("Columns:", "\n",
                "stand:", stand, "\n",
                "species:", species, "\n",
                "dbh:", dbh, "\n",
                "expf:", expf, "\n",
                "ht:", ht, "\n","\n")
  }
  #Initialize return attribute variables as NA
  VEGTYPE<-NA
  COVERTYPE_R1<-NA
  DOM6040<-NA
  VERTICAL_STRUCTURE<-NA
  SIZECLASS_NTG<-NA
  STRCLSSTR<-NA

  #Print plot BA and TPA if debug is TRUE
  if(debug) cat("BA of plot is", BA, "\n")
  if(debug) cat("TPA of plot is", TPA, "\n")

  #Stand-level basal area weighted diameter
  StBAWTDBH <- round(plotvals[["ALL"]]["BA_WT_DIA"][[1]][1],2)

  #Stand-level canopy cover percent
  StCC <- round(plotvals[["ALL"]]["CC"][[1]][1],2)

## Initialize the species-level vectors
  #Identify unique species in stand
  spStand <- unique(data$SpeciesPLANTS)
  #Initialize vector that will store TPA value for each species
  SpeciesTPA <- vector("numeric", length(spStand))
  #Initialize vector that will store BA value for each species
  SpeciesBA <- vector("numeric", length(spStand))
  #Initialize vector that will store proportion of TPA value for each species
  SpeciesPropTPA <- vector("numeric", length(spStand))
  #Initialize vector that will store proportion of BA value for each species
  SpeciesPropBA <- vector("numeric", length(spStand))
  #Initialize vector that will store a total height value for each species
  SpeciesTotalHeight <- vector("numeric", length(spStand))
  #Initialize vector that will store a BA weighted DBH value for each species
  BAWghtDiamBySpecies <- vector("numeric", length(spStand))
  #Initialize vector that will store a BA weighted height value for each species
  BAWghtHeightBySpecies <- vector("numeric", length(spStand))
  #Initialize vector that will store an average height value for each species
  AvgHeightBySpecies <- vector("numeric", length(spStand))

  #Initialize diameter class vector
  BAByDiameterClass <- vector("numeric",6)

  #Populate BAByDiameterClass vector
  for (i in 1:Ntrees){
    if(data$DBH[i] >= 0.0 && data$DBH[i] < 5.0)
      BAByDiameterClass[1] <- BAByDiameterClass[1] + data$TREEBA[i]
    if(data$DBH[i] >= 5.0 && data$DBH[i] < 10.0)
      BAByDiameterClass[2] <- BAByDiameterClass[2] + data$TREEBA[i]
    if(data$DBH[i] >= 10.0 && data$DBH[i] < 15.0)
      BAByDiameterClass[3] <- BAByDiameterClass[3] + data$TREEBA[i]
    if(data$DBH[i] >= 15.0 && data$DBH[i] < 20.0)
      BAByDiameterClass[4] <- BAByDiameterClass[4] + data$TREEBA[i]
    if(data$DBH[i] >= 20.0 && data$DBH[i] < 25.0)
      BAByDiameterClass[5] <- BAByDiameterClass[5] + data$TREEBA[i]
    if(data$DBH[i] >= 25.0)
      BAByDiameterClass[6] <- BAByDiameterClass[6] + data$TREEBA[i]
  }

  #Weight BAByDiameterClass vector by BA
  for (jdiam in 1:length(BAByDiameterClass) ){
    BAByDiameterClass[jdiam] <- round((BAByDiameterClass[jdiam]/BA),2)
  }

  #Populate the species-level vectors
  #SpeciesTPA
  for(i in 1:length(spStand)){
    SpeciesTPA[i] <- plotvals[[spStand[i]]]["TPA"]
  }
  #SpeciesBA
  for(i in 1:length(spStand)){
    SpeciesBA[i] <- plotvals[[spStand[i]]]["BA"]
  }
  #SpeciesPropTPA
  for(i in 1:length(spStand)){
    SpeciesPropTPA[i] <- SpeciesTPA[i]/TPA
  }
  #SpeciesPropBA
  for(i in 1:length(spStand)){
    SpeciesPropBA[i] <- SpeciesBA[i]/BA
  }
  #BAWghtDiamBySpecies
  for(i in 1:length(spStand)){
    BAWghtDiamBySpecies[i] <- plotvals[[spStand[i]]]["BA_WT_DIA"]
  }
  #BAWghtHeightBySpecies
  for(i in 1:length(spStand)){
    BAWghtHeightBySpecies[i] <- plotvals[[spStand[i]]]["BA_WT_HT"]
  }
  #AvgHeightBySpecies
  for(i in 1:length(spStand)){
    AvgHeightBySpecies[i] <- plotvals[[spStand[i]]]["AVE_HT"]
  }

  #If debug, print the values above
  if(debug)
  {
    cat("Species in stand:", spStand, "\n")
    cat("Species TPA in stand:", SpeciesTPA, "\n")
    cat("Species BA in stand:", SpeciesBA, "\n")
    cat("Species Total Height in the stand:", SpeciesTotalHeight, "\n")
    cat("Species proportion of TPA:", SpeciesPropTPA, "\n")
    cat("Species proportion of BA:", SpeciesPropBA, "\n")
    cat("Species BAWTDBH:", BAWghtDiamBySpecies, "\n")
    cat("Species BAWTHT:", BAWghtHeightBySpecies, "\n")
    cat("Species average height:", AvgHeightBySpecies, "\n")
  }

  if(BA < 20 && TPA > 100){
    DOM6040 <- computeDominance6040(TPA,spStand,SpeciesPropTPA,BAWghtDiamBySpecies,
                      AvgHeightBySpecies)
  }else if(BA > 20){
    DOM6040 <- computeDominance6040(BA,spStand,SpeciesPropBA,BAWghtDiamBySpecies,
                      BAWghtHeightBySpecies)
  }else{
    DOM6040 <- "NONE"
  }

MapDominance6040toCovertype <- c(
   "ABGR"="mixedmesiccon", "ABGR-IMIX"="mixedmesiccon",
   "ABGR-TMIX"="mixedmesiccon", "ABGR-HMIX"="mixedmesiccon",
   "ABLA"="sprucefir", "ABLA-IMIX"="sprucefir",
   "ABLA-TMIX"="sprucefir", "ABLA-HMIX"="sprucefir", "ACER"="aspenhardwood",
   "ACER-IMIX"="aspenhardwood","ACER-TMIX"="aspenhardwood",
   "BEPA"="aspenhardwood", "BEPA-IMIX"="aspenhardwood",
   "BEPA-TMIX"="aspenhardwood", "BEPA-HMIX"="aspenhardwood",
   "CELE3"="dryshrub", "CELE3-IMIX"="dryshrub",
   "CELE3-TMIX"="dryshrub", "CELE3-HMIX"="dryshrub",
   "JUNIP"="ponderosapine", "JUNIP-IMIX"="ponderosapine",
   "JUNIP-TMIX"="ponderosapine", "JUNIP-HMIX"="ponderosapine",
   "LALY"="whitebarksubalpinelarch", "LALY-IMIX"="whitebarksubalpinelarch",
   "LALY-TMIX"="whitebarksubalpinelarch", "LALY-HMIX"="whitebarksubalpinelarch",
   "LAOC"="wlarchmixedcon", "LAOC-IMIX"="wlarchmixedcon",
   "LAOC-TMIX"="wlarchmixedcon", "LAOC-HMIX"="wlarchmixedcon",
   "PIAL"="whitebarksubalpinelarch", "PIAL-IMIX"="whitebarksubalpinelarch",
   "PIAL-TMIX"="whitebarksubalpinelarch", "PIAL-HMIX"="whitebarksubalpinelarch",
   "PICO"="lodgepolepine", "PICO-IMIX"="lodgepolepine",
   "PICO-TMIX"="lodgepolepine", "PICO-HMIX"="lodgepolepine",
   "PIEN"="sprucefir", "PIEN-IMIX"="sprucefir",
   "PIEN-TMIX"="sprucefir", "PIEN-HMIX"="sprucefir",
   "PIFL2"="ponderosapine", "PIFL2-IMIX"="ponderosapine",
   "PIFL2-TMIX"="ponderosapine", "PIFL2-HMIX"="ponderosapine",
   "PIMO3"="mixedmesiccon", "PIMO3-IMIX"="mixedmesiccon",
   "PIMO3-TMIX"="mixedmesiccon", "PIMO3-HMIX"="mixedmesiccon",
   "PIPO"="ponderosapine", "PIPO-IMIX"="ponderosapine",
   "PIPO-TMIX"="ponderosapine", "PIPO-HMIX"="ponderosapine",
   "POPUL"="aspenhardwood", "POPUL-IMIX"="aspenhardwood",
   "POPUL-TMIX"="aspenhardwood", "POPUL-HMIX"="aspenhardwood",
   "POTR5"="aspenhardwood", "POTR5-IMIX"="aspenhardwood",
   "POTR5-TMIX"="aspenhardwood", "POTR5-HMIX"="aspenhardwood",
   "PSME"="mixedmesiccon", "PSME-IMIX"="mixedmesiccon",
   "PSME-TMIX"="mixedmesiccon", "PSME-HMIX"="mixedmesiccon",
   "TABR2"="sprucefir", "TABR2-IMIX"="sprucefir",
   "TABR2-TMIX"="sprucefir", "TABR2-HMIX"="sprucefir",
   "THPL"="mixedmesiccon", "THPL-IMIX"="mixedmesiccon",
   "THPL-TMIX"="mixedmesiccon", "THPL-HMIX"="mixedmesiccon",
   "TSHE"="mixedmesiccon", "TSHE-IMIX"="mixedmesiccon",
   "TSHE-TMIX"="mixedmesiccon", "TSHE-HMIX"="mixedmesiccon",
   "TSME"="mixedmesiccon", "TSME-IMIX"="mixedmesiccon",
   "TSME-TMIX"="mixedmesiccon", "TSME-HMIX"="mixedmesiccon",
   "IMIX"="mixedmesiccon", "TMIX"="mixedmesiccon",
   "HMIX"="aspenhardwood", "FRPE"="aspenhardwood",
   "FRPE-IMIX"="aspenhardwood", "FRPE-TMIX"="aspenhardwood",
   "FRPE-HMIX"="aspenhardwood", "NONE"="NONE"
  )

 #compute CoverType
  COVERTYPE_R1 <- as.character(MapDominance6040toCovertype[DOM6040])

  if(!is.na(COVERTYPE_R1) && COVERTYPE_R1=="mixedmesiccon"){
    Hot_Dry <- c(
      "000", "040" ,"050" ,"051" ,"052",
      "070" ,"090" ,"091" ,"092" ,"093",
      "094" ,"095")
    Warm_Dry <- c(
      "100", "110", "130", "140",
      "141", "142", "160", "161",
      "162", "103", "104", "100032",
      "100033", "100034", "100035", "100037",
      "105", "106", "150", "200",
      "210", "220", "230", "205",
      "390", "311", "380", "321",
      "180", "181", "182")

    switchToDDF <- match(HABTYPE, Hot_Dry)
    if(is.na(switchToDDF)) switchToDDF <- match(HABTYPE, Warm_Dry)
    if(!is.na(switchToDDF) && COVERTYPE_R1=="mixedmesiccon"){
      COVERTYPE_R1 <- "dryDouglasfir"
    }
  }

  #compute VERTICAL_STRUCTURE
  if(BA < 20 && TPA < 100){
    VERTICAL_STRUCTURE <- "NONE"
  }
  else if(BA < 20 && TPA >= 100){
    VERTICAL_STRUCTURE <- 1
  }
  else {
    VERTICAL_STRUCTURE <- computeVerticalStructure(BAByDiameterClass)
  }

  MapADPtoStSimPVT <- c(
    "110"="HODR","130"="HODR","140"="HODR","141"="HODR",
    "142"="HODR","160"="HODR","161"="HODR","162"="HODR",
    "170"="HODR","171"="HODR","172"="HODR","180"="HODR",
    "181"="HODR","182"="HODR","190"="HODR","200"="HODR",
    "205"="WMMW","210"="HODR","220"="HODR","230"="HODR",
    "240"="WMMW","250"="WMMW","260"="WMMW","261"="WMMW",
    "262"="WMMW","263"="WMMW","280"="WMMW","281"="WMMW",
    "282"="WMMW","283"="WMMW","290"="WMMW","291"="WMMW",
    "292"="WMMW","293"="WMMW","310"="WMMW","311"="HODR",
    "312"="WMMW","313"="WMMW","320"="WMMW","321"="HODR",
    "322"="WMMW","323"="WMMW","324"="HODR","330"="WMMW",
    "340"="WMMW","350"="WMMW","360"="WMMW","370"="WMMW",
    "380"="HODR","390"="WMMW","400"="COOL","410"="COOL",
    "420"="COOL","421"="COOL","422"="COOL","430"="WMMW",
    "440"="COOL","450"="COOL","457"="COOL","460"="COOL",
    "461"="COOL","462"="COOL","470"="COOL","472"="COOL",
    "475"="COOL","480"="COOL","500"="COOL","501"="WAMO",
    "505"="WMMW","506"="WMMW","507"="WMMW","508"="WMMW",
    "510"="WMMW","511"="WMMW","512"="WMMW","515"="WMMW",
    "516"="WAMO","517"="WAMO","518"="WAMO","519"="WAMO",
    "520"="WAMO","521"="WAMO","522"="WAMO","523"="WAMO",
    "524"="WAMO","525"="WAMO","526"="WAMO","529"="WAMO",
    "530"="WAMO","531"="WAMO","532"="WAMO","533"="WAMO",
    "534"="WAMO","535"="WAMO","540"="WAMO","541"="WAMO",
    "542"="WAMO","545"="WAMO","546"="WAMO","547"="WAMO",
    "548"="WAMO","550"="WAMO","555"="WAMO","560"="WAMO",
    "565"="WAMO","570"="WAMO","571"="WAMO","572"="WAMO",
    "573"="WAMO","574"="WAMO","575"="WAMO","576"="WAMO",
    "577"="WAMO","578"="WAMO","579"="WAMO","590"="WMMW",
    "591"="WMMW","592"="WMMW","600"="COOL","610"="COOL",
    "620"="COOL","621"="COOL","622"="COOL","623"="COOL",
    "624"="COOL","625"="COOL","630"="COOL","631"="COOL",
    "632"="COOL","635"="COOL","636"="COOL","637"="COOL",
    "640"="COOL","650"="COOL","651"="COOL","652"="COOL",
    "653"="COOL","654"="COOL","655"="COOL","660"="COOL",
    "661"="COOL","662"="COOL","663"="COOL","670"="COOL",
    "671"="COOL","672"="COLD","673"="COOL","674"="COLD",
    "675"="COOL","676"="COLD","677"="COOL","680"="COOL",
    "681"="COLD","682"="COOL","685"="COOL","686"="COOL",
    "687"="COOL","690"="COOL","691"="COOL","692"="COLD",
    "693"="COOL","694"="COLD","700"="COLD","710"="COOL",
    "711"="COLD","712"="COOL","713"="COLD","720"="COOL",
    "730"="COLD","731"="COLD","732"="COLD","733"="COLD",
    "740"="COOL","745"="COOL","750"="COOL","770"="COOL",
    "780"="COOL","790"="COOL","791"="COOL","792"="COOL",
    "800"="COOL","810"="COLD","820"="COLD","830"="COLD",
    "831"="COLD","832"="COLD","840"="COLD","841"="COLD",
    "842"="COLD","850"="COLD","860"="COLD","870"="COLD",
    "890"="COLD","900"="COLD","910"="WMMW","920"="COOL",
    "930"="COOL","940"="COLD","950"="COOL","100032"="HODR",
    "100033"="HODR","100034"="HODR","100035"="HODR","100037"="HODR"
  )

  #compute VEGTYPE
  if(HABTYPE!=""){
    #map R1HABTYP to StSimPVT
    PVT <- as.character(MapADPtoStSimPVT[as.character(HABTYPE)])

    AbbreviateCovertype <- c(
      "mixedmesiccon"="MMC","sprucefir"="SAF","aspenhardwood"="ASH",
      "dryshrub"="DSH","ponderosapine"="POP","whitebarksubalpinelarch"="WBP",
      "wlarchmixedcon"="LMC","lodgepolepine"="LPP","dryDouglasfir"="DDF",
      "NONE"="NONE")

    #Abbreviate COVERTYPE_R1
    COVABBR <- as.character(AbbreviateCovertype[COVERTYPE_R1])

    #Concatenate StSimPVT with the abbreviation of COVERTYPE_R1
    VEGTYPE <- paste0(PVT,"-",COVABBR)
  }

  #compute SIZECLASS_NTG
  if(!is.na(StBAWTDBH) || !is.null(StBAWTDBH)){
    if(StBAWTDBH <=0.1){
      SIZECLASS_NTG <- "Seedling"
    }
    if (StBAWTDBH > 0.1 && StBAWTDBH < 5.0){
      SIZECLASS_NTG <- "00.1-04.9"
    }
    if (StBAWTDBH >= 5.0 && StBAWTDBH < 10.0){
      SIZECLASS_NTG <- "05.0-09.9"
    }
    if (StBAWTDBH >= 10.0 && StBAWTDBH < 15.0){
      SIZECLASS_NTG <- "10.0-14.9"
    }
    if (StBAWTDBH >= 15.0 && StBAWTDBH < 20.0){
      SIZECLASS_NTG <- "15.0-19.9"
    }
    if (StBAWTDBH >= 20.0 && StBAWTDBH < 25.0){
      SIZECLASS_NTG <- "20.0-24.9"
    }
    if (StBAWTDBH >= 25.0){
      SIZECLASS_NTG <- "25.0+"
    }
  }

  #compute structure class strata STRCLSSTR variable
  if(!is.na(StBAWTDBH) && !is.na(StCC)){
    if(StCC<10) {
      STRCLSSTR <- "X"
    } else {
      if(StBAWTDBH>=0 && StBAWTDBH<5){#0-4.9"
        STRCLSSTR <- if(StCC<40) "E" else "F"
      }
      if(StBAWTDBH>=5 && StBAWTDBH<10){#5-9.9"
        STRCLSSTR <- if(StCC<40) "G" else "H"
      }
      if(StBAWTDBH>=10 && StBAWTDBH<15){#10-14.9"
        if(StCC>=10 && StCC<40){
          STRCLSSTR <- "I"
        } else if (StCC>=40 && StCC<60){
          STRCLSSTR <- "J"
        } else STRCLSSTR <- "K"
      }
      if(StBAWTDBH>=15 && StBAWTDBH<20){#15-19.9"
        if(StCC>=10 && StCC<40){
          STRCLSSTR <- "L"
        } else if (StCC>=40 && StCC<60){
          STRCLSSTR <- "M"
        } else STRCLSSTR <- "N"
      }
      if(StBAWTDBH>=20){#20"+
        if(StCC>=10 && StCC<40){
          STRCLSSTR <- "O"
        } else if (StCC>=40 && StCC<60){
          STRCLSSTR <- "P"
        } else STRCLSSTR <- "Q"
      }
    }
  }

  results=list("VEGTYPE" = VEGTYPE,
               "COVERTYPE_R1" = COVERTYPE_R1,
               "DOM6040" = DOM6040,
               "VERTICAL_STRUCTURE" = VERTICAL_STRUCTURE,
               "SIZECLASS_NTG" = SIZECLASS_NTG,
               "STRCLSSTR" = STRCLSSTR)

  return(results)
}
