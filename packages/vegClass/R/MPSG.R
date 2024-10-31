################################################################################
#Function: MPSG
#
#Calculates standardized variables for use by the Mountain Planning Services
#Group (MPSG).
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
#crwidth: Character string corresponding to name of column pertaining to crown
#         width values of tree records in data argument. By default, this
#         argument is set to "CrWidth".
#
#TPA:     TPA of stand/plot.
#
#BA:      Basal area of stand/plot.
#
#CC:      Percent canopy cover of stand/plot.
#
#plotvals: Named list containing plot attributes for each species in plot/stand
#         and/or across all species in plot/stand.
#
#InvDB:   Connection to inventory database defined in input
#
#InvStandTbl: Character string corresponding to name of the stand data table
#         in InvDB
#
#MPSGcovTyp: Integer value corresponding to the USFS region number whose
#         algorithms should be used in the calculations of the
#         COVERTYPE_MPSG variable.
#
#addHSS:	Logical variable used to indicate whether the USFS Region 2
#         Habitat Structural Stage (HSS) variables should be included in
#         the file specified in output argument.
#
#debug:	  Logical variable used to specify if debug output should be printed to
#         R console. If value is TRUE, then debug output will printed to R
#         console.
#
#Return value
#
#Named list containing
# - MPSG Cover Type (COVERTYPE)
# - MPSG tree size class (TREE_SIZE_CLASS)
# - MPSG percent canopy cover class (CROWN_CLASS)
# - number of canopy layers (VERTICAL STRUCTURE)
# - Habitat structural stage calculated and reported for the 1 through 4C classes (HSS1_4C)
# - Habitat structural stage calculated and reported for the 1 through 5 classes (HSS1_5)
################################################################################
#'@export
MPSG<-function(data,
                   stand = "StandID",
                   species = "SpeciesPLANTS",
                   dbh = "DBH",
                   expf = "TPA",
                   ht = "Ht",
                   crwidth = crwidth,
                   TPA,
                   BA,
                   CC,
                   plotvals,
                   InvDB,
                   InvStandTbl,
                   MPSGcovTyp,
                   addHSS,
                   debug = F){

  #Calculate the number of trees in the output treelist for the plot
  Ntrees <- nrow(data)

  #Initialize results vector to NA
  if(addHSS){
    results=list("COVERTYPE_MPSG" = NA,
                 "TREE_SIZE_CLASS" = NA,
                 "CROWN_CLASS" = NA,
                 "VERTICAL_STRUCTURE" = NA,
                 "HSS1_4C" = NA,
                 "HSS1_5" = NA)
  } else {
    results=list("COVERTYPE_MPSG" = NA,
                 "TREE_SIZE_CLASS" = NA,
                 "CROWN_CLASS" = NA,
                 "VERTICAL_STRUCTURE" = NA)
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
    cat("In function MPSG", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("Columns:", "\n",
        "stand:", stand, "\n",
        "species:", species, "\n",
        "dbh:", dbh, "\n",
        "expf:", expf, "\n",
        "ht:", ht, "\n","\n")
  }

  #Initialize return attribute variables as NA
  DOM_TYPE_R2<-NA
  DOM_TYPE_R2_CC1<-NA
  DOM_TYPE_R2_CC2<-NA
  DOM_TYPE_R2_CC3<-NA
  COVERTYPE_MPSG<-NA
  TREE_SIZE_CLASS<-NA
  CROWN_CLASS<-NA
  VERTICAL_STRUCTURE<-NA
  if(addHSS) HSS1_4C<-NA
  if(addHSS) HSS1_5<-NA

  #Print plot BA and TPA if debug is TRUE
  if(debug) cat("BA of plot is", BA, "\n")
  if(debug) cat("TPA of plot is", TPA, "\n")

  ## Initialize the species-level vectors
  #Identify unique species in stand
  spStand <- unique(data$SpeciesPLANTS)
  #Initialize vector that will store CC value for each species
  SpeciesCC <- vector("numeric", length(spStand))

  #Stand-level canopy cover percent
  STCC <- round(plotvals[["ALL"]]["CC"][[1]][1],2)
  #If a stands is less than 10%CC, classify DLF_GSC as "TREE"
  # DLF_GSC <- if(STCC < 10) "OTHER" else "TREE"

  if(STCC < 10){
    DOM_TYPE_R2 <- "NONE"
    DOM_TYPE_R2_CC1 <- "NONE"
    DOM_TYPE_R2_CC2 <- "NONE"
    DOM_TYPE_R2_CC3 <- "NONE"
    TREE_SIZE_CLASS <- "h"
    CROWN_CLASS <- "n"
    COVERTYPE_MPSG <- "NONE"
  }
  if(STCC >= 10){

    if(MPSGcovTyp==1){
      ctResults<-R1(data = data,
                    stand = stand,
                    species = species,
                    dbh = dbh,
                    expf = expf,
                    ht = ht,
                    TPA = TPA,
                    BA = BA,
                    plotvals = plotvals,
                    InvDB=InvDB,
                    InvStandTbl=InvStandTbl)
    COVERTYPE_MPSG <- ctResults$COVERTYPE_R1
    }
    if(MPSGcovTyp==2){
      #Call R2 function to get the R2-flavored covertype
      ctResults<-R2(data = data,
                    stand = stand,
                    species = species,
                    dbh = dbh,
                    expf = expf,
                    ht = ht,
                    TPA = TPA,
                    BA = BA,
                    CC = CC,
                    plotvals = plotvals)
      #Data dictionary for crosswalking the R2 covertype labels with the MPSG labels
      R2toMPSG <- c(
        "TAA"="POTR5","TSF"="ABLA0","TDF"="PSME0","TCW"="POPUL","TPJ"="PIED0",
        "TWF"="ABCO0","TBS"="PIPU0","TLP"="PICO0","TLI"="PIFL2","TPP"="PIPO0",
        "TBC"="PIAR0","TBO"="QUMA2","TAE"="FRPE","TER"="JUVI","TJP"="PIBA2",
        "TJF"="PIJE","TPB"="BEPA0","TSC"="TARA","TSP"="PISY","TWP"="PIST3",
        "TWB"="PIAL0","TWS"="PIGL0","TGO"="QUGA","OTH"="OTHER"
      )
      COVERTYPE_MPSG <- as.character(R2toMPSG[as.character(ctResults$COVERTYPE_R2)])
    }
    if(MPSGcovTyp==3){
      ctResults<-domTypeR3(data = data,
                           stand = stand,
                           species = species,
                           dbh = dbh,
                           crwidth = crwidth,
                           expf = expf,
                           TPA = TPA,
                           CC = CC)
      COVERTYPE_MPSG <- ctResults$DOMTYPE
    }

    #Calculate TREE_SIZE_CLASS
    #Stand-level basal area weighted diameter
    StBAWTDBH <- round(plotvals[["ALL"]]["BA_WT_DIA"][[1]][1],2)
    if(StBAWTDBH < 5.0) TREE_SIZE_CLASS <- "s"
    if(StBAWTDBH >= 5.0 && StBAWTDBH < 10.0) TREE_SIZE_CLASS <- "p"
    if(StBAWTDBH >= 10.0 && StBAWTDBH < 15.0) TREE_SIZE_CLASS <- "m"
    if(StBAWTDBH >= 15.0 && StBAWTDBH < 20.0) TREE_SIZE_CLASS <- "l"
    if(StBAWTDBH >= 20.0) TREE_SIZE_CLASS <- "v"

    #Calculate CROWN_CLASS
    if(STCC < 10) CROWN_CLASS <- "n"
    if(STCC >= 10 && STCC < 40) CROWN_CLASS <- "o"
    if(STCC >= 40 && STCC < 60) CROWN_CLASS <- "m"
    if(STCC >= 60) CROWN_CLASS <- "c"
  }

  #Calculate CANOPY_LAYERS
  VERTICAL_STRUCTURE<-baStory(data = data,
                              stand = stand,
                              dbh = dbh,
                              expf = expf,
                              BA = BA,
                              TPA = TPA,
                              CC = CC)
  if(is.na(VERTICAL_STRUCTURE)) VERTICAL_STRUCTURE<-0
  if(VERTICAL_STRUCTURE!="NONE"){
    if(VERTICAL_STRUCTURE==3) VERTICAL_STRUCTURE <- 2
  }

  #Add habitat structural stage estimates
  if(addHSS) HSS1_4C <- HSS(HSStype = 1,
                 data = data,
                 TPA = TPA,
                 CC = CC,
                 plotvals = plotvals)

  if(addHSS) HSS1_5 <- HSS(HSStype = 2,
                data = data,
                TPA = TPA,
                CC = CC,
                plotvals = plotvals)
  if(addHSS){
    results=list("COVERTYPE_MPSG" = COVERTYPE_MPSG,
                 "TREE_SIZE_CLASS" = TREE_SIZE_CLASS,
                 "CROWN_CLASS" = CROWN_CLASS,
                 "VERTICAL_STRUCTURE" = VERTICAL_STRUCTURE,
                 "HSS1_4C" = HSS1_4C,
                 "HSS1_5" = HSS1_5)
  } else {
    results=list("COVERTYPE_MPSG" = COVERTYPE_MPSG,
                 "TREE_SIZE_CLASS" = TREE_SIZE_CLASS,
                 "CROWN_CLASS" = CROWN_CLASS,
                 "VERTICAL_STRUCTURE" = VERTICAL_STRUCTURE)
  }

  return(results)
}
