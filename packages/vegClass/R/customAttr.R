################################################################################
#Function customAttr
#
#This function provides similar functionality of the SPMCDBH() FVS Event Monitor
#function by returning the following attributes for by species or species group,
#and ranges of diameters and heights.
#
#Trees per acre
#Basal area per acre
#Reineke SDI
#Zeide SDI
#Percent canopy cover
#Quadratic mean diameter
#Basal area weighted diameter
#Average height
#Total cubic foot volume/acre
#Merch cubic foot volume/acre
#Board foot volume/acre
#
#Arguments:
#
#data:       Data frame containing tree records from a single stand or plot.
#            Data frame must contain a column corresponding to stand/plot ID,
#            DBH, height, species, expansion factor, and crown width for each
#            tree record.
#
#region:     Integer variable corresponding to USFS region number (1, 3, or 8 are
#            valid values).
#
#stand:      Character string corresponding to name of column pertaining to
#            stand or plot ID associated with tree records in data argument. By
#            default, this value is set to "StandID".
#
#species:    Character string corresponding to name of column pertaining to USDA
#            plant symbols of tree records in data argument. By default, this
#            argument is set to "SpeciesPLANTS".
#
#dbh:        Character string corresponding to name of column pertaining to DBH
#            of tree records in data argument. By default, this argument is set
#            to "DBH".
#
#ht:         Character string corresponding to name of column pertaining to
#            total tree height of tree records in data argument. By default,
#            this argument is set to "Ht".
#
#crwidth:    Character string corresponding to name of column pertaining to
#            crown width values of tree records in data argument. By default,
#            this argument is set to "CrWidth".
#
#expf:       Character string corresponding to name of column pertaining to TPA
#            of tree records in data argument. By default, this argument is set
#            to "TPA".
#
#vol1:       Character string corresponding to name of column pertaining to
#            total cubic foot (western variants) or merchantable cubic foot
#            volume (eastern variants) of tree records in data argument. By
#            default, this argument is set to "MCuFt".
#
#vol2:       Character string corresponding to name of column pertaining to
#            merchantable cubic foot (western variants) or sawlog cubic foot
#            volume (eastern variants) of tree records in data argument. By
#            default, this argument is set to "SCuFt".
#
#vol3:       Character string corresponding to name of column pertaining to
#            board foot (western variants) or sawlog board foot volume (eastern
#            variants) of tree records in data argument. By default, this
#            argument is set to "SBdFt".
#
#debug:      logical variable indicating if debug statements should be printed.
#            By default this value is set to FALSE (F).
#
#custVars:   Dataframe specifying the arguments for the custom variables. Those
#            arguments include:
#
#            VARIABLE_NAME: name of the custom variable to be returned (required)
#            ATTRIBUTE: variable to be returned (required)
#            SPECIES: 2-letter species code or custom species group name (default=ALL)
#            MIN_DBH: Smallest DBH in inches to be included (default=0.0)
#            MAX_DBH: Largest DBH in inches to be included (default=200.0)
#            MIN_HT:  Shortest tree in feet to be included (default=0.0)
#            MAX_HT:  Tallest tree in feet to be included (default=500.0)
#
#Value
#
#Named list containing custom attributes for each plot/stand.
################################################################################

#'@export
customAttr <- function(data,
                     region = 3,
                     stand = "StandID",
                     species = "SpeciesPLANTS",
                     dbh = "DBH",
                     ht = "Ht",
                     crwidth = "CrWidth",
                     expf = "TPA",
                     vol1 = "MCuFt",
                     vol2 = "SCuFt",
                     vol3 = "SBdFt",
                     vol1DBH = vol1DBH,
                     vol2DBH = vol2DBH,
                     vol3DBH = vol3DBH,
                     customVars = customVars,
                     debug = F)
{
  if(debug)
  {
    cat("In function customAttr", "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "Species", species, "\n",
        "dbh:", dbh, "\n",
        "ht:", ht, "\n",
        "crWidth:", crwidth, "\n",
        "expf:", expf, "\n",
        "vol1:", vol1, "\n",
        "vol2:", vol2, "\n",
        "vol3:", vol3, "\n", "\n")
  }

  #==========================================================================
  #Do checks on customVars argument
  #==========================================================================

  #Test if value in customVars argument is null.
  if (!is.null(customVars)){
    library(readxl)
    custVars <- read_excel(path=customVars,
                 sheet = "Custom Variables", range = cell_cols("A:H"))
  }

  #Initialize output dataframe
  custData <- (as.data.frame(t(custVars$VARIABLE_NAME)))
  names(custData) <- custVars$VARIABLE_NAME
  custData[1,] <- 0

  #If data has no rows, return
  if(nrow(data) <= 0) return(custData)

  #Check for missing columns in data
  missing <- c(dbh, ht, crwidth, expf, stand, species, vol1, vol2, vol3) %in%
    colnames(data)

  #If name of columns provided in stand, dbh, expf, crwidth are not found in
  #data warning message is issued and 0 valued are returned.
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(custData)
  }
  #Coerce the column names of custVars to upper case to avoid casing discrepancies
  colnames(custVars) <- toupper(colnames(custVars))

  #For each variable, perform checks on required arguments: name and attribute.
  #Check if name and/or attribute are missing from the rows in custVars
  if(any(is.na(custVars$VARIABLE_NAME)) || any(is.na(custVars$ATTRIBUTE))){
        cat("One or more of the custom variable arguments are mising the
            required VARIABLE_NAME and/or ATTRIBUTE arguments.", "\n")
    return(custData)
  }

  #Get species group parameters
  specGrps <- read_excel(path=customVars,
                 sheet = "Species Groups", range = cell_cols("A:AP"))
  specGrps <- subset(specGrps,!is.na(Group_Label))
  #Coerce the column names of specGrps to upper case to avoid casing discrepancies
  colnames(specGrps) <- toupper(colnames(specGrps))

  #Create a copy pf the full data
  datafull <- data

  #Loop through each custom variable
  for(cv in 1:nrow(custVars)){
    if(cv>1) data <- datafull
    #Subset tree list by species, DBH, and HT
    ithVar <- as.data.frame(custVars[cv,])
    if(is.na(ithVar$SPECIES))ithVar$SPECIES=="ALL"
    #If species is not ALL or NA, subset data by species/species group
    if(ithVar$SPECIES!="ALL" && !is.na(ithVar$SPECIES)){
      #If SPECIES is actually a species group, get the species codes associated
      #with that species group and then perform the treelist subset
      if(ithVar$SPECIES %in% specGrps$GROUP_LABEL){
        ithSG <- subset(specGrps,GROUP_LABEL==ithVar$SPECIES)
        ithSG <- ithSG[,colSums(is.na(ithSG))==0]
        spCodes <- ithSG[,3:length(ithSG)]
        data <- subset(data,SpeciesPLANTS %in% spCodes)
      }else data <- subset(data,SpeciesPLANTS %in% ithVar$SPECIES)
    }#END SPECIES SUBSET
    #Subset by DBH
    dbhMin <- if(!is.na(ithVar$MIN_DBH)) ithVar$MIN_DBH else 0.0
    dbhMax <- if(!is.na(ithVar$MAX_DBH)) ithVar$MAX_DBH else 200.0
    data <- subset(data,DBH >= dbhMin & DBH < dbhMax)
    #Subset by HT
    htMin <- if(!is.na(ithVar$MIN_HT)) ithVar$MIN_HT else 0.0
    htMax <- if(!is.na(ithVar$MAX_HT)) ithVar$MAX_HT else 200.0
    data <- subset(data,Ht >= htMin & Ht < htMax)
    #TREES PER ACRE
    if(ithVar$ATTRIBUTE=="TREES PER ACRE"){
      #Loop across data and calculate attributes
      TPA <- 0
      if(nrow(data>0)){
        for(i in 1:nrow(data))
        {
          TPA <- TPA + data$TPA[i]
        }
      }
      custData[cv] <- TPA
    }
    #BASAL AREA PER ACRE
    if(ithVar$ATTRIBUTE=="BASAL AREA PER ACRE"){
      #Loop across data and calculate attributes
      BA <- 0
      if(nrow(data>0)){
        for(i in 1:nrow(data))
        {
          BA <- BA + data$TREEBA[i]
        }
      }
      custData[cv] <- BA
    }
    #REINEKE SDI
    if(ithVar$ATTRIBUTE=="REINEKE SDI"){
      #Loop across data and calculate attributes
      TPA <- 0
      DSQ <- 0
      QMD <- 0
      if(nrow(data>0)){
        for(i in 1:nrow(data))
        {
          #Get TPA and QMD for data first
          TPA <- TPA + data$TPA[i]
          DSQ <-  DSQ + data$DBH[i]^2 * data$TPA[i]
        }
      }
      if(TPA > 0)QMD = sqrt(DSQ/TPA)
      RSDI <- TPA*(QMD/10)^1.605
      custData[cv] <- RSDI
    }
    #ZEIDE SDI
    if(ithVar$ATTRIBUTE=="ZEIDE SDI"){
      #Loop across data and calculate attributes
      ZSDI <- 0
      if(nrow(data>0)){
      for(i in 1:nrow(data))
        {
         #Calculate ZSDI
          TZSDI <- (data$TPA[i] * (data$DBH[i]/10)^1.605)
          ZSDI <- ZSDI + TZSDI
        }
      }
      custData[cv] <- ZSDI
    }
    #CANOPY COVER PERCENT
    if(ithVar$ATTRIBUTE=="CANOPY COVER PERCENT"){
      #Loop across data and calculate attributes
      PCC <- 0
      if(nrow(data>0)){
        for(i in 1:nrow(data))
        {
          PCC <- PCC + (pi * (data$CrWidth[i]/2)^2 *(data$TPA[i]/43560) * 100)
        }
      }
      #Correct for overlap
      PCC <- correctCC(PCC)
      custData[cv] <- PCC
    }
    #QUADRATIC MEAN DIAMETER
    if(ithVar$ATTRIBUTE=="QUADRATIC MEAN DIAMETER"){
      #Loop across data and calculate attributes
      QMD <- 0
      DSQ <- 0
      if(nrow(data>0)){
        for(i in 1:nrow(data))
        {
          #Get TPA and QMD for data first
          TPA <- TPA + data$TPA[i]
          DSQ <-  DSQ + data$DBH[i]^2 * data$TPA[i]
        }
      }
      if(TPA > 0)QMD = sqrt(DSQ/TPA)
      custData[cv] <- QMD
    }
    #BASAL AREA WEIGHTED DIAMETER
    if(ithVar$ATTRIBUTE=="BASAL AREA WEIGHTED DIAMETER"){
      #Loop across data and calculate attributes
      BA <- 0
      BAWTD <- 0
      if(nrow(data>0)){
        for(i in 1:nrow(data))
        {
          BA <- BA + data$TREEBA[i]
          BAWTD <- BAWTD +(data$DBH[i]*data$TREEBA[i])
        }
      }
      if(BA > 0)BAWTD <- BAWTD/BA
      custData[cv] <- BAWTD
    }
    #AVERAGE HEIGHT
    if(ithVar$ATTRIBUTE=="AVERAGE HEIGHT"){
      #Loop across data and calculate attributes
      HT <- 0
      AVEHT <- 0
      if(nrow(data>0)){
        for(i in 1:nrow(data))
        {
          HT <- HT + data$Ht[i]
        }
        AVEHT <- HT/nrow(data)
      }
      custData[cv] <- AVEHT
    }
    #TOTAL CUBIC VOLUME
    if(ithVar$ATTRIBUTE=="TOTAL CUBIC VOLUME"){
      #Loop across data and calculate attributes
      TCUFTVOL <- 0
      volume <- volumeCalc(data,
                       vol1 = vol1,
                       vol2 = vol2,
                       vol3 = vol3,
                       vol1DBH = vol1DBH,
                       vol2DBH = vol2DBH,
                       vol3DBH = vol3DBH)
      TCUFTVOL <- volume[["VOL1"]]
      custData[cv] <- TCUFTVOL
    }
    #MERCH CUBIC VOLUME
    if(ithVar$ATTRIBUTE=="MERCH CUBIC VOLUME"){
      #Loop across data and calculate attributes
      MCUFTVOL <- 0
      volume <- volumeCalc(data,
                       vol1 = vol1,
                       vol2 = vol2,
                       vol3 = vol3,
                       vol1DBH = vol1DBH,
                       vol2DBH = vol2DBH,
                       vol3DBH = vol3DBH)
      MCUFTVOL <- volume[["VOL2"]]
      custData[cv] <- MCUFTVOL
    }
    #BOARD FOOT VOLUME
    if(ithVar$ATTRIBUTE=="BOARD FOOT VOLUME"){
      #Loop across data and calculate attributes
      TBFTVOL <- 0
      volume <- volumeCalc(data,
                       vol1 = vol1,
                       vol2 = vol2,
                       vol3 = vol3,
                       vol1DBH = vol1DBH,
                       vol2DBH = vol2DBH,
                       vol3DBH = vol3DBH)
      TBFTVOL <- volume[["VOL3"]]
      custData[cv] <- TBFTVOL
    }
  }#END Loop through each custom variable

  #Return List of attributes
  return(custData)
}
