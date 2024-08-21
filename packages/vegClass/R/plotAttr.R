################################################################################
#Function plotAttr
#
#This function calculates the following attributes for each species in
#plot/stand:
#
#Number of records in plot/stand (N)
#Basal area per acre (BA)
#Trees per acre (TPA)
#QMD (QMD)
#Percent canopy cover uncorrected for overlap (UNCC)
#Percent canopy cover corrected for overlap (CC)
#Zeide SDI (ZSDI)
#Reineke SDI (RSDI)
#Basal area weighted diameter (BA_WT_DIA)
#Basal area weighted height (BA_WT_HT)
#
#If region argument in function is 1 (USFS R1) the following value is
#calculated:
#
#Average height (AVE_HT)
#
#If region argument in function is 8 or 9 (USFS R8/R9) the following values are
#calculated:
#
#TPA weighted average height of advanced regeneration size class (SSSIZE)
#Trees per acre of advanced regeneration size class (SSTPA)
#Basal area of advanced regeneration size class (SSBA)
#Basal area weighted diameter of non merchantable size class (NMSIZE)
#Basal area of non merchantable size class (NMBA)
#Basal area weighted diameter of pulpwood size class (PWSIZE)
#Basal area of pulpwood size class (PWBA)
#Basal area weighted diameter of saw timber size class (STSIZE)
#Basal area of saw timber size class (STBA)
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
#            default, this argument is set to "MCuFt". Currently vol1 is only
#            used if region argument is set to 8.
#
#vol2:       Character string corresponding to name of column pertaining to
#            merchantable cubic foot (western variants) or sawlog cubic foot
#            volume (eastern variants) of tree records in data argument. By
#            default, this argument is set to "SCuFt". Currently vol2 is only
#            used if region argument is set to 8.
#
#vol3:       Character string corresponding to name of column pertaining to
#            board foot (western variants) or sawlog board foot volume (eastern
#            variants) of tree records in data argument. By default, this
#            argument is set to "SBdFt". Currently vol3 is only used if region
#            argument is set to 8.
#
#min:        Minimum diameter to consider in calculation of plot attributes. By
#            default this argument is set to 0.
#
#max:        Maximum diameter to consider in calculation of plot attributes. By
#            default this argument is set to 999.
#
#debug:      logical variable indicating if debug statements should be printed.
#            By default this value is set to FALSE (F).
#
#Value
#
#Named list containing plot attributes for each species in plot/stand.
################################################################################

#'@export
plotAttr <- function(data,
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
                     min = 0,
                     max = 999,
                     debug = F)
{
  if(debug)
  {
    cat("In function plotAttr", "\n")
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

  #Default attr list that will be returned if error is thrown.
  attrList <- list("ALL" = c("N" = NA,
                                "BA" = NA,
                                "TPA" = NA,
                                "QMD" = NA,
                                "UNCC" = NA,
                                "CC" = NA,
                                "ZSDI" = NA,
                                "RSDI" = NA,
                                "BA_WT_DIA" = NA,
                                "BA_WT_HT" = NA,
                                "AVE_HT" = NA,
                                "SSSIZE" = NA,
                                "SSTPA" = NA,
                                "SSBA" = NA,
                                "NMSIZE" = NA,
                                "NMBA" = NA,
                                "PWSIZE" = NA,
                                "PWBA" = NA,
                                "STSIZE" = NA,
                                "STBA" = NA))

  #If data has no rows, return
  if(nrow(data) <= 0) return(attrList)

  #Check for missing columns in data
  missing <- c(dbh, ht, crwidth, expf, stand, species, vol1, vol2, vol3) %in%
    colnames(data)

  #If name of columns provided in stand, dbh, expf, crwidth are not found in
  #data warning message is issued and 0 valued are returned.
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(attrList)
  }

  #Define vector (spStand) of unique species found in data. Also include an
  #all species category.
  spStand <- c(unique(data[[species]]), "ALL")

  #Create list that will house plot attributes for each species in data
  attrList <- vector(mode = "list", length = length(spStand))

  #Loop across attrList and store initialized plot attributes for each species
  for(i in 1:length(spStand))
  {
    #Extract species
    sp <- spStand[i]

    #Create name for species in list
    names(attrList)[i] <- sp

    #Assign vector for species sp
    attr <- c("N" = 0,
              "BA" = 0,
              "TPA" = 0,
              "QMD" = 0,
              "UNCC" = 0,
              "CC" = 0,
              "ZSDI" = 0,
              "RSDI" = 0,
              "BA_WT_DIA" = 0,
              "BA_WT_HT" = 0,
              "AVE_HT" = 0,
              "SSSIZE" = 0,
              "SSTPA" = 0,
              "SSBA" = 0,
              "NMSIZE" = 0,
              "NMBA" = 0,
              "PWSIZE" = 0,
              "PWBA" = 0,
              "STSIZE" = 0,
              "STBA" = 0)

    #Store attr in attrList
    attrList[[i]] <- attr
  }

  #Initialize values for BA, TPA, CC, DBHSQ ,ZSDI (Zeide SDI), and RSDI (Reineke
  #SDI), BAWTD, and BAWTH
  TBA = 0
  TEXPF = 0
  DBHSQ = 0
  TCC = 0
  TZSDI = 0
  BAWTD = 0
  BAWTH = 0
  N = 0

  #Loop across data and calculate attributes
  for(i in 1:nrow(data))
  {
    #If DBH of record is GE min DBH and less than max include it in calculations
    if(data[[dbh]][i] >= min & data[[dbh]][i] < max)
    {
      #Get species for tree i
      sp <- data[[species]][i]

      #Extract expansion factor of tree i
      TEXPF <- data[[expf]][i]

      #Extract DBH of tree i
      DBH <- data[[dbh]][i]

      #Extract HT of tree i
      HT <- data[[ht]][i]

      #Calculate BA of tree
      TBA <- DBH^2 * TEXPF * 0.0054542

      #Calculate CC of tree
      TCC <- pi * (data[[crwidth]][i]/2)^2 *(TEXPF/43560) * 100

      #Calculate tree contribution to QMD
      DBHSQ <- DBH^2 * TEXPF

      #Calculate tree contribution to BAWTD
      BAWTD = DBH * TBA

      #Calculate tree contribution to BAWTH
      BAWTH = HT * TBA

      #Calculate tree contribution of ZSDI
      TZSDI <- (TEXPF * (DBH/10)^1.605)

      #Update number of trees processed
      N <- N + 1

      #=========================================================================
      #Update values for ALL code
      #=========================================================================

      #Update N
      attrList[["ALL"]]["N"] <- attrList[["ALL"]]["N"] + 1

      #Update TPA
      attrList[["ALL"]]["TPA"] <- attrList[["ALL"]]["TPA"] + TEXPF

      #Update BA
      attrList[["ALL"]]["BA"] <- attrList[["ALL"]]["BA"] + TBA

      #Update QMD (DBHSQ values for now)
      attrList[["ALL"]]["QMD"] <- attrList[["ALL"]]["QMD"] + DBHSQ

      #Update CC
      attrList[["ALL"]]["UNCC"] <- attrList[["ALL"]]["UNCC"] + TCC

      #Update BA_WT_DIA
      attrList[["ALL"]]["BA_WT_DIA"] <- attrList[["ALL"]]["BA_WT_DIA"] + BAWTD

      #Update BA_WT_HT
      attrList[["ALL"]]["BA_WT_HT"] <- attrList[["ALL"]]["BA_WT_HT"] + BAWTH

      #Update ZSDI
      attrList[["ALL"]]["ZSDI"] <- attrList[["ALL"]]["ZSDI"] + TZSDI

      #=========================================================================
      #Update values for individual species
      #=========================================================================

      #Update N
      attrList[[sp]]["N"] <- attrList[[sp]]["N"] + 1

      #Update TPA
      attrList[[sp]]["TPA"] <- attrList[[sp]]["TPA"] + TEXPF

      #Update BA
      attrList[[sp]]["BA"] <- attrList[[sp]]["BA"] + TBA

      #Update QMD (DBHSQ values for now)
      attrList[[sp]]["QMD"] <- attrList[[sp]]["QMD"] + DBHSQ

      #Update CC
      attrList[[sp]]["UNCC"] <- attrList[[sp]]["UNCC"] + TCC

      #Update BA_WT_DIA
      attrList[[sp]]["BA_WT_DIA"] <- attrList[[sp]]["BA_WT_DIA"] + BAWTD

      #Update BA_WT_HT
      attrList[[sp]]["BA_WT_HT"] <- attrList[[sp]]["BA_WT_HT"] + BAWTH

      #Update ZSDI
      attrList[[sp]]["ZSDI"] <- attrList[[sp]]["ZSDI"] + TZSDI

      #=========================================================================
      #If region is 1, then update R1 specific variables
      #=========================================================================

      if(region==1)
      {
        #Update AVE_HT for both individual species and All code
        attrList[[sp]]["AVE_HT"] <- attrList[[sp]]["AVE_HT"] + HT*TEXPF
        attrList[["ALL"]]["AVE_HT"] <- attrList[["ALL"]]["AVE_HT"] + HT*TEXPF
      }

      #=========================================================================
      #If region is 8 or 9, then update R8/R9 specific variables
      #=========================================================================

      if(region %in% c(8,9))
      {
        #If tree is advanced regeneration
        if(DBH < 1.5)
        {
          #Update SSTPA
          attrList[[sp]]["SSTPA"] <- attrList[[sp]]["SSTPA"] + TEXPF

          #Update SSBA
          attrList[[sp]]["SSBA"] <- attrList[[sp]]["SSBA"] + TBA

          #Update SSSIZE (lorey height)
          attrList[[sp]]["SSSIZE"] <- attrList[[sp]]["SSSIZE"] + HT*TEXPF

          #Update SSTPA
          attrList[["ALL"]]["SSTPA"] <- attrList[["ALL"]]["SSTPA"] + TEXPF

          #Update SSBA
          attrList[["ALL"]]["SSBA"] <- attrList[["ALL"]]["SSBA"] + TBA

          #Update SSSIZE (lorey height)
          attrList[["ALL"]]["SSSIZE"] <- attrList[["ALL"]]["SSSIZE"] + HT*TEXPF
        }

        #If the tree is non merch
        else if(DBH >= 1.5 & data[[vol1]][i] <= 0)
        {
          #Update NMBA
          attrList[[sp]]["NMBA"] <- attrList[[sp]]["NMBA"] + TBA

          #Update NMSIZE (ba weighted DBH)
          attrList[[sp]]["NMSIZE"] <- attrList[[sp]]["NMSIZE"] + BAWTD

          #Update NMBA
          attrList[["ALL"]]["NMBA"] <- attrList[["ALL"]]["NMBA"] + TBA

          #Update NMSIZE (ba weighted DBH)
          attrList[["ALL"]]["NMSIZE"] <- attrList[["ALL"]]["NMSIZE"] + BAWTD
        }

        #If the tree pulpwood
        else if(data[[vol1]][i] > 0 & data[[vol2]][i] <= 0)
        {
          #Update PWTPA
          attrList[[sp]]["PWBA"] <- attrList[[sp]]["PWBA"] + TBA

          #Update PWSIZE (ba weighted DBH)
          attrList[[sp]]["PWSIZE"] <- attrList[[sp]]["PWSIZE"] + BAWTD

          #Update PWTPA
          attrList[["ALL"]]["PWBA"] <- attrList[["ALL"]]["PWBA"] + TBA

          #Update PWSIZE (ba weighted DBH)
          attrList[["ALL"]]["PWSIZE"] <- attrList[["ALL"]]["PWSIZE"] + BAWTD
        }

        #If the tree is saw timber
        else if(data[[vol3]][i] > 0)
        {
          #Update PWTPA
          attrList[[sp]]["STBA"] <- attrList[[sp]]["STBA"] + TBA

          #Update PWSIZE (ba weighted DBH)
          attrList[[sp]]["STSIZE"] <- attrList[[sp]]["STSIZE"] + BAWTD

          #Update PWTPA
          attrList[["ALL"]]["STBA"] <- attrList[["ALL"]]["STBA"] + TBA

          #Update PWSIZE (ba weighted DBH)
          attrList[["ALL"]]["STSIZE"] <- attrList[["ALL"]]["STSIZE"] + BAWTD
        }
      }

      if(debug)
      {
        cat("TREE:", N, "\n",
            "SPECIES:", sp, "\n",
            "TREE DBH:", DBH, "\n",
            "TREE EXP:", TEXPF, "\n",
            "TREECC:", TCC, "\n",
            "TREEBA:", TBA, "\n",
            "DBHSQ:", DBHSQ, "\n",
            "BAWTD:", BAWTD, "\n",
            "BAWTH:", BAWTH, "\n",
            "TZSDI:", TZSDI, "\n", "\n")
      }
    }
  }

  #Loop across attrList and finalize values
  for(i in 1:length(attrList))
  {
    #Now calculate QMD if TPA is not 0
    if(attrList[[i]]["TPA"] > 0)
    {
      attrList[[i]]["QMD"] = sqrt(attrList[[i]]["QMD"]/attrList[[i]]["TPA"])
    }

    #Calculate BA weighted diameter and height if BA is not 0
    if(attrList[[i]]["BA"] > 0)
    {
      attrList[[i]]["BA_WT_DIA"] <- attrList[[i]]["BA_WT_DIA"]/
        attrList[[i]]["BA"]

      attrList[[i]]["BA_WT_HT"] <- attrList[[i]]["BA_WT_HT"]/
        attrList[[i]]["BA"]
    }

    #Calculate average height if TPA is not 0
    if(attrList[[i]]["TPA"] > 0)
    {
      attrList[[i]]["AVE_HT"] = sqrt(attrList[[i]]["AVE_HT"]/attrList[[i]]["TPA"])
    }

    #Calculate advance regen size if SSTPA is greater than 0
    if(attrList[[i]]["SSTPA"] > 0)
    {
      attrList[[i]]["SSSIZE"] <- attrList[[i]]["SSSIZE"]/attrList[[i]]["SSTPA"]
    }

    #Calculate non-merch size if BA is greater than 0
    if(attrList[[i]]["NMBA"] > 0)
    {
      attrList[[i]]["NMSIZE"] <- attrList[[i]]["NMSIZE"]/
        attrList[[i]]["NMBA"]
    }

    #Calculate pulp size if BA is greater than 0
    if(attrList[[i]]["PWBA"] > 0)
    {
      attrList[[i]]["PWSIZE"] <- attrList[[i]]["PWSIZE"]/
        attrList[[i]]["PWBA"]
    }

    #Calculate saw timber size if BA is greater than 0
    if(attrList[[i]]["STBA"] > 0)
    {
      attrList[[i]]["STSIZE"] <- attrList[[i]]["STSIZE"]/
        attrList[[i]]["STBA"]
    }

    #Calculate RSDI
    attrList[[i]]["RSDI"] = attrList[[i]]["TPA"] *
      (attrList[[i]]["QMD"]/10)^1.605

    #Calculate corrected canopy cover
    attrList[[i]]["CC"] <- correctCC(attrList[[i]]["UNCC"])

    #Print stand and values in attr
    if(debug)
    {
      cat("Stand:", unique(data[[stand]]), "\n")
      cat("Species", names(attrList)[i], "\n")
      cat("N:", attrList[[i]]["N"], "\n")
      cat("BA:", attrList[[i]]["BA"], "\n")
      cat("TPA:", attrList[[i]]["TPA"], "\n")
      cat("QMD:", attrList[[i]]["QMD"], "\n")
      cat("UNCC:", attrList[[i]]["UNCC"], "\n")
      cat("CC:", attrList[[i]]["CC"], "\n")
      cat("ZSDI:", attrList[[i]]["ZSDI"], "\n")
      cat("RSDI:", attrList[[i]]["RSDI"], "\n")
      cat("AVE_HT:", attrList[[i]]["AVE_HT"], "\n")
      cat("SSTPA:", attrList[[i]]["SSTPA"], "\n")
      cat("SSBA:", attrList[[i]]["SSTPA"], "\n")
      cat("SSSIZE:", attrList[[i]]["SSSIZE"],"\n")
      cat("NMBA:", attrList[[i]]["NMBA"], "\n")
      cat("NMSIZE:", attrList[[i]]["NMSIZE"],"\n")
      cat("PWBA:", attrList[[i]]["PWBA"], "\n")
      cat("PWSIZE:", attrList[[i]]["PWSIZE"],"\n")
      cat("STBA:", attrList[[i]]["STBA"], "\n")
      cat("STSIZE:", attrList[[i]]["STSIZE"],"\n", "\n")
    }
  }

  #Return List of attributes
  return(attrList)
}

################################################################################
#Function: correctCC
#
#This function takes in an uncorrected percent canopy cover value and returns
#a corrected value using the relationship described on page 2 of Crookston,
#Nicholas L.; Stage, Albert R. 1999. Percent canopy cover and stand structure
#statistics from the Forest Vegetation Simulator. Gen. Tech. Rep. RMRS-GTR-24.
#Ogden, UT: U. S. Department of Agriculture, Forest Service, Rocky Mountain
#Research Station. 11 p.
#
#Argument
#
#CC: Uncorrected CC value
#
#Return value
#
#Corrected CC value.
################################################################################

#'@export
correctCC<-function(CC)
{
  corCC = 100 * (1 - exp ( - 0.01* CC))
  return(corCC)
}

################################################################################
#Function: volumeCalc
#
#By default this function calculates volume for stand/plot for three different
#pools found in FVS_TreeList or FVS_TreeList_East.
#
#Arguments:
#
#data:    Data frame containing tree records from a single stand or plot. Data
#         frame must contain a column corresponding to stand/plot ID, DBH,
#         expansion factor, and volume for each tree record.
#
#stand:   Character string corresponding to name of column pertaining to stand
#         or plot ID associated with tree records in data argument. By default,
#         this value is set to "StandID".
#
#dbh:     Character string corresponding to name of column pertaining to DBH of
#         tree records in data argument. By default, this argument is set to
#         "DBH".
#
#expf:    Character string corresponding to name of column pertaining to TPA of
#         tree records in data argument. By default, this argument is set to
#         "TPA".
#
#expfM:   Character string corresponding to name of column pertaining to TPA of
#         tree records that died during a cycle in argument. By default, this
#         argument is set to "MortPA".
#
#vol1:    Character string corresponding to name of column pertaining to total
#         cubic foot volume of tree records in data argument. By default, this
#         argument is set to "TCuFt".
#
#vol2:    Character string corresponding to name of column pertaining to
#         merchantable cubic foot volume of tree records in data argument. By
#         default, this argument is set to "MCuFt".
#
#vol3:    Character string corresponding to name of column pertaining to
#         board foot volume of tree records in data argument. By default, this
#         argument is set to "BdFt".
#
#vol1DBH: Minimum DBH of tree records included in calculation of vol1. By
#         default this argument is set to 0.
#
#vol2DBH: Minimum DBH of tree records included in calculation of vol2. By
#         default this argument is set to 5.
#
#vol3DBH: Minimum DBH of tree records included in calculation of vol3. By
#         default this argument is set to 9.

#debug:   logical variable indicating if debug statements should be printed. By
#         default this value is set to FALSE (F).
#
#Value
#
#VOL1:    Total cubic foot volume (western regions) / merchantable cubic foot
#         volume (eastern regions)
#
#VOL2:    Merchantable cubic foot volume (western regions) / sawlog cubic
#         foot volume (eastern regions)
#
#VOL3:    Merchantable board foot volume (western regions) / sawlog board
#         foot volume (eastern regions)
#
#VOL4:    Total cubic foot volume (western regions) / merchantable cubic foot
#         volume (eastern regions) that died during cycle.
#
#VOL5:    Merchantable cubic foot volume (western regions) / sawlog cubic
#         foot volume (eastern regions) that died during cycle.
#
#VOL6:    Merchantable board foot volume (western regions) / sawlog board
#         foot volume (eastern regions) that died during cycle.
################################################################################

#'@export
volumeCalc <- function(data,
                       stand = "StandID",
                       dbh = "DBH",
                       expf = "TPA",
                       expfM = "MortPA",
                       vol1 = "TCuFt",
                       vol2 = "MCuFt",
                       vol3 = "BdFt",
                       vol1DBH = 0,
                       vol2DBH = 5,
                       vol3DBH = 9,
                       debug = F)
{
  if(debug)
  {
    cat("In function volumeCalc", "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "expf:", expf, "\n",
        "expfM:", expfM, "\n",
        "vol1:", vol1, "\n",
        "vol2:", vol2, "\n",
        "vol3:", vol3, "\n",
        "vol1DBH:", vol1DBH, "\n",
        "vol2DBH:", vol2DBH, "\n",
        "vol3DBH:", vol3DBH, "\n", "\n")
  }

  #Initialize volume vector that will be returned
  volume <- c("VOL1" = 0,
              "VOL2" = 0,
              "VOL3" = 0,
              "VOL4" = 0,
              "VOL5" = 0,
              "VOL6" = 0)

  #If data has no rows, return
  if(nrow(data) <= 0) return(volume)

  #Check for missing columns in data
  missing <- c(dbh, expf, expfM, stand, vol1, vol2, vol3) %in% colnames(data)

  #If name of columns provided in stand, dbh, expf, vol1 - vol3 are not found in
  #data warning message is issued and 0 values are returned.
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(volume)
  }

  #Loop across data and calculate volumes
  for(i in 1:nrow(data))
  {

    #Accumulate volume 1 values
    if(data[[dbh]][i] >= vol1DBH)
    {
      volume["VOL1"] <- volume["VOL1"] + data[[vol1]][i] * data[[expf]][i]
    }

    #Accumulate volume 2 values
    if(data[[dbh]][i] >= vol2DBH)
    {
      volume["VOL2"] <- volume["VOL2"] + data[[vol2]][i] * data[[expf]][i]
    }

    #Accumulate volume 3 values
    if(data[[dbh]][i] >= vol3DBH)
    {
      volume["VOL3"] <- volume["VOL3"] + data[[vol3]][i] * data[[expf]][i]
    }

    #Accumulate volume 5 values
    if(data[[dbh]][i] >= vol1DBH)
    {
      volume["VOL4"] <- volume["VOL4"] + data[[vol1]][i] * data[[expfM]][i]
    }

    #Accumulate volume 5 values
    if(data[[dbh]][i] >= vol2DBH)
    {
      volume["VOL5"] <- volume["VOL5"] + data[[vol2]][i] * data[[expfM]][i]
    }

    #Accumulate volume 6 values
    if(data[[dbh]][i] >= vol3DBH)
    {
      volume["VOL6"] <- volume["VOL6"] + data[[vol3]][i] * data[[expfM]][i]
    }
  }

  #Print stand and values in volume
  if(debug)
  {
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("VOL1:", volume["VOL1"], "\n")
    cat("VOL2:", volume["VOL2"], "\n")
    cat("VOL3:", volume["VOL3"], "\n")
    cat("VOL4:", volume["VOL4"], "\n")
    cat("VOL5:", volume["VOL5"], "\n")
    cat("VOL6:", volume["VOL6"], "\n","\n")
  }

  #Return volume
  return(volume)
}
