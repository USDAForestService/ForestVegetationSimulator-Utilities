################################################################################
#Function plotAttr
#
#This function calculates the following attributes for plot/stand:
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
#Arguments:
#
#data:    Data frame containing tree records from a single stand or plot. Data
#         frame must contain a column corresponding to stand/plot ID, DBH,
#         expansion factor, and crown width for each tree record.
#
#stand:   Character string corresponding to name of column pertaining to stand
#         or plot ID associated with tree records in data argument. By default,
#         this value is set to "StandID".
#
#dbh:     Character string corresponding to name of column pertaining to DBH of
#         tree records in data argument. By default, this argument is set to
#         "DBH".
#
#ht:      Character string corresponding to name of column pertaining to total
#         tree height of tree records in data argument. By default, this
#         argument is set to "Ht".
#
#expf:    Character string corresponding to name of column pertaining to TPA of
#         tree records in data argument. By default, this argument is set to
#         "TPA".
#
#crwidth: Character string corresponding to name of column pertaining to crown
#         width values of tree records in data argument. By default, this
#         argument is set to "CrWidth".
#
#min:     Minimum diameter to consider in calculation of plot attributes. By
#         default this argument is set to 0.
#
#max:     Maximum diameter to consider in calculation of plot attributes. By
#         default this argument is set to 999.
#
#debug:   logical variable indicating if debug statements should be printed. By
#         default this value is set to FALSE.
#
#Value
#
#BA, TPA, QMD, CC, SDI, BA_WT_DIA, BA_WT_HT of inventory plot/stand
################################################################################

#'@export
plotAttr <- function(data,
                     stand = "StandID",
                     dbh = "DBH",
                     ht = "Ht",
                     crwidth = "CrWidth",
                     expf = "TPA",
                     min = 0,
                     max = 999,
                     debug = F,
                     type = 1)
{
  if(debug)
  {
    cat("In function plotAttr", "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "ht:", ht, "\n",
        "crWidth:", crwidth, "\n",
        "expf:", expf, "\n", "\n")
  }

  #Initialize attr vector that will be returned
  attr <- c("BA" = 0,
            "TPA" = 0,
            "QMD" = 0,
            "UNCC" = 0,
            "CC" = 0,
            "ZSDI" = 0,
            "RSDI" = 0,
            "BA_WT_DIA" = 0,
            "BA_WT_HT" = 0)

  #Check for missing columns in data
  missing <- c(dbh, ht, crwidth, expf, stand) %in% colnames(data)

  #If name of columns provided in stand, dbh, expf, crwidth are not found in
  #data warning message is issued and 0 valued are returned.
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(attr)
  }

  #Initialize values for BA, TPA, CC, DBHSQ ,ZSDI (Zeide SDI), and RSDI (Reineke
  #SDI), BAWTD, and BAWTH
  BA = 0
  TPA = 0
  DBHSQ = 0
  CC = 0
  ZSDI = 0
  RSDI = 0
  BAWTD = 0
  BAWTH = 0

  #Loop across data and calculate attributes
  for(i in 1:nrow(data))
  {
    #If DBH of record is GE min DBH and less than max include it in calculations
    if(data[[dbh]][i] >= min & data[[dbh]][i] < max)
    {
      #Calculate BA of tree
      TREEBA <- data[[dbh]][i]^2 * data[[expf]][i] * 0.005454

      #Calculate CC of tree
      TREECC <- pi * (data[[crwidth]][i]/2)^2 *(data[[expf]][i]/43560) * 100

      #Calculate tree contribution to QMD
      DBHSQ <- DBHSQ + data[[dbh]][i]^2 * data[[expf]][i]

      #Calculate tree contribution to BAWTD
      BAWTD = BAWTD + data[[dbh]][i] * TREEBA

      #Calculate tree contribution to BAWTH
      BAWTH = BAWTH + data[[ht]][i] * TREEBA

      #Update TPA
      attr["TPA"] <- attr["TPA"] + data[[expf]][i]

      #Update BA
      attr["BA"] <- attr["BA"] + TREEBA

      #Update CC
      attr["UNCC"] <- attr["UNCC"] + TREECC

      #Update ZSDI
      attr["ZSDI"] <- attr["ZSDI"] + (data[[expf]][i] *
                                        (data[[dbh]][i]/10)^1.605)

      if(debug)
      {
        cat("TREE DBH:", data[[dbh]][i], "\n",
            "TREE EXP:", data[[expf]][i], "\n",
            "TREECC:", TREECC, "\n",
            "TREEBA:", TREEBA, "\n",
            "TPA:", attr["TPA"], "\n",
            "BA:", attr["BA"], "\n",
            "DBHSQ:", DBHSQ, "\n",
            "BAWTD:", BAWTD, "\n",
            "UNCC:", attr["CC"], "\n",
            "ZSDI:", attr["ZSDI"], "\n", "\n")
      }
    }
  }

  #Now calculate QMD if TPA is not 0
  if(attr["TPA"] > 0)
  {
    attr["QMD"] = sqrt(DBHSQ/attr["TPA"])
  }

  #Calculate BA weighted diameter and height if BA is not 0
  if(attr["BA"] > 0)
  {
    attr["BA_WT_DIA"] <- BAWTD/attr["BA"]
    attr["BA_WT_HT"] <- BAWTH/attr["BA"]
  }

  #Calculate RSDI
  attr["RSDI"] = attr["TPA"] * (attr["QMD"]/10)^1.605

  #Calculate corrected canopy cover
  attr["CC"] <- correctCC(attr["UNCC"])

  #Print stand and values in attr
  if(debug)
  {
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("BA:", attr["BA"], "\n")
    cat("TPA:", attr["TPA"], "\n")
    cat("QMD:", attr["QMD"], "\n")
    cat("UNCC:", attr["UNCC"], "\n")
    cat("CC:", attr["CC"], "\n")
    cat("ZSDI:", attr["ZSDI"], "\n")
    cat("RSDI:", attr["RSDI"], "\n")
    cat("BA_WT_DIA:", attr["BA_WT_DIA"], "\n", "\n")
  }

  #Return vector of attributes
  return(attr)
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
    #If DBH of record is GE vol1DBH, add vol1 * expf to VOL1 in volume vector
    if(data[[dbh]][i] >= vol1DBH)
    {
      #Accumulate volume 1 values
      volume["VOL1"] <- volume["VOL1"] + data[[vol1]][i] * data[[expf]][i]
    }

    #If DBH of record is GE vol2DBH, add vol2 * expf to VOL2 in volume vector
    if(data[[dbh]][i] >= vol2DBH)
    {
      #Accumulate volume 2 values
      volume["VOL2"] <- volume["VOL2"] + data[[vol2]][i] * data[[expf]][i]
    }

    #If DBH of record is GE vol3DBH, add vol3 * expf to VOL3 in volume vector
    if(data[[dbh]][i] >= vol3DBH)
    {
      #Accumulate volume 3 values
      volume["VOL3"] <- volume["VOL3"] + data[[vol3]][i] * data[[expf]][i]
    }

    #If DBH of record is GE vol1DBH, add vol1 * expfM to VOL4 in volume vector
    if(data[[dbh]][i] >= vol1DBH)
    {
      #Accumulate volume 1 values
      volume["VOL4"] <- volume["VOL4"] + data[[vol1]][i] * data[[expfM]][i]
    }

    #If DBH of record is GE vol2DBH, add vol2 * expfM to VOL5 in volume vector
    if(data[[dbh]][i] >= vol2DBH)
    {
      #Accumulate volume 2 values
      volume["VOL5"] <- volume["VOL5"] + data[[vol2]][i] * data[[expfM]][i]
    }

    #If DBH of record is GE vol3DBH, add vol3 * expfM to VOL6 in volume vector
    if(data[[dbh]][i] >= vol3DBH)
    {
      #Accumulate volume 3 values
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
