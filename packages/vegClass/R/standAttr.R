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
#
#Arguments:
#
#data:  Data frame containing tree records from a single stand or plot. Data
#       frame must contain a column corresponding to stand/plot ID, DBH, and
#       expansion factor for each tree record.
#
#stand: Character string corresponding to name of column pertaining to stand or
#       plot ID associated with tree records in data argument. By default, this
#       value is set to "StandID".
#
#dbh:   Character string corresponding to name of column pertaining to DBH of
#       tree records in data argument. By default, this argument is set to
#       "DBH".
#
#expf:  Character string corresponding to name of column pertaining to TPA of
#       tree records in data argument. By default, this argument is set to
#       "TPA".
#
#min:   Minimum diameter to consider in calculation of plot attributes. By
#       default this argument is set to 0.
#
#max:   Maximum diameter to consider in calculation of plot attributes. By
#       default this argument is set to 999.
#
#debug: logical variable indicating if debug statements should be printed. By
#       default this value is set to FALSE.
#
#Value
#
#BA, TPA, QMD, CC, SDI, and BA_WT_DIA of inventory plot/stand
################################################################################

#'@export
plotAttr <- function(data,
                     stand = "StandID",
                     dbh = "DBH",
                     crwidth = "CrWidth",
                     expf = "TPA",
                     min = 0.1,
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
        "crWidth:", crwidth, "\n",
        "expf:", expf, "\n", "\n")
  }

  #Check for missing columns in data
  missing <- c(dbh, crwidth, expf, stand) %in% colnames(data)

  #If name of columns provided in stand, dbh, expf, crwidth are not found in
  #data warning message is issued and NA value is returned.
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Initialize attr vector that will be returned
  attr <- c("BA" = 0,
            "TPA" = 0,
            "QMD" = 0,
            "UNCC" = 0,
            "CC" = 0,
            "ZSDI" = 0,
            "RSDI" = 0,
            "BA_WT_DIA" = 0)

  #Initialize values for BA, TPA, CC, DBHSQ , BAWT, ZSDI (Zeide SDI), and RSDI
  BA = 0
  TPA = 0
  DBHSQ = 0
  CC = 0
  ZSDI = 0
  RSDI = 0
  BAWTD = 0

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

      #Calculate tree contribution to BAWT
      BAWTD = BAWTD + data[[dbh]][i] * TREEBA

      #Update TPA
      attr["TPA"] <- attr["TPA"] + data[[expf]][i]

      #Update BA
      attr["BA"] <- attr["BA"] + TREEBA

      #Update CC
      attr["UNCC"] <- attr["UNCC"] + TREECC

      #Update ZSDI
      attr["ZSDI"] <- attr["ZSDI"] + (data[[expf]][i] * (data[[dbh]][i]/10)^1.605)

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

  #Calculate BA weighted diameter if BA is not 0
  if(attr["BA"] > 0)
  {
    attr["BA_WT_DIA"] <- BAWTD/attr["BA"]
  }

  #Calculate RSDI
  attr["RSDI"] = attr["TPA"] * (attr["QMD"]/10)^1.605

  #Calculate corrected canopy cover
  attr["CC"] <- correctCC(attr["UNCC"])

  #Print stand and values in attribute
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
