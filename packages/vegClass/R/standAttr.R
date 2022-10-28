################################################################################
#Function plotBA
#
#This function calculates basal area per acre of plot/stand.
#
#Arguments:
#
#data:  Tree level dataframe corresponding to trees from a single stand.
#
#stand: Name of column corresponding to stand ID associated with tree records
#       in data. By default this value is set to "StandID".
#
#dbh:   Name of column in data argument corresponding to DBH of tree records. By
#       default this argument is set to "DBH".
#
#expf:  Name of column in data argument corresponding to TPA of tree records.
#       By default this argument is set to "TPA".
#
#min:   Minimum diameter to consider in calculation of BA. By default this
#       argument is set to 0.
#
#min:   Maximum diameter to consider in calculation of BA. By default this
#       argument is set to 999.
#
#debug: logical variable indicating if debug statements should be printed. By
#       default this value is set to FALSE.
#
#Value
#
#BA of inventory plot/stand
################################################################################

#'@export
plotBA <- function(data,
                   stand = "StandID",
                   dbh = "DBH",
                   expf = "TPA",
                   min = 0.1,
                   max = 999,
                   debug = F)
{
  if(debug)
  {
    cat("In function plotBA", "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "expf:", expf, "\n", "\n")
  }

  #Check of missing columns in data
  missing <- c(dbh, expf, stand) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Initialize BA
  BA = 0

  #Loop across data and calculate BA
  for(i in 1:nrow(data))
  {
    #If DBH of record is GE min DBH and less than max add it to BA sum
    if(data[[dbh]][i] >= min & data[[dbh]][i] < max)
    {
      TREEBA = data[[dbh]][i]^2 * data[[expf]][i] * 0.005454
      BA = BA + TREEBA

      if(debug){
        cat("TREE DBH:", data[[dbh]][i], "\n",
            "TREE EXP:", data[[expf]][i], "\n",
            "TREEBA:", TREEBA, "\n",
            "BA:", BA, "\n", "\n")
      }
    }
  }

  #Print stand and BA if debug is true.
  if(debug)
  {
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("BA:", BA, "\n", "\n")
  }

  #Return BA
  return(BA)
}

################################################################################
#Function plotTPA
#
#This function calculates trees per acre of plot/stand.
#
#Arguments:
#
#data:  Tree level dataframe corresponding to trees from a single stand.
#
#stand: Name of column corresponding to stand ID associated with tree records
#       in data. By default this value is set to "StandID".
#
#expf:  Name of column in data argument corresponding to TPA of tree records.
#       By default this argument is set to "TPA".
#
#min:   Minimum diameter to consider in calculation of TPA. By default this
#       argument is set to 0.
#
#min:   Maximum diameter to consider in calculation of TPA. By default this
#       argument is set to 999.
#
#debug: logical variable indicating if debug statements should be printed. By
#       default this value is set to FALSE.
#
#Value
#
#TPA of inventory plot/stand
################################################################################

#'@export
plotTPA <- function(data,
                    stand = "StandID",
                    dbh = "DBH",
                    expf = "TPA",
                    min = 0.1,
                    max = 999,
                    debug = F)
{
  if(debug)
  {
    cat("In function plotTPA", "\n")
    cat("Columns:", "\n",
      "Stand:", stand, "\n",
      "dbh:", dbh, "\n",
      "expf:", expf, "\n", "\n")
  }

  #Check of missing columns in data
  missing <- c(expf, stand) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Initialize TPA
  TPA = 0

  #Loop across data and calculate TPA
  for(i in 1:nrow(data))
  {
    #If DBH of record is GE min DBH and less than max add it to TPA sum
    if(data[[dbh]][i] >= min & data[[dbh]][i] < max)
    {
      TPA = TPA + data[[expf]][i]

      if(debug){
        cat("TREE DBH:", data[[dbh]][i], "\n",
            "TREE EXP:", data[[expf]][i], "\n",
            "TPA:", TPA, "\n", "\n")
      }
    }
  }

  #Print stand and TPA if debug is true.
  if(debug)
  {
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("TPA:", TPA, "\n", "\n")
  }

  #Return TPA
  return(TPA)
}

################################################################################
#Function plotCC
#
#This function calculates percent canopy cover (CC) of plot/stand.
#
#Arguments:
#
#data:    Tree level dataframe corresponding to trees from a single stand.
#
#stand:   Name of column corresponding to stand associated with tree records
#         in data. By default this value is set to "StandID".
#
#crwidth: Name of column in data argument corresponding to crown width of tree
#         records.By default this argument is set to "TPA".
#
#expf:    Name of column in data argument corresponding to expansion factor of
#         tree records By default this argument is set to "TPA".
#
#min:     Minimum diameter to consider in calculation of CC. By default this
#         argument is set to 0.
#
#min:     Maximum diameter to consider in calculation of CC. By default this
#         argument is set to 999.
#
#debug:   logical variable indicating if debug statements should be printed. By
#         default this value is set to F.
#
#type:    Integer value representing type of percent canopy cover to return.
#         1 = not corrected for overlap
#         2 = corrected for overlap
#
#Value
#
#Percent canopy cover of inventory plot/stand
################################################################################

#'@export
plotCC <- function(data,
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
    cat("In function plotCC", "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "rrWidth:", crwidth, "\n",
        "expf:", expf, "\n", "\n")
  }

  #Check of missing columns in data
  missing <- c(crwidth, expf, stand) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #If type is not 1 or 2, set to 1
  if(type < 1 | type > 2) type = 1

  #Initialize CC
  CC = 0

  #Loop across data and calculate CC
  for(i in 1:nrow(data))
  {
    #If DBH of record is GE min DBH and less than max add it to CC sum
    if(data[[dbh]][i] >= min & data[[dbh]][i] < max)
    {

      TREECC = pi * (data[[crwidth]][i]/2)^2 *(data[[expf]][i]/43560) * 100
      CC = CC + TREECC

      if(debug)
      {
        cat("TREE DBH:", data[[dbh]][i], "\n",
            "TREE EXP:", data[[expf]][i], "\n",
            "TREECC:", TREECC, "\n",
            "CC:", CC, "\n", "\n")
      }
    }
  }

  #Determine if canopy cover should be corrected
  if(type != 1)
  {
    CC <- correctCC(CC)
  }

  #Print stand and CC if debug is true.
  if(debug)
  {
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("CC:", CC, "\n", "\n")
  }

  #Return CC
  return(CC)
}

################################################################################
#Function plotQMD
#
#This function calculates QMD of plot/stand.
#
#Arguments:
#
#data:    Tree level dataframe corresponding to trees from a single stand.
#
#stand:   Name of column corresponding to stand associated with tree records
#         in data. By default this value is set to "StandID".
#
#dbh:     Name of column in data argument corresponding to DBH of tree records.
#         By default this argument is set to "TPA".
#
#
#expf:    Name of column in data argument corresponding to expansion factor of
#         tree records By default this argument is set to "TPA".
#
#min:     Minimum diameter to consider in calculation of QMD. By default this
#         argument is set to 0.
#
#min:     Maximum diameter to consider in calculation of QMD. By default this
#         argument is set to 999.
#
#debug:   logical variable indicating if debug statements should be printed. By
#         default this value is set to F.
#
#type:    Integer value representing type of percent canopy cover to return.
#         1 = not corrected for overlap
#         2 = corrected for overlap
#
#Value
#
#Percent canopy cover of inventory plot/stand
################################################################################

#'@export
plotQMD<-function(data,
                  stand = "StandID",
                  dbh = "DBH",
                  expf = "TPA",
                  min = 0.1,
                  max = 999,
                  debug = F)
{
  if(debug)
  {
    cat("In function plotQMD", "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "expf:", expf, "\n", "\n")
  }

  #Check of missing columns in data
  missing <- c(dbh, expf, stand) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Initialize DBHSQ and TPA
  DBHSQ = 0
  TPA = 0

  #Loop across data and calculate DBHSQ and TPA
  for(i in 1:nrow(data))
  {
    #If DBH of record is GE min DBH and less than max add it to DBHSQ and TPA
    #sums.
    if(data[[dbh]][i] >= min & data[[dbh]][i] < max)
    {
      DBHSQ <- DBHSQ + data[[dbh]][i]^2 * data[[expf]][i]
      TPA = TPA + data[[expf]][i]

      if(debug){
        cat("TREE DBH:", data[[dbh]][i], "\n",
            "TREE EXP:", data[[expf]][i], "\n",
            "DBHSQ:", DBHSQ, "\n",
            "TPA:", TPA, "\n", "\n")
      }
    }
  }

  #Calculate QMD from DBHSQ and TPA
  #If TPA is 0, then set QMD to 0.
  if(TPA <= 0) QMD = 0
  else
  {
    QMD <- sqrt(DBHSQ/TPA)
  }


  #Print stand and QMD if debug is true.
  if(debug)
  {
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("QMD:", QMD, "\n", "\n")
  }

  #Return QMD
  return(QMD)
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
#Function plotSDI
#
#This function calculates SDI for plot/stand.
#
#Arguments:
#
#data:  Tree level dataframe corresponding to trees from a single stand.
#
#stand: Name of column corresponding to stand ID associated with tree records
#       in data. By default this value is set to "StandID".
#
#dbh:   Name of column in data argument corresponding to DBH of tree records. By
#       default this argument is set to "DBH".
#
#expf:  Name of column in data argument corresponding to TPA of tree records.
#       By default this argument is set to "TPA".
#
#min:   Minimum diameter to consider in calculation of BA. By default this
#       argument is set to 0.
#
#min:   Maximum diameter to consider in calculation of BA. By default this
#       argument is set to 999.
#
#type:  Integer value used to specify if Zeide or Reinke SDI should be
#       calculated. By default this argument is set to 1.
#       1: Zeide SDI
#       2: Reineke
#
#debug: logical variable indicating if debug statements should be printed. By
#       default this value is set to FALSE.
#
#Value
#
#SDI of inventory plot/stand
################################################################################

#'@export
plotSDI <- function(data,
                    stand = "StandID",
                    dbh = "DBH",
                    expf = "TPA",
                    min = 0.1,
                    max = 999,
                    type = 1,
                    debug = F)
{

  if(debug)
  {
    cat("In function plotSDI", "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "expf:", expf, "\n", "\n")
  }

  #Check of missing columns in data
  missing <- c(dbh, expf, stand) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #If type is not 1 or 2, set to 1
  if(type < 1 | type > 2) type = 1

  #If type is 1, calculate Zeide SDI
  if(type == 1)
  {
    SDI = 0

    for(i in 1:nrow(data))
    {
      #If DBH of record is GE min DBH and less than max DBH add tree SDI to SDI
      #SDI
      if(data[[dbh]][i] >= min & data[[dbh]][i] < max)
      {
        SDI <- SDI + (data[[expf]][i] * (data[[dbh]][i]/10)^1.605)

        if(debug){
          cat("TREE DBH:", data[[dbh]][i], "\n",
              "TREE EXP:", data[[expf]][i], "\n",
              "SDI:", SDI, "\n", "\n")
        }
      }
    }
  }

  #Calculate Reineke SDI
  else
  {
    #Calculate plotTPA for DBH range
    TPA = plotTPA(data,
                  stand = stand,
                  dbh = dbh,
                  expf = expf,
                  min = min,
                  max = max)

    #Calculate plotQMD for DBH range
    QMD = plotQMD(data,
                  stand = stand,
                  dbh = dbh,
                  expf = expf,
                  min = min,
                  max = max)

    #Calculate SDI
    SDI = TPA * (QMD/10)^1.605

    #Do debug
    if(debug)
    {
      cat("TPA:", TPA, "\n",
          "QMD:", QMD, "\n",
          "SDI:", SDI, "\n", "\n")
    }
  }

  return(SDI)
}
