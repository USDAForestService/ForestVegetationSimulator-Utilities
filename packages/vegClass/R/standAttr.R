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
                   debug = F)
{
  #Check of missing columns in data
  missing <- c(dbh, expf, stand) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Calculate BA
  BA <- sum(data[[dbh]]^2 * data[[expf]] * 0.005454)

  #Print dbh and tpa if debug is TRUE
  if(debug)
  {
    cat("In function plotBA", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("DBH:", "\n", paste(data[[dbh]], "\n"), "\n")
    cat("EXPF:", "\n", paste(data[[expf]], "\n"), "\n")
    cat("BA:", BA, "\n")
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
                    expf = "TPA",
                    debug = F)
{
  #Check of missing columns in data
  missing <- c(expf, stand) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Calculate BA
  TPA <- sum(data[[expf]])

  #Print TPA and expf if debug is TRUE
  if(debug)
  {
    cat("In function plotTPA", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("TPA:", "\n", paste(data[[expf]], "\n"), "\n")
    cat("TPA:", TPA, "\n")
  }

  #Return TPA
  return(TPA)
}

################################################################################
#Function plotCC
#
#This function calculates percent canopy cover of plot/stand.
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
#
#expf:    Name of column in data argument corresponding to expansion factor of
#         tree records By default this argument is set to "TPA".
#
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
                   crwidth = "CrWidth",
                   expf = "TPA",
                   debug = F,
                   type = 1)
{
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

  #Calculate CC
  CC <- sum(pi * (data[[crwidth]]/2)^2 *(data[[expf]]/43560) * 100)

  #Determine if canopy cover should be corrected
  if(type != 1)
  {
    CC <- correctCC(CC)
  }

  #Print crwidth, expf, and CC if debug is TRUE
  if(debug)
  {
    cat("In function plotCC", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("CRWIDTH:", "\n", paste(data[[crwidth]], "\n"), "\n")
    cat("TPA:", "\n", paste(data[[expf]], "\n"), "\n")
    cat("CC:", CC, "\n")
  }

  #Return CC
  return(CC)
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
