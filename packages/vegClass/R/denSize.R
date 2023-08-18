################################################################################
#denSizeR8
#
#This function calculates a density/size class based on USFS R8 rulesets defined
#by Chad Keyser in 2023. Size density class criteria for R8 is as follows:
#
#If the basal area of the stand is less than 10 BA, then the size class in the
#size density classification is advanced regeneration.
#
#Advanced regeneration size class = 1
#
#If the size class of the stand is advanced regeneration, then one of the
#following density classes will be determined:
#
#A: 0 - 200 stand TPA
#B: 200 - 400 stand TPA
#C: 400 - 600 stand TPA
#D: 600 + stand TPA
#
#If the basal area of the stand is greater than or equal to 10 BA, then the size
#class in the size density classification is either non-merch size, pulpwood
#size, or saw timber size. The size class with the most BA is selected as the
#size class in the density/size classification.
#
#Non-merch size class = 2
#Pulpwood size class = 3
#Saw timber size class = 4

#If the size class of the stand is non-merch, pulpwood or saw timber, then one
#of the following density classes will be determined:
#
#A: 0 - 40 BA
#B: 40 - 80 BA
#C: 80 - 120 BA
#D: 120+ BA
#
#Arguments
#
#data:     Data frame containing tree records from a single stand or plot. Data
#          frame must contain a column corresponding to stand/plot ID.
#
#stand:    Character string corresponding to name of column pertaining to stand
#          or plot ID associated with tree records in data argument. By default,
#          this value is set to "StandID".
#
#attrList: List of size and density attributes for species found in stand/plot.
#          This list is produced by the plotAttr function.
#
#debug:	   Logical variable used to specify if debug output should be printed to
#          R console. If value is TRUE, then debug output will printed to R
#          console.
################################################################################

#'@export
denSizeR8<-function(data,
                    stand = "StandID",
                    attrList,
                    debug = F)
{
  #Initialize density size class variable that will be returned
  denSize <- NA

  #If data has no rows, return
  if(nrow(data) <= 0) return(denSize)

  #Check for missing columns in data
  missing <- c(stand) %in% colnames(data)

  #If name of stand column is not found in data warning message is issued and
  #denSize is returned
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(denSize)
  }

  #If attribute list is NULL or length is less than or equal to 1 (ALL
  #species with NA values), return
  if(is.null(attrList) | length(attrList) <= 1)
  {
    return(denSize)
  }

  #Print stand and columns from data
  if(debug)
  {
    cat("In function denSizeR8", "\n")
    cat("Stand:", unique(data[[stand]]), "\n", "\n")
  }

  #Get TPA and BA for stand
  TPA <- attrList[["ALL"]]["TPA"]
  BA <- attrList[["ALL"]]["BA"]

  #If BA is less than 10, then an advanced regeneration size/density class will
  #be defined.
  if(BA < 10)
  {
    sizeClass <- "1"

    #Determine density class based on TPA
    if(TPA >= 0 & TPA < 200) densityClass = "A"
    else if(TPA >= 200 & TPA < 400) densityClass = "B"
    else if(TPA >= 400 & TPA < 600) densityClass = "C"
    else densityClass = "D"
  }

  #Else define density size class of non-merch, pulpwood, or sawtimber.
  else
  {
    #Find index of size class with most BA
    size <- which.max(c(attrList[["ALL"]]["NMBA"],
                      attrList[["ALL"]]["PWBA"],
                      attrList[["ALL"]]["STBA"]))

    #if size is 1: non merch
    #if size is 2: pulpwood
    #if size is 3: saw timber
    if(size == 1) sizeClass <- 2
    else if(size == 2) sizeClass <- 3
    else sizeClass <- 4

    #Determine density class based on BA
    if(BA >= 0 & BA < 40) densityClass = "A"
    else if(BA >= 40 & BA < 80) densityClass = "B"
    else if(BA >= 80 & BA < 120) densityClass = "C"
    else densityClass = "D"
  }

  #Determine denSize
  denSize <- paste0(sizeClass,
                    densityClass)

  #Do debug
  if(debug)
  {
    cat("ARTPA:", attrList[["ALL"]]["ARTPA"], "\n")
    cat("NMBA:", attrList[["ALL"]]["NMBA"], "\n")
    cat("PWBA:", attrList[["ALL"]]["PWBA"], "\n")
    cat("STBA:", attrList[["ALL"]]["STBA"], "\n")
    cat("BA:", BA, "\n")
    cat("TPA:", TPA, "\n")
    cat("sizeClass:", sizeClass, "\n")
    cat("densityClass:", densityClass, "\n")
    cat("denSize:", denSize, "\n", "\n")
  }

  return(denSize)
}
