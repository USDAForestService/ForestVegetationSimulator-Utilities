################################################################################
#Function: vegOut
#
#This function is used to calculate the following attributes for an input
#plot/stand:
#
# - Basal area per acre for seedlings + stems (BA)
# - Trees per acre for seedlings + stems (TPA)
# - Quadratic mean diameter for seedlings + stems (QMD)
# - Zeide stand density index for seedlings + stems (ZSDI)
# - Reineke stand density index for seedlings + stems (RSDI)
# - Basal area per acre for stems (BA_STM)
# - Trees per acre for stems (TPA_STM)
# - Quadratic mean diameter for stems (QMD_STM)
# - Zeide stand density index stems (ZSDI_STM)
# - Reineke stand density index for stems (RSDI_STM)
# - Dominance type (DOM_TYPE)
# - Dominant species/genus/category or species/genus/category occurring before
#   underscore in dominance type (DCC1)
# - Percent canopy cover represented by DCC1 (XDCC1),
# - Species/genus/category occurring after underscore in dominance type (DCC2),
# - Percent canopy cover represented by DCC2 (XDCC2).
# - Storiedness (BA_STORY)
# - Canopy size class (CAN_SIZCLS)
# - Canopy size class timberland (CAN_SZTMB)
# - Canopy size class woodland (CAN_SZWDL)
# - QMD of top 20% (QMD_TOP20)
# - Basal area weighted diameter (BA_WT_DIA)
#
#Arguments
#
#data:    Tree level dataframe corresponding to trees from a single stand. When
#         called from main.R, this corresponds to a dataframe of trees from a
#         single stand and year combination.
#
#stand:   Name of column corresponding to stand associated with tree records
#         in data. By default this value is set to "StandID".
#
#species: Character string corresponding to name of column pertaining to USDA
#         plant symbols of tree records in data argument. By default, this
#         argument is set to "SpeciesPLANTS".
#
#dbh:     Name of column in data argument corresponding to DBH of tree records.
#         By default this argument is set to "DBH".
#
#expf:    Name of column in data argument corresponding to TPA of tree records.
#         By default this argument is set to "TPA".
#
#crwidth: Name of column corresponding crown width values of tree records in
#         data. By default this argument is set to "CrWidth".
#
#debug:	  Logical variable used to specify if debug output should be printed to
#         R console. If value is TRUE, then debug output will printed to R
#         console.
#
#Return value
#
#Single row dataframe containing attributes described above.
################################################################################

#'@export
vegOut <- function(data,
                   stand = "StandID",
                   species = "SpeciesPLANTS",
                   dbh = "DBH",
                   expf = "TPA",
                   crwidth = "CrWidth",
                   debug = F)
{
  #Initialize output dataframe
  vegData <- data.frame(CAN_COV = NA,
                        BA = NA,
                        TPA = NA,
                        QMD = NA,
                        ZSDI = NA,
                        RSDI = NA,
                        BA_STM = NA,
                        TPA_STM = NA,
                        QMD_STM = NA,
                        ZSDI_STM = NA,
                        RSDI_STM = NA,
                        DOM_TYPE = NA,
                        DCC1 = NA,
                        XDCC1 = NA,
                        DCC2 = NA,
                        XDCC2 = NA,
                        CAN_SIZCL = NA,
                        CAN_SZTMB = NA,
                        CAN_SZWDL = NA,
                        BA_STORY = NA)

  #Check of missing columns in data
  missing <- c(stand, species, dbh, expf, crwidth) %in% colnames(data)

  #If there is a FALSE value in missing report message and return vegData (all
  #NA values at this point)
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(vegData)
  }

  #Do debug
  if(debug)
  {
    cat("In function vegOut", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("Columns:", "\n",
        "stand:", stand, "\n",
        "species:", species, "\n",
        "dbh:", dbh, "\n",
        "expf:", expf, "\n",
        "crwidth:", crwidth, "\n", "\n")
  }

  #Calculate plot attributes for all trees (seedlings + stems)
  allAttr <- plotAttr(data,
                      stand = stand,
                      dbh = dbh,
                      crwidth = crwidth,
                      expf = expf)

  #Calculate canopy cover uncorrected for overlap (CAN_COV)
  vegData$CAN_COV <- allAttr["CC"]

  #Calculate BA, TPA, QMD, and SDI for seedlings + stems
  vegData$BA <- allAttr["BA"]
  vegData$BA_WT_DIA <- allAttr["BA_WT_DIA"]
  vegData$TPA <- allAttr["TPA"]
  vegData$QMD <- allAttr["QMD"]
  vegData$ZSDI <- allAttr["ZSDI"]
  vegData$RSDI <- allAttr["RSDI"]

  #Calculate plot attributes for stems (DBH >= 1")
  stemAttr <- plotAttr(data,
                       stand = stand,
                       dbh = dbh,
                       crwidth = crwidth,
                       expf = expf,
                       min = 1)

  vegData$BA_STM <- stemAttr["BA"]

  vegData$TPA_STM <- stemAttr["TPA"]

  vegData$QMD_STM <- stemAttr["QMD"]

  vegData$ZSDI_STM <- stemAttr["ZSDI"]

  vegData$RSDI_STM <- stemAttr["RSDI"]

  #Calculate QMD_TOP20
  vegData$QMD_TOP20 <- qmdTop20(data,
                                stand = stand,
                                dbh = dbh,
                                expf = expf,
                                TPA = allAttr["TPA"],
                                CC = allAttr["CC"])

  #Calculate DomType, dcc1, dcc2, xdcc1, and xdcc2
  dtResults<-domType(data = data,
                     stand = stand,
                     species = species,
                     dbh = dbh,
                     crwidth = crwidth,
                     expf = expf,
                     TPA = allAttr["TPA"],
                     CC = allAttr["UNCC"])

  #Dominance type
  vegData$DOM_TYPE<-dtResults[["DOMTYPE"]]

  #Dominance type 1
  vegData$DCC1<-dtResults[["DCC1"]]

  #Dominance type 1 CC
  vegData$XDCC1<-round(dtResults[["XDCC1"]],2)

  #Dominance type 2
  vegData$DCC2<-dtResults[["DCC2"]]

  #Dominance type 2 CC
  vegData$XDCC2<-round(dtResults[["XDCC2"]],2)

  #Canopy size class - R3 midscale mapping
  vegData$CAN_SIZCL<-canSizeCl(data = data,
                               stand = stand,
                               dbh = dbh,
                               crwidth = crwidth,
                               expf = expf,
                               type = 1,
                               TPA = allAttr["TPA"],
                               CC = allAttr["CC"])

  #Canopy size class - timberland
  vegData$CAN_SZTMB<-canSizeCl(data = data,
                               stand = stand,
                               dbh = dbh,
                               crwidth = crwidth,
                               expf = expf,
                               type = 2,
                               TPA = allAttr["TPA"],
                               CC = allAttr["CC"])

  #Canopy size class - woodland
  vegData$CAN_SZWDL<-canSizeCl(data = data,
                               stand = stand,
                               dbh = dbh,
                               crwidth = crwidth,
                               expf = expf,
                               type = 3,
                               TPA = allAttr["TPA"],
                               CC = allAttr["CC"])

  #BA storiedness
  vegData$BA_STORY<-baStory(data,
                            stand = stand,
                            dbh = dbh,
                            expf = expf,
                            BA = allAttr["BA"],
                            TPA = allAttr["TPA"],
                            CC = allAttr["CC"])

  return(vegData)
}

