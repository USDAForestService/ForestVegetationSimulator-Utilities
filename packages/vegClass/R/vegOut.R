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
# - Dominance type (DOM_TYPE - R3 or R8 ruleset)
# - Dominant species/genus/category or species/genus/category occurring before
#   underscore in dominance type (DCC1 - R3 ruleset)
# - Percent canopy cover represented by DCC1 (XDCC1 - R3 ruleset),
# - Species/genus/category occurring after underscore in dominance type (DCC2 -
#   R3 ruleset)
# - Percent canopy cover represented by DCC2 (XDCC2 - R3 ruleset).
# - Storiedness (BA_STORY - R3 ruleset)
# - Canopy size class (CAN_SIZCLS - R3 ruleset)
# - Canopy size class timberland (CAN_SZTMB - R3 ruleset)
# - Canopy size class woodland (CAN_SZWDL - R3 ruleset)
# - QMD of top 20% (QMD_TOP20)
# - Basal area weighted diameter (BA_WT_DIA)
# - Basal area weighted height (BA_WT_HT)
# - Dominance type of advance regeneration SSDOMSPP - R8 ruleset)
# - TPA weighted average height of advance regeneration (SSSIZE - R8 ruleset)
# - Tree per acre of advance regeneration (SSTPA - R8 ruleset)
# - Dominance type of non-merchantable trees (NMDOMSPP - R8 only)
# - Basal area weighted diameter of non-merchantable trees (NMSIZE - R8 ruleset)
# - Basal area of non-merchantable trees (NMBA - R8 ruleset)
# - Dominance type of pulpwood trees (PWDOMSPP - R8 ruleset)
# - Basal area weighted diameter of pulpwood trees (PWSIZE - R8 ruleset)
# - Basal area of pulpwood trees (PWBA - R8 ruleset)
# - Dominance type of saw timber sized trees (STDOMSPP - R8 ruleset)
# - Basal area weighted diameter of saw timber sized trees (STSIZE - R8 ruleset)
# - Basal area of saw timber sized trees (STBA - R8 ruleset)
# - Size and density class (VEGCLASS - R8 ruleset)
#
#Arguments
#
#data:    Data frame containing tree records from a single stand or plot. Data
#         frame must contain a column corresponding to stand/plot ID, species
#         (USDA Plant Symbol only), DBH, expansion factor, and crown width for
#         each tree record.
#
#region:  Integer variable corresponding to USFS region number (1, 2, 3, 4,
#         5, 6, 8, 9, or 10).
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
#ht:      Character string corresponding to name of column pertaining to total
#         height of tree records in data argument. By default, this argument is
#         set to "Ht".
#
#expf:    Character string corresponding to name of column pertaining to TPA of
#         tree records in data argument. By default, this argument is set to
#         "TPA".
#
#crwidth: Name of column corresponding crown width values of tree records in
#         data. By default this argument is set to "CrWidth".
#
#vol1:    Character string corresponding to name of column pertaining to
#         total cubic foot (western variants) or merchantable cubic foot
#         volume (eastern variants) of tree records in data argument. By
#         default, this argument is set to "MCuFt". Currently vol1 is only
#         used if region argument is set to 8.
#
#vol2:    Character string corresponding to name of column pertaining to
#         merchantable cubic foot (western variants) or sawlog cubic foot
#         volume (eastern variants) of tree records in data argument. By
#         default, this argument is set to "SCuFt". Currently vol2 is only
#         used if region argument is set to 8.
#
#vol3:    Character string corresponding to name of column pertaining to
#         board foot (western variants) or sawlog board foot volume (eastern
#         variants) of tree records in data argument. By default, this
#         argument is set to "SBdFt". Currently vol3 is only used if region
#         argument is set to 8.
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
                   region = 3,
                   stand = "StandID",
                   species = "SpeciesPLANTS",
                   dbh = "DBH",
                   ht = "Ht",
                   expf = "TPA",
                   crwidth = "CrWidth",
                   vol1 = "MCuFt",
                   vol2 = "SCuFt",
                   vol3 = "SBdFt",
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
                        BA_WT_DIA = NA,
                        BA_WT_HT = NA,
                        QMD_TOP20 = NA)

  #Check of missing columns in data
  missing <- c(stand, species, dbh, ht, expf, crwidth, vol1, vol2, vol3) %in%
    colnames(data)

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
        "crwidth:", crwidth, "\n",
        "vol1:", vol1, "\n",
        "vol2:", vol2, "\n",
        "vol3:", vol3, "\n","\n")
  }

  #Calculate plot attributes for all trees (seedlings + stems). Do this for all
  #regions.
  allAttr <- plotAttr(data,
                      region = region,
                      stand = stand,
                      species = species,
                      dbh = dbh,
                      ht = ht,
                      crwidth = crwidth,
                      expf = expf,
                      vol1 = vol1,
                      vol2 = vol2,
                      vol3 = vol3)

  #Calculate canopy cover uncorrected for overlap (CAN_COV)
  vegData$CAN_COV <- round(allAttr[["ALL"]]["CC"],2)

  #Calculate BA, BA_WT_DIA, TPA, QMD, and SDI for seedlings + stems. Do this for
  #all regions.
  vegData$BA <- round(allAttr[["ALL"]]["BA"],2)
  vegData$BA_WT_DIA <- round(allAttr[["ALL"]]["BA_WT_DIA"],2)
  vegData$BA_WT_HT <- round(allAttr[["ALL"]]["BA_WT_HT"],2)
  vegData$TPA <- round(allAttr[["ALL"]]["TPA"],2)
  vegData$QMD <- round(allAttr[["ALL"]]["QMD"],2)
  vegData$ZSDI <- round(allAttr[["ALL"]]["ZSDI"],2)
  vegData$RSDI <- round(allAttr[["ALL"]]["RSDI"],2)

  #Calculate BA, BA_WT_DIA, TPA, QMD, and SDI for stems (DBH >= 1")
  stemAttr <- plotAttr(data,
                       region = region,
                       stand = stand,
                       dbh = dbh,
                       ht = ht,
                       crwidth = crwidth,
                       expf = expf,
                       vol1 = vol1,
                       vol2 = vol2,
                       vol3 = vol3,
                       min = 1)

  vegData$BA_STM <- round(stemAttr[["ALL"]]["BA"],2)
  vegData$TPA_STM <- round(stemAttr[["ALL"]]["TPA"],2)
  vegData$QMD_STM <- round(stemAttr[["ALL"]]["QMD"],2)
  vegData$ZSDI_STM <- round(stemAttr[["ALL"]]["ZSDI"],2)
  vegData$RSDI_STM <- round(stemAttr[["ALL"]]["RSDI"],2)

  #Calculate QMD_TOP20
  vegData$QMD_TOP20 <- round(qmdTop20(data,
                                stand = stand,
                                dbh = dbh,
                                expf = expf,
                                TPA = allAttr[["ALL"]]["TPA"],
                                CC = allAttr[["ALL"]]["CC"]),2)

  #If region is not 8 or 9
  if(! region %in% c(8, 9))
  {
    #Calculate DomType, dcc1, dcc2, xdcc1, and xdcc2
    dtResults<-domTypeR3(data = data,
                         stand = stand,
                         species = species,
                         dbh = dbh,
                         crwidth = crwidth,
                         expf = expf,
                         TPA = allAttr[["ALL"]]["TPA"],
                         CC = allAttr[["ALL"]]["UNCC"])

    #Dominance type
    vegData$DOM_TYPE<-dtResults[["DOMTYPE"]]

    #Primary attribute of dominance type
    vegData$DCC1<-dtResults[["DCC1"]]

    #CC of primary attribute
    vegData$XDCC1<-round(dtResults[["XDCC1"]],2)

    #Secondary attribute of dominance type
    vegData$DCC2<-dtResults[["DCC2"]]

    #CC of secondary attribute
    vegData$XDCC2<-round(dtResults[["XDCC2"]],2)

    #Canopy size class - R3 midscale mapping
    vegData$CAN_SIZCL<-canSizCl(data = data,
                                stand = stand,
                                dbh = dbh,
                                crwidth = crwidth,
                                expf = expf,
                                type = 1,
                                TPA = allAttr[["ALL"]]["TPA"],
                                CC = allAttr[["ALL"]]["CC"])

    #Canopy size class - timberland
    vegData$CAN_SZTMB<-canSizCl(data = data,
                                stand = stand,
                                dbh = dbh,
                                crwidth = crwidth,
                                expf = expf,
                                type = 2,
                                TPA = allAttr[["ALL"]]["TPA"],
                                CC = allAttr[["ALL"]]["CC"])

    #Canopy size class - woodland
    vegData$CAN_SZWDL<-canSizCl(data = data,
                                stand = stand,
                                dbh = dbh,
                                crwidth = crwidth,
                                expf = expf,
                                type = 3,
                                TPA = allAttr[["ALL"]]["TPA"],
                                CC = allAttr[["ALL"]]["CC"])

    #BA storiedness
    vegData$BA_STORY<-baStory(data = data,
                              stand = stand,
                              dbh = dbh,
                              expf = expf,
                              BA = allAttr[["ALL"]]["BA"],
                              TPA = allAttr[["ALL"]]["TPA"],
                              CC = allAttr[["ALL"]]["CC"])
  }

  #Else region is 8 or 9
  else
  {
    #Calculate ardomspp, nmdomspp, pwdomspp, stdomspp, domtype
    dtResults<-domTypeR8(data = data,
                         attrList = allAttr)

    #Determine vegClass (size/density)
    vegClass <- denSizeR8(data = data,
                          attrList = allAttr)

    #SSDOMSPP
    vegData$SSDOMSPP <- dtResults[["SSDOMSPP"]]

    #SSSIZE
    vegData$SSSIZE <- round(allAttr[["ALL"]]["SSSIZE"],2)

    #SSTPA
    vegData$SSTPA <- round(allAttr[["ALL"]]["SSTPA"],2)

    #NMDOMSPP
    vegData$NMDOMSPP <- dtResults[["NMDOMSPP"]]

    #NMSIZE
    vegData$NMSIZE <- round(allAttr[["ALL"]]["NMSIZE"],2)

    #NMBA
    vegData$NMBA <- round(allAttr[["ALL"]]["NMBA"],2)

    #PWDOMSPP
    vegData$PWDOMSPP <- dtResults[["PWDOMSPP"]]

    #PWSIZE
    vegData$PWSIZE <- round(allAttr[["ALL"]]["PWSIZE"],2)

    #PWBA
    vegData$PWBA <- round(allAttr[["ALL"]]["PWBA"],2)

    #STDOMSPP
    vegData$STDOMSPP <- dtResults[["STDOMSPP"]]

    #STSIZE
    vegData$STSIZE <- round(allAttr[["ALL"]]["STSIZE"],2)

    #STBA
    vegData$STBA <- round(allAttr[["ALL"]]["STBA"],2)

    #DOMTYPE
    vegData$DOMTYPE <- dtResults[["DOMTYPE"]]

    #VEGCLASS
    vegData$VEGCLASS <- vegClass
  }

  return(vegData)
}
