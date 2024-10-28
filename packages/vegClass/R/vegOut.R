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
# - STSIM potential vegetation type grouping (VEGTYPE - R1 ruleset)
# - Cover Type (COVERTYPE_R1 - R1 ruleset)
# - Dominance group 6040 (DOM6040 - R1 ruleset)
# - Number of Canopy layers (VERTICAL STRUCTURE - R1 ruleset)
# - Basal area weighted diameter size class (SIZECLASS_NTG)
# - Size & density structure class strata (STRCLSSTR - R1 ruleset)
# - Dominance type reported as the top 3 species by percent canopy cover (R2 ruleset)
# - Percent canopy cover of the most dominant species in the stand (R2 ruleset)
# - Percent canopy cover of the 2nd most dominant species in the stand (R2 ruleset)
# - Percent canopy cover of the 3rd most dominant species in the stand (R2 ruleset)
# - Cover type (R2 ruleset)
# - Tree size class (R2 ruleset)
# - Percent Canopy cover class (R2 ruleset)
# - Habitat structural stage calculated and reported for the 1 through 4C classes (R2 ruleset)
# - Habitat structural stage calculated and reported for the 1 through 5 classes (R2 ruleset)
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

#Arguments
#
#data:    Data frame containing tree records from a single stand or plot. Data
#         frame must contain a column corresponding to stand/plot ID, species
#         (USDA Plant Symbol only), DBH, expansion factor, and crown width for
#         each tree record.
#
#region:  Integer variable corresponding to USFS region number (1, 2, 3, 8, or
#         MPSG (Mountain Planning Services Group, R1-4)).
#
#MPSGcovTyp: Integer value corresponding to the USFS region number whose
#         algorithms should be used in the calculations of the
#         COVERTYPE_MPSG variable.
#
#addHSS:	Logical variable used to indicate whether the USFS Region 2
#         Habitat Structural Stage (HSS) variables should be included in
#         the file specified in output argument.
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
#InvDB:   Connection to inventory database defined in input
#
#InvStandTbl: Character string corresponding to name of the stand data table
#         in InvDB
#
#debug:	  Logical variable used to specify if debug output should be printed to
#         R console. If value is TRUE, then debug output will printed to R
#         console.
#Return value
#
#Single row dataframe containing attributes described above.
################################################################################

#'@export
vegOut <- function(data,
                   region = 3,
                   MPSGcovTyp = MPSGcovTyp,
                   addHSS = addHSS,
                   stand = "StandID",
                   species = "SpeciesPLANTS",
                   dbh = "DBH",
                   ht = "Ht",
                   expf = "TPA",
                   crwidth = "CrWidth",
                   vol1 = "MCuFt",
                   vol2 = "SCuFt",
                   vol3 = "SBdFt",
                   vol1DBH = vol1DBH,
                   vol2DBH = vol2DBH,
                   vol3DBH = vol3DBH,
                   InvDB=InvDB,
                   InvStandTbl=InvStandTbl,
                   customVars = customVars,
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

  #If region is 1
  if(region==1)
  {
    #Calculate VEGTYPE, COVERTYPE_R1, DOM6040, VERTICAL_STRUCTURE,
    #SIZECLASS_NTG, and STRCLSSTR
    dtResults<-R1(data = data,
                       stand = stand,
                       species = species,
                       dbh = dbh,
                       expf = expf,
                       ht = ht,
                       TPA = allAttr[["ALL"]]["TPA"],
                       BA = allAttr[["ALL"]]["BA"],
                       plotvals = allAttr,
                       InvDB=InvDB,
                       InvStandTbl=InvStandTbl)

    #Region 1 Potential Vegetation Type (StSim)-Cover Type
    vegData$VEGTYPE<-dtResults[["VEGTYPE"]]

    #Region 1 Cover Type
    vegData$COVERTYPE_R1<-dtResults[["COVERTYPE_R1"]]

    #DOM6040 Subclass
    vegData$DOM6040<-dtResults[["DOM6040"]]

    #CANOPY LAYERS
    vegData$VERTICAL_STRUCTURE<-dtResults[["VERTICAL_STRUCTURE"]]

    #SIZECLASS_NTG
    vegData$SIZECLASS_NTG<-dtResults[["SIZECLASS_NTG"]]

    #structure class strata
    vegData$STRCLSSTR<-dtResults[["STRCLSSTR"]]
  }
  #If region is 2
  if(region==2){

    dtResults<-R2(data = data,
                        stand = stand,
                        species = species,
                        dbh = dbh,
                        expf = expf,
                        ht = ht,
                        TPA = allAttr[["ALL"]]["TPA"],
                        BA = allAttr[["ALL"]]["BA"],
                        CC = allAttr[["ALL"]]["CC"],
                        plotvals = allAttr)

    #DOM_TYPE_R2
    vegData$DOM_TYPE_R2<-dtResults[["DOM_TYPE_R2"]]

    #DOM_TYPE_R2_CC1
    vegData$DOM_TYPE_R2_CC1<-dtResults[["DOM_TYPE_R2_CC1"]]

    #DOM_TYPE_R2_CC2
    vegData$DOM_TYPE_R2_CC2<-dtResults[["DOM_TYPE_R2_CC2"]]

    #DOM_TYPE_R2_CC3
    vegData$DOM_TYPE_R2_CC3<-dtResults[["DOM_TYPE_R2_CC3"]]

    #Region 2 Cover Type
    vegData$COVERTYPE_R2<-dtResults[["COVERTYPE_R2"]]

    #TREE_SIZE_CLASS
    vegData$TREE_SIZE_CLASS_R2<-dtResults[["TREE_SIZE_CLASS_R2"]]

    #CROWN_COVER
    vegData$CROWN_CLASS_R2<-dtResults[["CROWN_CLASS_R2"]]

    #Habitat Estimated Structural Stage 1-4C
    vegData$HSS1_4C<-dtResults[["HSS1_4C"]]

    #Habitat Estimated Structural Stage 1-5
    vegData$HSS1_5<-dtResults[["HSS1_5"]]
  }
  #If region is 3
  if(region==3)
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

  #If region is 8
  if(region == 8)
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
  #If region is MPSG
  if(region=="MPSG")
  {
    MPSGresults<-MPSG(data = data,
                      stand = stand,
                      species = species,
                      dbh = dbh,
                      expf = expf,
                      ht = ht,
                      crwidth = crwidth,
                      TPA = allAttr[["ALL"]]["TPA"],
                      BA = allAttr[["ALL"]]["BA"],
                      CC = allAttr[["ALL"]]["CC"],
                      plotvals = allAttr,
                      InvDB = InvDB,
                      InvStandTbl = InvStandTbl,
                      MPSGcovTyp = MPSGcovTyp,
                      addHSS = addHSS)

    #COVERTYPE_MPSG
    vegData$COVERTYPE<-MPSGresults[["COVERTYPE_MPSG"]]

    #TREE_SIZE_CLASS
    vegData$TREE_SIZE_CLASS<-MPSGresults[["TREE_SIZE_CLASS"]]

    #CROWN_COVER
    vegData$CROWN_CLASS<-MPSGresults[["CROWN_CLASS"]]

    #CANOPY_LAYERS
    vegData$VERTICAL_STRUCTURE<-MPSGresults[["VERTICAL_STRUCTURE"]]

    if(addHSS){
      #Habitat Estimated Structural Stage 1-4C
      vegData$HSS1_4C<-MPSGresults[["HSS1_4C"]]

      #Habitat Estimated Structural Stage 1-5
      vegData$HSS1_5<-MPSGresults[["HSS1_5"]]
    }
  }
  #If we need to calculate additional custom variables
  if (!is.null(customVars)){

    #Calculate custom attributes
    custAttr <- customAttr(data,
                          region = region,
                          stand = stand,
                          species = species,
                          dbh = dbh,
                          ht = ht,
                          crwidth = crwidth,
                          expf = expf,
                          vol1 = vol1,
                          vol2 = vol2,
                          vol3 = vol3,
                          vol1DBH = vol1DBH,
                          vol2DBH = vol2DBH,
                          vol3DBH = vol3DBH,
                          customVars = customVars)

    vegData <- cbind(vegData,custAttr)
  }

  return(vegData)
}
