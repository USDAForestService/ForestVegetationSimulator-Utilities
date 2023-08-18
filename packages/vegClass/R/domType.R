################################################################################
#Function: domTypeR3
#
#Calculates stand dominance type using USFS Region 3 rule sets in accordance with
#Vandendriesche, D., 2013. A Compendium of NFS Regional Vegetation
#Classification Algorithms. USDA Forest Service. Fort Collins, CO (R3-1 - R3-3).
#
#Arguments
#
#data:    Data frame containing tree records from a single stand or plot. Data
#         frame must contain a column corresponding to stand/plot ID, DBH,
#         species code (USDA plant symbol), expansion factor, and crown width
#         for each tree record.
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
#expf:    Character string corresponding to name of column pertaining to TPA of
#         tree records in data argument. By default, this argument is set to
#         "TPA".
#
#crwidth: Character string corresponding to name of column pertaining to crown
#         width values of tree records in data argument. By default, this
#         argument is set to "CrWidth".
#
#TPA:     TPA of stand/plot.
#
#CC:      Canopy cover uncorrected for overlap of stand/plot.
#
#debug:	  Logical variable used to specify if debug output should be printed to
#         R console. If value is TRUE, then debug output will printed to R
#         console.
#
#Return value
#
#Named list containing:
# - Dominance type (DOMTYPE),
# - Dominant species/genus/category or species/genus/category occurring before
#   underscore in dominance type (DCC1)
# - Percent canopy cover corrected for overlap represented by DCC1 (XDCC1),
# - Species/genus/category occurring after underscore in dominance type (DCC2),
# - Percent canopy cover corrected for overlap represented by DCC2 (XDCC2).
################################################################################

#'@export
domTypeR3<-function(data,
                    stand = "StandID",
                    species = "SpeciesPLANTS",
                    dbh = "DBH",
                    expf = "TPA",
                    crwidth = "CrWidth",
                    TPA = 0,
                    CC = 0,
                    debug = F){

  #Initialize results vector to NA
  results=list("DOMTYPE" = NA,
               "DCC1" = NA,
               "XDCC1" = NA,
               "DCC2" = NA,
               "XDCC2" = NA)

  #If TPA is 0 return
  if(TPA <= 0) return(results)

  #If data has no rows, return
  if(nrow(data) <= 0) return(results)

  #Check for missing columns in data
  missing <- c(stand, species, dbh, expf, crwidth) %in% colnames(data)

  #If name of columns provided in stand, dbh, expf, species, and crwidth are not
  #found in data warning message is issued and NA values are returned.
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(results)
  }

  #Print stand and columns from data
  if(debug)
  {
    cat("In function domType", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("Columns:", "\n",
                "stand:", stand, "\n",
                "species:", species, "\n",
                "dbh:", dbh, "\n",
                "expf:", expf, "\n",
                "crwidth:", crwidth, "\n", "\n")
  }

  #Initialize boolean variable that is used to determine if DomType has been
  #found.
  domTypeFound = F

  #Initialize DomType as NA
  DomType<-NA

  #Initialize dcc1 and dcc2 variables. These variables are used to store
  #species, genera, leaf retention, or shade tolerance categories encompassed
  #in DomType that have the most and second most percent canopy cover
  #representation.
  dcc1<-"NA"
  dcc2<-"NA"

  #Initialize xdcc1 and xdcc2 variables. These are used to store percent
  #canopy cover corrected for overlap for dcc1 and dcc2 respectively.
  xdcc1<-0
  xdcc2<-0

  #Calculate percent canopy cover for each tree record if TREECC does not
  #exist in data
  if(! "TREECC" %in% colnames(data))
  {
    data$TREECC <- pi * (data[[crwidth]]/2)^2 *(data[[expf]]/43560) * 100
  }

  #Print CC and TPA if debug is TRUE
  if(debug) cat("CC of plot is", CC, "\n")
  if(debug) cat("TPA of plot is", TPA, "\n")

  #Identify unique species in stand
  spStand <- unique(data[[species]])

  #Initialize vector that will store genus value for each species
  genusStand <- vector("character", length(spStand))

  #Initialize vector that will store shade tolerance value for each species
  shadeTolStand <- vector("character", length(spStand))

  #Initialize vector that will store shade leaf retention value for each species
  leafRetStand <- vector("character", length(spStand))

  #Populate genusStand, shadeTolStand, and leadRetStand vectors.
  #GENUS, R3_SHADE_TOL, and LEAF_RETEN vectors exist in commons.R
  for(i in 1:length(spStand))
  {
    sp <- spStand[i]

    #Match sp in PLANT
    spIndex <- match(sp, PLANT)

    #Get genus
    genus <- GENUS[spIndex]

    #Get shade tolerance
    shadeTol <- R3_SHADE_TOL[spIndex]

    #Get leaf retention
    leafRet <- LEAF_RETEN[spIndex]

    #Load vectors
    genusStand[i] <- genus
    shadeTolStand[i] <- shadeTol
    leafRetStand[i] <- leafRet

    #Name the vectors
    names(genusStand)[i] <- sp
    names(shadeTolStand)[i] <- sp
    names(leafRetStand)[i] <- sp
  }

  #If debug, print the values in genusStand, shadeTolStand, and lefRetStand
  if(debug)
  {
    cat("Species in stand:", spStand, "\n")
    cat("Genera in stand:", genusStand, "\n")
    cat("Shade tolerance values in stand:", shadeTolStand, "\n")
    cat("Leaf retention values in stand:", leafRetStand, "\n")
  }

  #==========================================================================
  #LEAD 1 - 5
  #
  #LEAD 1:     Tree life form > 10% canopy cover
  #LEAD 2 - 5: Ignored. Shrubs and herbs are not accounted for since FVS
  #            does not consider these types of life forms.
  #==========================================================================

  #If corrected CC is less and then 10 and TPA less than 100, DOMTYPE is NVG
  if(correctCC(CC) < 10 & TPA < 100)
  {
    DomType = "NVG"
    domTypeFound = T
    if(debug) cat("LEAD 1-5", "CC:", correctCC(CC),"LT 10 and",
                  "TPA:", TPA,"LT 100.", "\n", "dcc1:", dcc1,"xdcc1:",xdcc1,
                  "DomType:", DomType, "\n",
                  fill = 80)
  }

  #==========================================================================
  #Start processing of LEAD 6 - 18
  #==========================================================================

  if(!domTypeFound)
  {
    #========================================================================
    #Initialize vectors for storing percent canopy cover (CC) for species,
    #genera, leaf retention (R3 values), and shade tolerance (R3 values).
    #========================================================================

    #USDA plant symbols found in data
    spValues<-spStand

    #Genera found in data
    genusValues<-unique(genusStand)

    #Vector for storing percent canopy cover for each USDA plant symbol
    sppCC<-rep(0, length(spStand))
    names(sppCC)<-spStand

    #Vector for storing percent canopy cover for each genera
    genusCC<-rep(0, length(genusValues))
    names(genusCC)<-genusValues

    #Vector for storing percent canopy cover for each shade tolerance category
    shadeTolCC<-rep(0, 2)
    names(shadeTolCC)<-c("INT", "TOL")

    #Vector for storing percent canopy cover for leaf retention categories
    leafRetenCC<-rep(0, 2)
    names(leafRetenCC)<-c("EVERGREEN", "DECIDUOUS")

    #==========================================================================
    #Loop across across data and populate sppCC, genusCC, ShadeTolCC, and
    #leafRetenCC.
    #==========================================================================

    #Loop across data
    for(k in 1:nrow(data))
    {
      #Extract species being processed
      spp = data[[species]][k]

      #Extract genus for spp k
      genus<- genusStand[spp]

      #Extract shade tolerance for spp k
      shadeTol<-shadeTolStand[spp]

      #Extract leaf retention for spp k
      leafReten<-leafRetStand[spp]

      #Add SPPC to sppCC
      sppCC[spp]<-sppCC[spp] + data$TREECC[k]

      #Add SPPC to genusCC
      genusCC[genus]<-genusCC[genus] + data$TREECC[k]

      #Bypass addition of SPPC to shadeTolCC if shadeTol for species k is NA.
      if(!is.na(shadeTol))
      {
        #Add SPPC to shadeTolCC
        shadeTolCC[shadeTol]<-shadeTolCC[shadeTol] + data$TREECC[k]
      }

      #Add SPPC to leafRetenCC
      leafRetenCC[leafReten]<-leafRetenCC[leafReten] + data$TREECC[k]
    }

    #Sort sppCC, genusCC, leafRetenCC, and shadeTolCC by descending order
    sppCC<-sort(sppCC, decreasing = T)
    genusCC<-sort(genusCC, decreasing = T)
    leafRetenCC<-sort(leafRetenCC, decreasing = T)
    shadeTolCC<-sort(shadeTolCC, decreasing = T)

    #Debug
    if(debug) cat("In domType function", "\n")
    if(debug) cat("sppCC:", names(sppCC), "\n",
                  "sppCC values:", sppCC, "\n", "\n",
                  "genusCC:", names(genusCC), "\n",
                  "genusCC values:", genusCC,"\n", "\n",
                  "leafRetenCC:", names(leafRetenCC), "\n",
                  "leafRetenCC values:", leafRetenCC, "\n", "\n",
                  "shadeTolCC:", names(shadeTolCC), "\n",
                  "shadeTolCC values:", shadeTolCC, "\n", "\n")

    #========================================================================
    #LEAD 11-12
    #
    #LEAD 11: Canopy cover of single most abundant tree species >= 60% of
    #         total tree canopy cover.
    #LEAD 12: Canopy cover of two most abundant tree species >= 80% of total
    #         tree canopy cover and each species individually >= 20% of total
    #         tree canopy cover.
    #========================================================================

    #Test if most abundant species is the dominance type
    if(sppCC[1] >= (CC * 0.60)){

      #Set dominance type
      DomType = names(sppCC[1])

      #Set dcc1
      dcc1 = names(sppCC[1])

      #Extract value for xdcc1
      xdcc1 = sppCC[1]

      #Set domTypeFound to T
      domTypeFound = T

      #Lead 11 debug
      if(debug) cat("LEAD 11", "sppCC:", sppCC[1], "CC:", CC,
                    "dcc1:", dcc1, "xdcc1:", xdcc1, "DomType:", DomType,
                    "\n", fill = 80)
    }

    if(!domTypeFound)
    {
      #Test if Canopy cover of two most abundant tree species >= 80% of total
      #tree canopy cover and each species individually > 20% of total tree
      #canopy cover
      if(sppCC[1] >= (CC * 0.20) &&
         sppCC[2] >= (CC * 0.20) &&
         sppCC[1] + sppCC[2] >= (CC * 0.80)){

        #Get and sort species names
        spNames<-sort(c(names(sppCC[1]), names(sppCC[2])), decreasing = F)

        #Separate names with underscore
        DomType<-paste(spNames, collapse = '_')

        #Set dcc1 and xdcc1
        dcc1 = names(sppCC[1])
        xdcc1 = sppCC[1]

        #Set dcc2 and xdcc2
        dcc2 = names(sppCC[2])
        xdcc2 = sppCC[2]

        domTypeFound = T

        #Lead 12 debug
        if(debug) cat("LEAD 12", "sppCC1:", sppCC[1], "sppCC2:", sppCC[2],
                      "CC:", CC, "dcc1:", dcc1, "xdcc1:", xdcc1,
                      "dcc2:", dcc2, "xdcc2:", xdcc2,"DomType:", DomType,
                      "\n", fill = 80)
      }
    }

    #========================================================================
    #LEAD 13-14
    #
    #LEAD 13: Canopy cover of single most abundant tree genus >= 60% of total
    #         tree canopy cover.
    #LEAD 14: Canopy cover of the single most abundant tree species and
    #         single most abundant tree genus collectively >= 80% of total
    #         tree canopy cover, each individually >= 20% of total tree canopy
    #         cover. The most abundant genus must be mutually exclusive from
    #         the genus of the most abundant species.
    #========================================================================

    #Check if most abundant genus is the dominance type
    if(!domTypeFound){
      if(genusCC[1] >= (CC * 0.60)){

        #Set DomType
        DomType = names(genusCC[1])

        #Set dcc1 and xdcc1
        dcc1 = names(genusCC[1])
        xdcc1 = genusCC[1]

        domTypeFound = T

        #LEAD 13
        if(debug) cat("LEAD 13", "genusCC1:", genusCC[1], "CC:", CC,
                      "dcc1:", dcc1, "xdcc1:", xdcc1,"DomType:", DomType, "\n",
                      fill = 80)
      }
    }

    #Check if most abundant genus and most abundant species are the dominance
    #type
    if(!domTypeFound)
    {
      #Find index where species and genus are mutually exclusive
      genIndex<-excGenusSp(names(sppCC)[1], names(genusCC), genusStand)

      if(genusCC[genIndex] >= (CC * 0.20) &&
         sppCC[1] >= (CC * 0.20) &&
         genusCC[genIndex] + sppCC[1] >= (CC * 0.80))
      {
        #Get and sort names of genus and species
        genusSpNames<-sort(c(names(genusCC[genIndex]), names(sppCC[1])),
                           decreasing = F)

        #Set DomType
        DomType<-paste(genusSpNames, collapse = '_')


        #Set dcc1, xdcc1, dcc2, and xdcc2

        #Situation where species has more CC than genus
        if(sppCC[1] > genusCC[genIndex])
        {
          #Set dcc1 and xdcc1 (species is dominant)
          dcc1 = names(sppCC[1])
          xdcc1 = sppCC[1]

          #Set dcc2 and xdcc2
          dcc2 = names(genusCC[genIndex])
          xdcc2 = genusCC[genIndex]
        }

        #Situation where genus has more CC than species
        else
        {
          #Set dcc1 and xdcc1 (genus is dominant)
          dcc1 = names(genusCC[genIndex])
          xdcc1 = genusCC[genIndex]

          #Set dcc2 and xdcc2
          dcc2 = names(sppCC[1])
          xdcc2 = sppCC[1]
        }

        domTypeFound = T

        #LEAD 14
        if(debug) cat("LEAD 14", "sppCC1:", sppCC[1], "genusCC:", sppCC[genIndex],
                      "CC:", CC, "dcc1:", dcc1, "xdcc1:", xdcc1, "dcc2:",
                       dcc2, "xdcc2:", xdcc2, "DomType:", DomType, "\n",
                      fill = 80)
      }
    }

    #==========================================================================
    #LEAD 15: Canopy cover of two most abundant tree genera >= 80% of total tree
    #         canopy cover, each individually >= 20% of total tree canopy cover.
    #==========================================================================

    if(!domTypeFound)
    {
      if(genusCC[1] >= (CC * 0.20) &&
         genusCC[2] >= (CC * 0.20) &&
         genusCC[1] + genusCC[2] >= (CC * 0.80)){

        #Get and sort names of genera
        genusNames<-sort(c(names(genusCC[1]), names(genusCC[2])),
                           decreasing = F)

        #Separate genera names with an underscore
        DomType<-paste(genusNames, collapse = '_')

        #Set dcc1 and xdcc1
        dcc1 = names(genusCC[1])
        xdcc1 = genusCC[1]

        #Set dcc2 and xdcc2
        dcc2 = names(genusCC[2])
        xdcc2 = genusCC[2]

        domTypeFound = T

        if(debug) cat("LEAD 15", "genusCC1:", genusCC[1], "genusCC2:", genusCC[2],
                      "CC:", CC, "dcc1:", dcc1, "xdcc1:", xdcc1, "dcc2:",
                      dcc2, "xdcc2:", xdcc2, "DomType:", DomType, "\n",
                      fill = 80)
      }
    }

    #==========================================================================
    #LEAD 6-7, 16 - 18
    #
    #LEAD 6:  Evergreen trees > 75% of total tree canopy cover.
    #LEAD 7:  Deciduous trees > 75% of total tree canopy cover or deciduous
    #         trees < 75% of total tree canopy cover.
    #LEAD 16: Deciduous tree" subclass (TD).
    #LEAD 17: Mixed evergreen-deciduous tree subclass (TX)
    #LEAD 18: Total canopy cover of shade tolerant trees > canopy cover of
    #         shade intolerant trees or Total canopy cover of shade tolerant
    #         trees < canopy cover of shade intolerant trees.
    #==========================================================================

    if(!domTypeFound)
    {
      #If evergreen trees <= 75% of total tree canopy cover, then determine if
      #DomType is TDMX or TEDX
      if(leafRetenCC["EVERGREEN"] <= CC * 0.75)
      {
        #If Deciduous trees > 75% of total tree canopy cover then DomType is TDMX
        if(leafRetenCC["DECIDUOUS"] > CC * 0.75)
        {
          #Set DomType
          DomType = 'TDMX'

          #Set dcc1 and xdcc1
          dcc1 = names(leafRetenCC[1])
          xdcc1 = leafRetenCC[1]

          #Set dcc2 and xdcc2
          dcc2 = names(leafRetenCC[2])
          xdcc2 = leafRetenCC[2]

          domTypeFound = T

          #LEAD 7
          if(debug) cat("LEAD 7", "CC:", CC, "dcc1:", dcc1, "xdcc1:",
                        xdcc1, "dcc2:", dcc2, "xdcc2:", xdcc2, "DomType:",
                        DomType, "\n", fill = 80)
        }

        #Else, DomType is TEDX
        else
        {
          #Set DomType
          DomType = 'TEDX'

          #Set dcc1 and xdcc1
          dcc1 = names(leafRetenCC[1])
          xdcc1 = leafRetenCC[1]

          #Set dcc2 and xdcc2
          dcc2 = names(leafRetenCC[2])
          xdcc2 = leafRetenCC[2]

          domTypeFound = T

          #LEAD 6
          if(debug) cat("LEAD 6", "CC:", CC, "dcc1:", dcc1, "xdcc1:",
                        xdcc1, "dcc2:", dcc2, "xdcc2:", xdcc2, "DomType:",
                        DomType, "\n", fill = 80)
        }
      }

      #Plot is dominated by evergreens. Determine if TETX or TEIX should be
      #DomType based on shade tolerance of evergreens.
      else
      {
        #If CC of tolerant evergreens > CC of intolerant evergreens then DomType
        #is TETX
        if(shadeTolCC["TOL"] > shadeTolCC["INT"])
        {
          #Set DomType
          DomType = "TETX"

          #Set dcc1 and xdcc1
          dcc1 = names(leafRetenCC[1])
          xdcc1 = leafRetenCC[1]

          #Set dcc2 and xdcc2
          dcc2 = "TOL"
          xdcc2 = shadeTolCC["TOL"]

          domTypeFound = T

          #LEAD 17
          if(debug) cat("LEAD 18", "CC:", CC, "dcc1:", dcc1, "xdcc1:",
                        xdcc1, "dcc2:", dcc2, "xdcc2:", xdcc2, "DomType:",
                        DomType, "\n", fill = 80)
        }

        #Else DomType is TEIX
        else
        {
          #Set DomType
          DomType = "TEIX"

          #Set dcc1 and xdcc1
          dcc1 = names(leafRetenCC[1])
          xdcc1 = leafRetenCC[1]

          #Set dcc2 and xdcc2
          dcc2 = "INT"
          xdcc2 = shadeTolCC["INT"]
          domTypeFound = T

          #LEAD 18
          if(debug) cat("LEAD 17", "CC:", CC, "dcc1:", dcc1, "xdcc1:",
                        xdcc1, "dcc2:", dcc2, "xdcc2:", xdcc2, "DomType:",
                        DomType, "\n", fill = 80)
        }
      }
    }
  }

  #Correct canopy cover values for xdcc1 and xdcc2
  xdcc1<-correctCC(xdcc1)
  xdcc2<-correctCC(xdcc2)

  #Collect results to return in a list:
  #Index 1: DomType
  #Index 2: dcc1
  #Index 3: xdcc1
  #Index 4: dcc2
  #Index 5: xdcc2

  results=list("DOMTYPE" = DomType,
               "DCC1" = dcc1,
               "XDCC1" = xdcc1,
               "DCC2" = dcc2,
               "XDCC2" = xdcc2)

  return(results)
}

#############################################################################
#Function: excGenusSp
#
#This function is used to determine a genus that is mutually exclusive from
#the genus of the most abundant species by percent canopy cover in lead 14 of
#R3 dominance type algorithm. This function takes the name of the most
#abundant species, a vector with names of each genus (sorted by genera with most
#to least canopy cover), and a named vector of genera (not sorted by percent
#canopy cover).
#
#Arguments
#
#sp:        Name of most abundant species by percent canopy cover for current
#           stand in domType function.
#
#spNames:   Vector of genera names ordered by abundance in terms of percent
#           canopy cover for current stand in domType function.
#
#genusList: Vector of genera whose indices are named by each species found
#           in current stand in domType function.
#
#Return value
#
#Index where the name of the most abundant genus does not match the genus in
#the input list of genera.
#############################################################################

excGenusSp<-function(sp, genNames, genusList)
{
  #Initialize boolean flag variable. This variable will be set to true when
  #genus argument and genus found in genusList do NOT match.
  validMatch = F

  #Initialize counter variable
  i = 1

  #Initialize mutual exclusive index to 1
  excIndex = 1

  #Find genus of most abundant species (sp) based on percent canopy cover
  genIndex<-match(sp, names(genusList))

  #Extract genus from genus list
  genus<-genusList[genIndex]

  #Traverse across genNames and look for first instance when genus and genus
  #from genusList do NOT match.
  while(i <= length(genNames) & !validMatch)
  {
    #Extract genus from genera ordered by abundance of percent canopy cover
    genus2<-genNames[i]

    #If genus and genus2 are not the same then validMatch becomes true
    if(genus != genus2)
    {
      excIndex<-i
      validMatch = T
    }

    i = i + 1
  }

  return(excIndex)
}

################################################################################
#Function: domTypeR8
#
#Calculates stand dominance type using USFS Region 8 rule sets determined by
#Chad Keyser in 2023. Five dominance type classifications are calculated in this
#function: dominance type of advanced regeneration, non-merchantable trees,
#pulpwood sized trees, saw timber sized trees, and entire stand. Criteria for
#determining dominance type is as follows:
#
#Advance regeneration dominance
#
#If 1 species represents greater than 70% of trees per acre in advanced
#regeneration size class then dominance type is a single species.
#
#If 2 species represent greater than 70% of trees per acre in advanced
#regeneration size class then dominance type is two species.
#
#Otherwise dominance type is 3 species (3 most abundant species based on TPA).
#
#If there is no TPA in advance regeneration size class, then dominance type is
#set to NONE.
#
#Non merch, pulpwood, sawtimber, and stand dominance
#
#If 1 species represents greater than 70% of basal area in respective size class
#(non merch, pulpwood, sawtimber, and stand) the dominance type is two species.
#
#If 2 species represent greater than 70% of basal area in respective size class
#(non merch, pulpwood, sawtimber, and stand) the dominance type is two species.
#
#Otherwise dominance type is 3 species (3 most abundant species based on BA).
#
#If there is no BA in a given size class, then dominance type is set to NONE
#for that size class.
#
#Example Dominance types:
#
#Single species: ACRU
#Two species:    ACRU-ACSA3
#Three species:  ACRU-ACSA3-NYSY
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
#
#Return value
#
#Named list containing:
# - Dominance type of advanced regeneration (SSDOMSPP)
# - Dominance type of non-merchantable trees (NMDOMSPP)
# - Dominance type of pulpwood sized trees (PWDOMSPP)
# - Dominance type of saw timber sized trees (STDOMSPP)
# - Dominance type of stand (DOMTYPE)
#################################################################################

#'@export
domTypeR8<-function(data,
                    stand = "StandID",
                    attrList,
                    debug = F){

  #Initialize results vector to NA
  results=list("SSDOMSPP" = NA,
               "NMDOMSPP" = NA,
               "PWDOMSPP" = NA,
               "STDOMSPP" = NA,
               "DOMTYPE" = NA)

  #If data has no rows, return
  if(nrow(data) <= 0) return(results)

  #Check for missing columns in data
  missing <- c(stand) %in% colnames(data)

  #If name of column provided in stand is not found in data, warning message is
  #issued and NA values are returned.
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(results)
  }

  #If attribute list is NULL return or length is less than or equal to 1 (ALL
  #species with NA values), return
  if(is.null(attrList) | length(attrList) <= 1)
  {
    return(results)
  }

  #Print stand and columns from data
  if(debug)
  {
    cat("In function domTypeR8", "\n")
    cat("Stand:", unique(data[[stand]]), "\n", "\n")
  }

  #Get ALL SSTPA, NMBA, PWBA, STBA, BA, and TPA
  allSSTPA <- attrList[["ALL"]]["SSTPA"]
  allNMBA <- attrList[["ALL"]]["NMBA"]
  allPWBA <- attrList[["ALL"]]["PWBA"]
  allSTBA <- attrList[["ALL"]]["STBA"]
  allBA <- attrList[["ALL"]]["BA"]
  allTPA <- attrList[["ALL"]]["TPA"]

  #Do debug if true
  if(debug)
  {
    cat("SSTPA:", allSSTPA, "\n")
    cat("NMBA:", allNMBA, "\n")
    cat("PWBA:", allPWBA, "\n")
    cat("STBA:", allSTBA, "\n")
    cat("BA:", allBA, "\n")
    cat("TPA:", allTPA, "\n", "\n")
  }

  #Define vectors that will be used to determine dominance type classes
  #Length is one less than length of attrList since "ALL" species is ignored
  arDomTypeVec <- vector(mode = "numeric", length = length(attrList) - 1)
  nmDomTypeVec <- vector(mode = "numeric", length = length(attrList) - 1)
  pwDomTypeVec <- vector(mode = "numeric", length = length(attrList) - 1)
  stDomTypeVec <- vector(mode = "numeric", length = length(attrList) - 1)
  domTypeVec <- vector(mode = "numeric", length = length(attrList) - 1)

  #Loop across length of attrList
  for(i in 1:length(attrList))
  {
    #Get species
    sp <- names(attrList)[i]

    #If species is "ALL" move to next iteration
    if(sp == 'ALL') next

    #Get SSTPA and put in arDomTypeVec
    SSTPA <- attrList[[sp]]["SSTPA"]
    arDomTypeVec[i] <- SSTPA
    names(arDomTypeVec)[i] <- sp

    #Get NMBA and put in nmDomTypeVec
    NMBA <- attrList[[sp]]["NMBA"]
    nmDomTypeVec[i] <- NMBA
    names(nmDomTypeVec)[i] <- sp

    #Get PWBA and put in pwDomTypeVec
    PWBA <- attrList[[sp]]["PWBA"]
    pwDomTypeVec[i] <- PWBA
    names(pwDomTypeVec)[i] <- sp

    #Get STBA and put in stDomTypeVec
    STBA <- attrList[[sp]]["STBA"]
    stDomTypeVec[i] <- STBA
    names(stDomTypeVec)[i] <- sp

    #Get BA and put in DomTypeVec
    BA <- attrList[[sp]]["BA"]
    domTypeVec[i] <- BA
    names(domTypeVec)[i] <- sp
  }

  #Sort the vectors
  arDomTypeVec <- sort(arDomTypeVec,
                       decreasing = T)

  nmDomTypeVec <- sort(nmDomTypeVec,
                       decreasing = T)

  pwDomTypeVec <- sort(pwDomTypeVec,
                       decreasing = T)

  stDomTypeVec <- sort(stDomTypeVec,
                       decreasing = T)

  domTypeVec <- sort(domTypeVec,
                     decreasing = T)

  #Print dominance type vectors if debug is on
  if(debug)
  {
    #Advance regen
    cat("arDomType:", "\n")
    cat(names(arDomTypeVec), "\n")
    cat(arDomTypeVec, "\n", "\n")

    #Non-merch
    cat("nmDomType:", "\n")
    cat(names(nmDomTypeVec), "\n")
    cat(nmDomTypeVec, "\n", "\n")

    #Pulp
    cat("pwDomType:", "\n")
    cat(names(pwDomTypeVec), "\n")
    cat(pwDomTypeVec, "\n", "\n")

    #Saw
    cat("stDomType:", "\n")
    cat(names(stDomTypeVec), "\n")
    cat(stDomTypeVec, "\n", "\n")

    #Stand
    cat("DomType:", "\n")
    cat(names(domTypeVec), "\n")
    cat(domTypeVec, "\n", "\n")
  }

  #==============================
  #Determine SS dominance type
  #==============================

  #if no trees in this class, set arDomType to NONE
  if(allSSTPA <= 0)
  {
    arDomType <- "NONE"
  }

  #Single species dominance
  else if(arDomTypeVec[1] >= (allSSTPA) * 0.7)
  {
    arDomType <- names(arDomTypeVec[1])
  }

  #Two species dominance
  else if((arDomTypeVec[1] + arDomTypeVec[2]) >= (allSSTPA) * 0.7)
  {
    arDomType <- paste(names(arDomTypeVec[1]),
                       names(arDomTypeVec[2]),
                       sep = "-")
  }

  #Three species dominance
  else
  {
    arDomType <- paste(names(arDomTypeVec[1]),
                       names(arDomTypeVec[2]),
                       names(arDomTypeVec[3]),
                       sep = "-")
  }

  #==============================
  #Determine NM dominance type
  #==============================

  #if no BA in this class, set nmDomType to NONE
  if(allNMBA <= 0)
  {
    nmDomType <- "NONE"
  }

  #Single species dominance
  else if(nmDomTypeVec[1] >= (allNMBA) * 0.7)
  {
    nmDomType <- names(nmDomTypeVec[1])
  }

  #Two species dominance
  else if((nmDomTypeVec[1] + nmDomTypeVec[2]) >= (allNMBA) * 0.7)
  {
    nmDomType <- paste(names(nmDomTypeVec[1]),
                       names(nmDomTypeVec[2]),
                       sep = "-")
  }

  #Three species dominance
  else
  {
    nmDomType <- paste(names(nmDomTypeVec[1]),
                       names(nmDomTypeVec[2]),
                       names(nmDomTypeVec[3]),
                       sep = "-")
  }

  #==============================
  #Determine PW dominance type
  #==============================

  #if no BA in this class, set nmDomType to NONE
  if(allPWBA <= 0)
  {
    pwDomType <- "NONE"
  }

  #Single species dominance
  else if(pwDomTypeVec[1] >= (allPWBA) * 0.7)
  {
    pwDomType <- names(pwDomTypeVec[1])
  }

  #Two species dominance
  else if((pwDomTypeVec[1] + pwDomTypeVec[2]) >= (allPWBA) * 0.7)
  {
    pwDomType <- paste(names(pwDomTypeVec[1]),
                       names(pwDomTypeVec[2]),
                       sep = "-")
  }

  #Three species dominance
  else
  {
    pwDomType <- paste(names(pwDomTypeVec[1]),
                       names(pwDomTypeVec[2]),
                       names(pwDomTypeVec[3]),
                       sep = "-")
  }

  #==============================
  #Determine ST dominance type
  #==============================

  #if no BA in this class, set stDomType to NONE
  if(allSTBA <= 0)
  {
    stDomType <- "NONE"
  }

  #Single species dominance
  else if(stDomTypeVec[1] >= (allSTBA) * 0.7)
  {
    stDomType <- names(stDomTypeVec[1])
  }

  #Two species dominance
  else if((stDomTypeVec[1] + stDomTypeVec[2]) >= (allSTBA) * 0.7)
  {
    stDomType <- paste(names(stDomTypeVec[1]),
                       names(stDomTypeVec[2]),
                       sep = "-")
  }

  #Three species dominance
  else
  {
    stDomType <- paste(names(stDomTypeVec[1]),
                       names(stDomTypeVec[2]),
                       names(stDomTypeVec[3]),
                       sep = "-")
  }

  #=================================
  #Determine stand dominance type
  #=================================

  #Single species dominance
  if(domTypeVec[1] >= (allBA) * 0.7)
  {
    domType <- names(domTypeVec[1])
  }

  #Two species dominance
  else if((domTypeVec[1] + domTypeVec[2]) >= (allBA) * 0.7)
  {
    domType <- paste(names(domTypeVec[1]),
                     names(domTypeVec[2]),
                     sep = "-")
  }

  #Three species dominance
  else
  {
    domType <- paste(names(domTypeVec[1]),
                     names(domTypeVec[2]),
                     names(domTypeVec[3]),
                     sep = "-")
  }

  #Store dominance types in results list
  results[["SSDOMSPP"]] <- arDomType
  results[["NMDOMSPP"]] <- nmDomType
  results[["PWDOMSPP"]] <- pwDomType
  results[["STDOMSPP"]] <- stDomType
  results[["DOMTYPE"]] <- domType

  #Print results if debug is on
  if(debug)
  {
    cat("SSDOMSPP:", arDomType, "\n")
    cat("NMDOMSPP:", nmDomType, "\n")
    cat("PWDOMSPP:", pwDomType, "\n")
    cat("STDOMSPP:", stDomType, "\n")
    cat("DOMTYPE:", domType, "\n")
    cat("SSTPA:", allSSTPA, "\n")
    cat("NMBA:", allNMBA, "\n")
    cat("PWBA:", allPWBA, "\n")
    cat("STBA:", allSTBA, "\n")
    cat("BA:", allBA, "\n", "\n")
  }

  return(results)
}
