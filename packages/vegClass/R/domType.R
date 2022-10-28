################################################################################
#Function: domType
#
#Calculate stand dominance type in accordance with NFS Regional Vegetation
#Classification Algorithms Vandendriesche (2013 pg. R3-1 - R3-3).
#
#Arguments
#
#data:    Tree level dataframe corresponding to trees from a single stand.
#
#stand:   Name of column corresponding to stand associated with tree records
#         in data. By default this value is set to "StandID".
#
#species: Name of column corresponding to USDA plant symbols associated with
#         tree records in data.
#
#dbh:     Name of column in data argument corresponding to DBH of tree records.
#         By default this argument is set to "DBH".
#
#expf:    Name of column in data argument corresponding to expansion factor of
#         tree records By default this argument is set to "TPA".
#
#crwidth: Name of column corresponding crown width values of tree records in
#         data. By default this argument is set to "CrWidth".
#
#
#debug:	  Boolean variable used to specify if debug output should be printed to
#         R console. If value is TRUE, then debug output will printed to R
#         console.
#
#Return value
#
#List containing:
# - dominance type (DOMTYPE),
# - dominant species/genus/category or species/genus/category occurring before
#   underscore in dominance type (DCC1)
# - percent canopy cover represented by DCC1 (XDCC1),
# - species/genus/category occurring after underscore in dominance type (DCC2),
# - percent canopy cover represented by DCC2 (XDCC2).
################################################################################

#'@export
domType<-function(data,
                  stand = "StandID",
                  species = "SpeciesPLANTS",
                  dbh = "DBH",
                  expf = "TPA",
                  crwidth = "CrWidth",
                  debug = F){

  #Check of missing columns in data
  missing <- c(stand, expf, crwidth) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Print stand
  if(debug) cat("Stand:", unique(data[[stand]]), "\n")

  if(debug) cat("Columns:", "\n",
                "stand:", stand, "\n",
                "species:", species, "\n",
                "dbh:", dbh, "\n",
                "expf:", expf, "\n",
                "crwidth:", crwidth, "\n", "\n")

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
  #canopy cover for dcc1 and dcc2 respectively.
  xdcc1<-0
  xdcc2<-0

  #Calculate CC
  totalCC <- plotCC(data,
                    stand = stand,
                    dbh = dbh,
                    crwidth = crwidth,
                    expf = expf,
                    type = 2)

  #Calculate TPA
  tpa <- plotTPA(data,
                 stand = stand,
                 dbh = dbh,
                 expf = expf)

  #Calculate TREECC
  data$TREECC <- pi * (data[[crwidth]]/2)^2 *(data[[expf]]/43560) * 100

  #Print CC and TPA if debug is TRUE
  if(debug) cat("CC of plot is", totalCC, "\n")
  if(debug) cat("TPA of plot is", tpa, "\n")

  #Identify unique species in stand
  spStand <- unique(data[[species]])

  #Initialize vector that will store genus value for each species
  genusStand <- vector("character", length(spStand))

  #Initialize vector that will store shade tolertance value for each species
  shadeTolStand <- vector("character", length(spStand))

  #Initialize vector that will store shade leaf reten value for each species
  leafRetStand <- vector("character", length(spStand))

  #Populate vectors
  #GENUS, R3_SHADE_TOL, and LEAF_RETEN exist in commons.R
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

  #If corrected CC is less and then 10 and tpa less than 100, DOMTYPE is NVG
  if(correctCC(totalCC) < 10 & tpa < 100)
  {
    DomType = "NVG"
    domTypeFound = T
    if(debug) cat("LEAD 1-5", "totalCC:", correctCC(totalCC),"LT 10 and",
                  "tpa:", tpa,"LT 100.", "\n", "dcc1:", dcc1,"xdcc1:",xdcc1,
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

    #Define genusSp. This vector keeps track of each genus for each species
    #in data.
    genusSp<-vector(length = length(spStand))

    #==========================================================================
    #Loop across sppValues and populate sppCC, genusCC, ShadeTolCC, and
    #leafRetenCC.
    #==========================================================================

    #Loop across all unique species found in stand
    for(k in 1:length(spStand))
    {
      #Extract species being processed
      spp = spStand[k]

      #Extract genus for spp k
      genus<- genusStand[spp]

      #Extract shade tolerance for spp k
      shadeTol<-shadeTolStand[spp]

      #Extract leaf retention for spp k
      leafReten<-leafRetStand[spp]

      #Obtain sum of CC for species k
      SPPC<- sum(data$TREECC[data[[species]] == spp])

      #Add SPPC to sppCC
      sppCC[spp]<-sppCC[spp] + SPPC

      #Add SPPC to genusCC
      genusCC[genus]<-genusCC[genus] + SPPC

      #Bypass addition of SPPC to shadeTolCC if shadeTol for species k is NA.
      if(!is.na(shadeTol))
      {
        #Add SPPC to shadeTolCC
        shadeTolCC[shadeTol]<-shadeTolCC[shadeTol] + SPPC
      }

      #Add SPPC to leafRetenCC
      leafRetenCC[leafReten]<-leafRetenCC[leafReten] + SPPC

      #Add genus to genusSp
      genusSp[k]<-genus
      names(genusSp)[k]<-spp
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
    #LEAD 11: Canopy cover of single most abundant tree species > 60% of
    #         total tree canopy cover.
    #LEAD 12: Canopy cover of two most abundant tree species > 80% of total
    #         tree canopy cover, each individually > 20% of total tree canopy
    #         cover.
    #========================================================================

    #Test if most abundant species is the dominance type
    if(sppCC[1] > (totalCC * 0.60)){

      #Set dominance type
      DomType = names(sppCC[1])

      #Set dcc1
      dcc1 = names(sppCC[1])

      #Calculate value for dcc1Per
      xdcc1 = sppCC[1]

      #Set domTypeFound to T
      domTypeFound = T

      #Lead 11 debug
      if(debug) cat("LEAD 11", "sppCC:", sppCC[1], "totalCC:", totalCC,
                    "dcc1:", dcc1, "xdcc1:", xdcc1, "DomType:", DomType,
                    "\n", fill = 80)
    }

    if(!domTypeFound)
    {
      #Test if Canopy cover of two most abundant tree species > 80% of total
      #tree canopy cover, each individually > 20% of total tree canopy cover
      if(sppCC[1] >= (totalCC * 0.20) &&
         sppCC[2] >= (totalCC * 0.20) &&
         sppCC[1] + sppCC[2] >= (totalCC * 0.80)){

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
                      "totalCC:", totalCC, "dcc1:", dcc1, "xdcc1:", xdcc1,
                      "dcc2:", dcc2, "xdcc2:", xdcc2,"DomType:", DomType,
                      "\n", fill = 80)
      }
    }

    #========================================================================
    #LEAD 13-14
    #
    #LEAD 13: Canopy cover of single most abundant tree genus > 60% of total
    #         tree canopy cover.
    #LEAD 14: Canopy cover of the single most abundant tree species and
    #         single most abundant tree genus collectively > 80% of total
    #         tree canopy cover, each individually > 20% of total tree canopy
    #         cover (most abundant species and most abundant genus mutually
    #         exclusive).
    #========================================================================

    #Check if most abundant genus is the dominance type
    if(!domTypeFound){
      if(genusCC[1] > (totalCC * 0.60)){

        #Set DomType
        DomType = names(genusCC[1])

        #Set dcc1 and xdcc1
        dcc1 = names(genusCC[1])
        xdcc1 = genusCC[1]

        domTypeFound = T

        #LEAD 13
        if(debug) cat("LEAD 13", "genusCC1:", genusCC[1], "totalCC:", totalCC,
                      "dcc1:", dcc1, "xdcc1:", xdcc1,"DomType:", DomType, "\n",
                      fill = 80)
      }
    }

    #Check if most abundant genus and most abundant species are the dominance
    #type
    if(!domTypeFound)
    {
      #Find index where species and genus are mutually exclusive
      genIndex<-excGenusSp(names(sppCC)[1], names(genusCC), genusSp)

      if(genusCC[genIndex] >= (totalCC * 0.20) &&
         sppCC[1] >= (totalCC * 0.20) &&
         genusCC[genIndex] + sppCC[1] >= (totalCC * 0.80))
      {
        #Get and sort names of genus and species
        genusSpNames<-sort(c(names(genusCC[genIndex]), names(sppCC[1])),
                           decreasing = F)

        #Set DomType
        DomType<-paste(genusSpNames, collapse = '_')

        #Set dcc1 and xdcc1
        dcc1 = names(sppCC[1])
        xdcc1 = sppCC[1]

        #Set dcc2 and xdcc2
        dcc2 = names(genusCC[genIndex])
        xdcc2 = genusCC[genIndex]

        domTypeFound = T

        #LEAD 14
        if(debug) cat("LEAD 14", "sppCC1:", sppCC[1], "genusCC:", sppCC[genIndex],
                      "totalCC:", totalCC, "dcc1:", dcc1, "xdcc1:", xdcc1, "dcc2:",
                       dcc2, "xdcc2:", xdcc2, "DomType:", DomType, "\n",
                      fill = 80)
      }
    }

    #==========================================================================
    #LEAD 15: Canopy cover of two most abundant tree genera > 80% of total tree
    #         canopy cover, each individually > 20% of total tree canopy cover.
    #==========================================================================

    if(!domTypeFound)
    {
      if(genusCC[1] >= (totalCC * 0.20) &&
         genusCC[2] >= (totalCC * 0.20) &&
         genusCC[1] + genusCC[2] >= (totalCC * 0.80)){

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
                      "totalCC:", totalCC, "dcc1:", dcc1, "xdcc1:", xdcc1, "dcc2:",
                      dcc2, "xdcc2:", xdcc2, "DomType:", DomType, "\n",
                      fill = 80)
      }
    }

    #==========================================================================
    #LEAD 6-7, 16 - 18
    #
    #LEAD 6:  Evergreen trees > 75% of total tree canopy cover.
    #LEAD 7:  Deciduous trees > 75% of total tree canopy cover or Deciduous
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
      if(leafRetenCC["EVERGREEN"] <= totalCC * 0.75)
      {
        #If Deciduous trees > 75% of total tree canopy cover then DomType is TDMX
        if(leafRetenCC["DECIDUOUS"] > totalCC * 0.75)
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
          if(debug) cat("LEAD 7", "totalCC:", totalCC, "dcc1:", dcc1, "xdcc1:",
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
          if(debug) cat("LEAD 6", "totalCC:", totalCC, "dcc1:", dcc1, "xdcc1:",
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
          if(debug) cat("LEAD 18", "totalCC:", totalCC, "dcc1:", dcc1, "xdcc1:",
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
          if(debug) cat("LEAD 17", "totalCC:", totalCC, "dcc1:", dcc1, "xdcc1:",
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
#abundant species, the names of each genus (sorted by percent canopy cover
#in descending order), and a named list of genera (unsorted).
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

  # cat("genIndex:", genIndex,
  #     "genus:", genus,
  #     "\n")

  #Traverse across genNames and look for first instance when genus and genus from
  #genusList do NOT match.
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
