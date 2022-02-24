#############################################################################
#Function: domType
#
#CURRENT STATUS: Function returns up to 'LEAD 18' of R3 DomType algorithm. 
#Accuracy stills needs to be verified against known dataset. Need to
#determine shade tolerance for species that do not have a value. May need to
#correct canopy cover for overlap. Also need to determine how many characters
#from genus names should be reported.
#
#Calculate stand dominance type in accordance with NFS Regional Vegetation
#Classification Algorithms Vandendriesche (2013) pg. R3-1.
#
# Dominance base on percentage of total tree canopy cover
#
#Arguments
#stdYrFrame: Data frame consisting of treelist for single year of single 
#            stand. Frame attributes include: StandID, Year, SpeciesPlants,
#            TPA, DBH, TREEBA, TREECC, RunTitle, Age, SPCD (FIA species code),
#            Genus, Leaf_Reten, R3_Shade_Tol, R3_Dia_Meas.
#totalCC:    Percent canopy cover of stand.
#############################################################################

#Plot, 0004201604060100990968, crashed due to other hardwood species not
#having shade tolerance values.

#Species with NA shade tolerance codes based on AZ and NM FIA datasets in 
#relation to species in CR variant.
#2TB (INT)
#ABLA (INT)
#PIMOF (INT)
#2TE (INT)

domType<-function(stdYrFrame, totalCC){
  
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
  #canopy cover (ratio of species or genus percent canopy cover to total percent
  #canopy cover) for dcc1 and dcc2 respectively.
  xdcc1<-0
  xdcc2<-0
  
  #==========================================================================
  #LEAD 1 - 5
  #
  #LEAD 1:     Tree life form > 10% canopy cover
  #LEAD 2 - 5: Ignored. Shrubs and herbs are not accounted for since FVS
  #            does not consider these types of life forms.
  #==========================================================================
  
  if(totalCC < 10)
  {
    DomType = "NVG"
    domTypeFound = T
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
    
    #USDA plant symbols found in stdYrFrame
    sppValues<-unique(stdYrFrame$SpeciesPLANTS)
    # cat("sppValues", sppValues, "\n")
    
    #Genera found in stdYrFrame
    genusValues<-unique(stdYrFrame$GENUS)
    # cat("genusValues", genusValues, "\n")
    
    #Shade tolerance categories found in stdYrFrame
    shadeTolValues<-unique(stdYrFrame$R3_SHADE_TOL)
    # cat("shadeTolValues", shadeTolValues, "\n")
    
    #Vector for storing percent canopy cover for each USDA plant symbol
    sppCC<-rep(0, length(sppValues))
    names(sppCC)<-sppValues
    
    #Vector for storing percent canopy cover for each genera
    genusCC<-rep(0, length(genusValues))
    names(genusCC)<-genusValues
    
    #Vector for storing percent canopy cover for each shade tolerance category
    shadeTolCC<-rep(0, 2)
    names(shadeTolCC)<-c("INT", "TOL")
    
    #Vector for storing percent canopy cover for leaf retention categories
    leafRetenCC<-rep(0, 2)
    names(leafRetenCC)<-c("EVERGREEN", "DECIDUOUS")
    
    #Define genusSpList. This vector keeps track of each genus for each species
    #in stdYrFrame
    genusSp<-vector(length = length(sppValues))
    
    #==========================================================================
    #Loop across stdYrFrame and populate sppCC, genusCC, ShadeTolCC, and
    #leafRetenCC.
    #==========================================================================
    
    #Loop across all unique species found in stand
    for(k in 1:length(sppValues))
    {
      #Extract species being processed
      spp = sppValues[k]
      # cat("Species", spp, "\n")
      
      #Extract genus for spp k
      genus<-stdYrFrame$GENUS[stdYrFrame$SpeciesPLANTS == spp][1]
      # cat("Genus", genus, "\n")
      
      #Extract shade tolerance for spp k
      shadeTol<-stdYrFrame$R3_SHADE_TOL[stdYrFrame$SpeciesPLANTS == spp][1]
      # cat("shadeTol", shadeTol, "\n")
      
      #Extract leaf reten for spp k
      leafReten<-stdYrFrame$LEAF_RETEN[stdYrFrame$SpeciesPLANTS == spp][1]
      # cat("leafReten", leafReten, "\n")
      
      #Obtain sum CC for species k
      SPPC<- sum(stdYrFrame$TREECC[stdYrFrame$SpeciesPLANTS == spp])
      # cat("SPPC for species", spp, SPPC, "\n")
      
      #Add SPPC to sppCC
      sppCC[names(sppCC) == spp]<-sppCC[names(sppCC) == spp] + SPPC
      # cat("SPPC for species", spp, SPPC, "\n")
      
      #Add SPPC to genusCC
      genusCC[names(genusCC) == genus]<-genusCC[names(genusCC) == genus] + SPPC
      # cat("SPPC for genus", genus, SPPC, "\n")
      
      #Bypass addition of SPPC to shaeTolCC if shadeTol for species k is NA.
      if(!is.na(shadeTol))
      {
        #Add SPPC to shadeTolCC
        shadeTolCC[names(shadeTolCC) == shadeTol]<-shadeTolCC[names(shadeTolCC) == shadeTol] +
          SPPC
        # cat("SPPC for ShadeTolCC", shadeTolCC, SPPC, "\n")
      }

      #Add SPPC to leafRetenCC
      leafRetenCC[names(leafRetenCC) == leafReten]<-leafRetenCC[names(leafRetenCC) == leafReten] +
        SPPC
      # cat("SPPC for leafRetenCC", leafRetenCC, SPPC, "\n")
      
      #Add genus to genusSp
      genusSp[k]<-genus
      names(genusSp)[k]<-spp
    }
    
    #Sort sppCC, genusCC, leafRetenCC, and shadeTolCC by descending order
    sppCC<-sort(sppCC, decreasing = T)
    genusCC<-sort(genusCC, decreasing = T)
    leafRetenCC<-sort(leafRetenCC, decreasing = T)
    shadeTolCC<-sort(shadeTolCC, decreasing = T)
    
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
      xdcc1 = (sppCC[1] / totalCC) * 100
      
      #Set domTypeFound to T
      domTypeFound = T
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
        xdcc1 = (sppCC[1] / totalCC) * 100
        
        #Set dcc2 and xdcc2
        dcc2 = names(sppCC[2])
        xdcc2 = (sppCC[2] / totalCC) * 100
        
        domTypeFound = T
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
        xdcc1 = (genusCC[1] / totalCC) * 100
    
        domTypeFound = T
      }
    }
    
    #Check if most abundant genus and most abundant species are the dominance
    #type
    if(!domTypeFound)
    {
      #Find index where species and genus are mutually exclusive
      spIndex<-excGenus(names(genusCC[1]), names(sppCC), genusSp)
      
      if(genusCC[1] >= (totalCC * 0.20) && 
         sppCC[spIndex] >= (totalCC * 0.20) && 
         genusCC[1] + sppCC[spIndex] >= (totalCC * 0.80))
      {
        #Get and sort names of genus and species
        genusSpNames<-sort(c(names(genusCC[1]), names(sppCC[spIndex])), 
                           decreasing = F)
        
        #Separate genus and species name with underscore
        DomType<-paste(genusSpNames, collapse = '_')
        
        #Set DomType
        DomType = names(genusCC[1])
        
        #Set dcc1 and xdcc1
        dcc1 = names(genusCC[1])
        xdcc1 = (genusCC[1] / totalCC) * 100
        
        #Set dcc2 and xdcc2
        dcc2 = names(sppCC[spIndex])
        xdcc2 = (sppCC[spIndex] / totalCC) * 100
        
        domTypeFound = T
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
        xdcc1 = (genusCC[1] / totalCC) * 100
        
        #Set dcc2 and xdcc2
        dcc2 = names(genusCC[2])
        xdcc2 = (genusCC[2] / totalCC) * 100
        
        domTypeFound = T
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
          xdcc1 = (leafRetenCC[1] / totalCC) * 100
          
          #Set dcc2 and xdcc2
          dcc2 = names(leafRetenCC[2])
          xdcc2 = (leafRetenCC[2] / totalCC) * 100
          
          domTypeFound = T
        }
        
        #Else, DomType is TEDX
        else
        {
          #Set DomType
          DomType = 'TEDX'
          
          #Set dcc1 and xdcc1
          dcc1 = names(leafRetenCC[1])
          xdcc1 = (leafRetenCC[1] / totalCC) * 100
          
          #Set dcc2 and xdcc2
          dcc2 = names(leafRetenCC[2])
          xdcc2 = (leafRetenCC[2] / totalCC) * 100

          domTypeFound = T
        }
      }
      
      #Plot is dominated by evergreens. Determine if TETX or TEIX should be 
      #DomType based on shade tolerance of evergreens.
      else
      {
        #If CC of tolerant evergreens > CC of intolerant evergreens then DomType
        #is TEIX
        if(shadeTolCC["TOL"] > shadeTolCC["INT"])
        {
          #Set DomType
          DomType = "TEIX"
          
          #Set dcc1 and xdcc1
          dcc1 = names(leafRetenCC[1])
          xdcc1 = (leafRetenCC[1] / totalCC) * 100
          
          #Set dcc2 and xdcc2
          dcc2 = "TOL"
          xdcc2 = (shadeTolCC["TOL"] / totalCC) * 100
          
          domTypeFound = T
        }
        
        #Else DomType is TETX
        else
        {
          #Set DomType
          DomType = "TETX"
          
          #Set dcc1 and xdcc1
          dcc1 = names(leafRetenCC[1])
          xdcc1 = (leafRetenCC[1] / totalCC) * 100
          
          #Set dcc2 and xdcc2
          dcc2 = "INT"
          xdcc2 = (shadeTolCC["INT"] / totalCC) * 100
          domTypeFound = T
        }
      }
    }
  }
  
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
#Function: excGenus
#
#This function is used to determine if most abundant genus by percent canopy
#cover and most abundant species by most abundant canopy cover are mutually
#exclusive in lead 14 of R3 dominance type algorithm. This function takes
#the name of the most abundant genus, the names of each species found in the
#plot (sorted by percent canopy cover in descending order), and a named list
#of genera. This function returns the index where the name of the most 
#abundant genus does not match the genus in the input list of genera.
#
#Arguments
#genus:     Name of most abundant genus by percent canopy cover for current
#           inventory plot in domType function.
#spNames:   Vector of species names ordered by abundance in terms of percent
#           canopy cover for current inventory plot in domType function.
#genusList: Vector of genera whose indices are named by each species found
#           in current inventory plot in domType function.
#############################################################################

# genus<-"PSEUDOTSUGA"
# spNames<-c("PSME", "PIPO", "PICO")
# genusList<-c("PIPO" = "PINUS", "PICO" = "PINUS", "PSME" = "PSEUDOTSUGA")
# excGenus(genus, spNames, genusList)

excGenus<-function(genus, spNames, genusList)
{
  #Initialize boolean flag variable. This variable will be set to true when
  #genus argument and genus found in genusList do NOT match.
  validMatch = F
  
  #Initialize counter variable
  i = 1
  
  #Initialize mutual exclusive index to 1
  excIndex = 1
  
  #Traverse across spNames and look for first instance when genus and genus from
  #genusList do NOT match.
  while(i <= length(spNames) & !validMatch)
  {
    #Extract species from spNames
    sp<-spNames[i]
    
    #Find index where sp matches the species name in genusList
    spIndex<-match(sp, names(genusList))
    
    #Extract genus from genusList
    genus2<-genusList[spIndex]
    
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
