#############################################################################
#Function: domType
#CURRENT STATUS: This function is a work in progress.
# AS OF Feb 11 12:15 EST, Function returns up to 'LEAD 15' of R3 DomType 
# algorithm.  Accuracy stills needs to be verified against known dataset
#
#Calculate stand dominance type in accordance with 
# NFS Regional Vegetation Classification Algorithms
# Vandendriesche (2013) pg. R3-1
#
# Dominance base on percentage of total tree canopy cover
#
#Arguments
#stdYrFrame:      Dataframe consisting of treelist for single year
#                   of single stand.  Frame attributes include:
#                   StandID, Year, SpeciesPlants, TPA, DBH, BA, TREECC,
#                   RunTitle, Age
#
#spInfo:          Dataframe of relevant species information including:
#                   SpeciesFVS, Genus, Leaf_Reten, R3_Shade_Tol, R3_Dia_Meas
#                     
#
#############################################################################

domTypeFunction<-function(stdYrFrame, spInfo){
  
  #calculate total tree canopy cover for the stand-year
  totalCC<-sum(stdYrFrame$TREECC)
  domTypFound = F
  domType1 = ''
  domType2 = ''
  dt1 = 0
  dt2 = 0
  
  #create list of unique species
  sppList<-unique(stdYrFrame$SpeciesPLANTS)
  sppFrm<-vector(mode = "list", length(sppList))
  
  for(k in 1:length(sppList))
  {
    temp<-stdYrFrame[stdYrFrame$SpeciesPLANTS == sppList[k], ]
    spp.out<-data.frame(Species = sppList[k], subset(sp.info, SpeciesFVS == sppList[k], select = c("GENUS", "LEAF_RETEN", "R3_SHADE_TOL")))
    spp.out$SPPC = sum(temp$TREECC)
    
    # Check if species represents more than 60% of total canopy cover and assign type and set bool flag
    if(spp.out$SPPC > (totalCC * 0.60)){
      DomType = spp.out$Species
      domTypFound = T
    }
    
    #set top two species in stand and add species frame to sppFrm vector for future processing if needed
    else if(!domTypFound){
      if(spp.out$SPPC > dt1){
        if(dt1 > dt2){
          domType2 = domType1
          dt2 = dt1
        }
        dt1 = spp.out$SPPC
        domType1 = spp.out$Species
      }
      
      else if(spp.out$SPPC > dt2 && 
              spp.out$SPPC <= dt1){
        dt2 = spp.out$SPPC
        domType2 = spp.out$Species
      }
      
      sppFrm[[k]] = spp.out
    }
  }
  
  ###############################################################################
  #If no single species dominance type found, 
  #Check if two highest species are each >= 20% total canopy cover,
  #and sum to at least 80% of total canopy cover
  ###############################################################################
  if(!domTypFound){
    
    if(dt1 >= (totalCC * 0.20) && 
       dt2 >= (totalCC * 0.20) && 
       dt1 + dt2 >= (totalCC * 0.80)){
      DomType<-c(domType1, domType2)
      DomType<-paste(min(DomType), max(DomType), sep = '_')
      domTypFound = T
    }
    
    #Else domType not species based, have to check Genus
    else if(!domTypFound){
      #take list of spp dataframes and condense into single dataframe
      YrSppFrm<-do.call("rbind", sppFrm)
      
      #generate list of unique genus, keep domType1 as larger species canopy cover, 
      #Store genus of domType1 species, 
      #reset domType2 and dt2 for use in storing largest genus, in case of need
      GeneraList<-unique(YrSppFrm$GENUS)
      genusOfDomType1 = YrSppFrm[YrSppFrm$Species == domType1, ]$GENUS
      domType2 = ''
      dt2 = 0
      
      #initialize variables to hold top 2 genera in event dominance is not single species w/ single genus
      domGen1 = ''
      dg1 = 0
      domGen2 = ''
      dg2 = 0
      
      ####################################################################################
      #STILL NEEDS TESTING
      ####################################################################################
      #iterate through genus list
      for(k in 1:length(GeneraList)){
        #check for single genus > 60% total CC
        if(sum(YrSppFrm[YrSppFrm$GENUS == GeneraList[k],]$SPPC) > (totalCC * 0.60)){
          DomType = GeneraList[k]
          domTypFound = T
        }
        
        #if single genus dominance not found, 
        else if(!domTypFound){
          currentGenusSPPC = sum(YrSppFrm[YrSppFrm$GENUS == GeneraList[k], ]$SPPC)
          
          #Determine if genus in position 'k' is largest found genus, exclusive of genus of dominant single species type
          if(currentGenusSPPC > dt2 && GeneraList[k] != genusOfDomType1){
            domType2 = GeneraList[k]
            dt2 = sum(YrSppFrm[YrSppFrm$GENUS == GeneraList[k], ]$SPPC)
          }
          
          #Determine and save genra with largest canopy cover in case dominance type is two genera based description
          if(currentGenusSPPC > dg1){
            if(dg1 > dg2){
              domGen2 = domGen1
              dg2 = dg1
            }
            dg1 = currentGenusSPPC
            domGen1 = GeneraList[k]
          }
          
          else if(currentGenusSPPC > dg2 && 
                  currentGenusSPPC <= dg1){
            dg2 = currentGenusSPPC
            domGen2 = GeneraList[k]
          }
        }
      } # END OF ITERATION THROUGH GENERALIST
    }
    
    #print(domTypFound)
    #If no dominance type yet found
    if(!domTypFound){
      
      #Check if dominant species (domType1 and dt1) 
      #and dominant genus (not of dominant species genus) (domType2 and dt2) 
      #are each at least 20% of canopy cover and 
      #total at least 80% of canopy cover
      
      if(dt1 >= (totalCC * 0.20) && 
         dt2 >= (totalCC * 0.20) && 
         dt1 + dt2 >= (totalCC * 0.80)){
        
        DomType<-c(domType1, domType2)
        DomType<-paste(min(DomType), max(DomType), sep = '_')
        domTypFound = T
        #print(DomType)
      }
      
      else if(dg1 >= (totalCC * 0.20) && 
              dg2 >= (totalCC * 0.20) && 
              dg1 + dg2 >= (totalCC * 0.80)){
        
        DomType<-c(domGen1, domGen2)
        DomType<-paste(min(DomType), max(DomType), sep = '_')
        domTypFound = T
        print(DomType)
      }
    }
  }
  return(DomType)
}