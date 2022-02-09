#############################################################################
#Function: domType
#CURRENT STATUS: This function is a work in progress.
#
#Calculate stand dominance type in accordance with 
# NFS Regional Vegetation Classification Algorithms
# Vandendriesche (2013) pg. R3-1
#
# Dominance base on percentage of total tree canopy cover

("SELECT TL.CaseID, TL.StandID, TL.Year,
              TL.SpeciesPLANTS, TL.TPA, TL.DBH,
              0.005454*TL.DBH*TL.DBH*TL.TPA as BA,
              (3.141593*TL.CrWidth/2*TL.CrWidth/2)*TL.TPA/43560 * 100 AS TREECC
              FVS_Cases.RunTitle, FVS_Summary2.Age
FROM FVS_TreeList TL INNER JOIN FVS_Cases
 ON FVS_TreeList.CaseID = FVS_Cases.CaseID
INNER JOIN FVS_Summary2
 ON FVS_TreeList.CaseID = FVS_Summary2.CaseID AND
    FVS_TreeList.Year = FVS_Summary2.Year")
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
    #print(spp.out)
    
    # Check if species represents more than 60% of total canopy cover and assign type and set bool flag
    if(spp.out$SPPC > (totalCC * 0.60)){
      DomType = spp.out$Species
      domTypFound = T
    }
    
    #set top two species in stand and add species frame to sppFrm vector for future processing if needed
    else{
      if(spp.out$SPPC > dt1){
        dt1 = spp.out$SPPC
        domType1 = spp.out$Species
      }
      
      if(spp.out$SPPC > dt2 && spp.out$SPPC <= dt1 && spp.out$Species != domType1){
        dt2 = spp.out$SPPC
        domType2 = spp.out$Species
      }
      
      sppFrm[[k]] = spp.out
    }
  }
  
  if(!domTypFound){
    
    if(dt1 >= (totalCC * 0.20) && dt2 >= (totalCC * 0.20) && dt1 + dt2 >= (totalCC * 0.80)){
      DomType<-c(domType1, domType2)
      DomType<-paste(min(DomType), max(DomType), sep = '_')
      domTypFound = T
      #print(DomType)
    }
    
    #Else domType not species based, have to check Genus
    else{
      
    }
  }
}