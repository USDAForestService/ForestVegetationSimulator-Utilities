################################################################################
#Function: R2
#
#Calculates variables produced in the R2Calcs application and in the hss_wi5.kcp,
#found at https://www.fs.usda.gov/fvs/software/addfiles/hss_wi5.zip
#
#Arguments
#
#data:    Data frame containing tree records from a single stand or plot. Data
#         frame must contain a column corresponding to stand/plot ID, DBH,
#         species code (USDA plant symbol), expansion factor, and height
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
#ht:      Character string corresponding to name of column pertaining to Ht of
#         tree records in data argument. By default, this argument is set to
#         "Ht".
#
#TPA:     TPA of stand/plot.
#
#BA:      Basal area of stand/plot.
#
#CC:      Percent canopy cover of stand/plot.
#
#plotvals: Named list containing plot attributes for each species in plot/stand
#         and/or across all species in plot/stand.
#
#debug:	  Logical variable used to specify if debug output should be printed to
#         R console. If value is TRUE, then debug output will printed to R
#         console.
#
#Return value
#
#Named list containing
# - Dominance type reported as the top 3 species (DOM_TYPE_R2)
# - Percent canopy cover of the most dominant species in the stand (DOM_TYPE_R2_CC1)
# - Percent canopy cover of the 2nd most dominant species in the stand (DOM_TYPE_R2_CC2)
# - Percent canopy cover of the 3rd most dominant species in the stand (DOM_TYPE_R2_CC3)
# - R2 Cover Type (COVERTYPE_R2)
# - R2 tree size class (TREE_SIZE_CLASS_R2)
# - R2 percent canopy cover class (CROWN_CLASS_R2)
# - Habitat structural stage calculated and reported for the 1 through 4C classes (HSS1_4C)
# - Habitat structural stage calculated and reported for the 1 through 5 classes (HSS1_5)
################################################################################
#'@export
R2<-function(data,
                  stand = "StandID",
                  species = "SpeciesPLANTS",
                  dbh = "DBH",
                  expf = "TPA",
                  ht = "Ht",
                  TPA,
                  BA,
                  CC,
                  plotvals,
                  debug = F){

  #Calculate the number of trees in the output treelist for the plot
  Ntrees <- nrow(data)

  #Initialize results vector to NA
  results=list("DOM_TYPE_R2" = NA,
               "DOM_TYPE_R2_CC1" = NA,
               "DOM_TYPE_R2_CC2" = NA,
               "DOM_TYPE_R2_CC3" = NA,
               "COVERTYPE_R2" = NA,
               "TREE_SIZE_CLASS" = NA,
               "CROWN_CLASS" = NA,
               "HSS1_4C" = NA,
               "HSS1_5" = NA)

  #Check for missing columns in data
  missing <- c(stand, species, dbh, expf, ht) %in% colnames(data)

  #If name of columns provided in stand, species, dbh, expf, and ht  are not
  #found in data warning message is issued and NA values are returned.
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(results)
  }

  #Print stand
  if(debug)
  {
    cat("In function R2", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("Columns:", "\n",
                "stand:", stand, "\n",
                "species:", species, "\n",
                "dbh:", dbh, "\n",
                "expf:", expf, "\n",
                "ht:", ht, "\n","\n")
  }

  #Initialize return attribute variables as NA
  DOM_TYPE_R2<-NA
  DOM_TYPE_R2_CC1<-NA
  DOM_TYPE_R2_CC2<-NA
  DOM_TYPE_R2_CC3<-NA
  COVERTYPE_R2<-NA
  TREE_SIZE_CLASS<-NA
  CROWN_CLASS<-NA
  HSS1_4C<-NA
  HSS1_5<-NA

  #Print plot BA and TPA if debug is TRUE
  if(debug) cat("BA of plot is", BA, "\n")
  if(debug) cat("TPA of plot is", TPA, "\n")

  ## Initialize the species-level vectors
  #Identify unique species in stand
  spStand <- unique(data$SpeciesPLANTS)
  #Initialize vector that will store CC value for each species
  SpeciesCC <- vector("numeric", length(spStand))

  #Stand-level canopy cover percent
  STCC <- round(plotvals[["ALL"]]["CC"][[1]][1],2)

  if(STCC < 10){
    DOM_TYPE_R2 <- "NONE"
    DOM_TYPE_R2_CC1 <- "NONE"
    DOM_TYPE_R2_CC2 <- "NONE"
    DOM_TYPE_R2_CC3 <- "NONE"
    TREE_SIZE_CLASS <- "n"
    CROWN_CLASS <- "0"
    COVERTYPE_R2 <- "NONE"
  }
  if(STCC >= 10){
#Calculate COVERTYPE_R2
#Determine DOM_TYPE, DOM_TYPE_R2_CC1, DOM_TYPE_R2_CC2, and DOM_TYPE_R2_CC3
    #Populate SpeciesCC
    for(i in 1:length(spStand)){
      SpeciesCC[i] <- plotvals[[spStand[i]]]["CC"]
    }
    #Get species names associated with each SpeciesCC value
    names(SpeciesCC) <- subset(names(plotvals),names(plotvals)!="ALL")

    #Sort SpeciesCC from largest to smallest CC value
    SpeciesCC <- sort(SpeciesCC,decreasing = T)

    if(length(SpeciesCC)>=3){
      SP1 <- names(SpeciesCC[1])
      SP2 <- names(SpeciesCC[2])
      SP3 <- names(SpeciesCC[3])
      DOM_TYPE_R2 <- paste0(SP1,":",SP2,":",SP3)
      DOM_TYPE_R2_CC1 <- SpeciesCC[[1]]
      DOM_TYPE_R2_CC2 <- SpeciesCC[[2]]
      DOM_TYPE_R2_CC3 <- SpeciesCC[[3]]
    }
    else if(length(SpeciesCC)==2){
      SP1 <- names(SpeciesCC[1])
      SP2 <- names(SpeciesCC[2])
      SP3 <- ""
      DOM_TYPE_R2 <- paste0(SP1,":",SP2)
      DOM_TYPE_R2_CC1 <- SpeciesCC[[1]]
      DOM_TYPE_R2_CC2 <- SpeciesCC[[2]]
      DOM_TYPE_R2_CC3 <- 0
    }
    else{
     SP1 <- names(SpeciesCC[1])
     SP2 <- ""
     SP3 <- ""
     DOM_TYPE_R2 <- SP1
     DOM_TYPE_R2_CC1 <- SpeciesCC[[1]]
     DOM_TYPE_R2_CC2 <- 0
     DOM_TYPE_R2_CC3 <- 0
    }
    safsrm <- ""
    totsf <- 0
    totpj <- 0
    totdf <- 0
    #ASPEN (SAF 217)
    if(SP1=="POTR5") safsrm <- "T217"
    #BLUE SPRUCE (SAF 216)
    if(SP1=="PIPU") safsrm <- "T216"
    #BRISTLECONE PINE (SAF 209)
    if(SP1=="PIAR") safsrm <- "T209"
    #BUR OAK (SAF 236)
    if(SP1=="QUMA2") safsrm <- "T236"
    #COTTONWOOD (SAF 235 Assumptions are includes balsam poplar)
    if(SP1=="POAN3" || SP1=="PODE3" || SP1=="POAC5" || SP1=="PODEW" ||
       SP1=="POAC5" || SP1=="POFR2" || SP1=="PODEM" || SP1=="POSA" ||
       SP1=="POBA2" || SP1=="POPUL") safsrm <- "T235"
    #DOUGLAS-FIR (SAF 210)
    if(SP1=="PSME" || SP1=="PSMEG") safsrm <- "T210"
    if(SP1=="ABCO"){
      if(SP2=="PSME" || SP2=="PSMEG") safsrm <- "T210"
      if(SP3=="PSME" || SP3=="PSMEG") safsrm <- "T210"
    }
    #EASTERN RED-CEDAR (SAF 045, Nebraska)
    if(SP1=="JUVI") safsrm <- "T045"
    #GAMBEL OAK (SRM 413 See SOPJ)
    if(SP1=="QUGA" && SP1!="PIED" && SP1!="JUSC2" && SP1!="JUNIP" &&
       SP1!="JUOS" && SP1!="JUMO" && SP1!="SAUT3") safsrm <- "S413"
    #GREEN ASH (SAF ???)
    if(SP1=="FRPE") safsrm <- "TASH"
    #JACK PINE (SAF 001, Nebraska)
    if(SP1=="PIBA2") safsrm <- "T001"
    #JEFFREY PINE (SAF 247)
    if(SP1=="PIJE") safsrm <- "T247"
    #LODGEPOLE PINE (SAF 218)
    if(SP1=="PICO" || SP1=="PICOL") safsrm <- "T218"
    #LIMBER PINE (SAF 219)
    if(SP1=="PIFL2") safsrm <- "T219"
    #PAPER BIRCH (SAF 252)
    if(SP1=="BEPA") safsrm <- "T252"
    #PINYON/JUNIPER (SAF 239)
    if(SP1=="PIED" || SP1=="JUSC2" || SP1=="SAUT3" || SP1=="JUNIP" ||
       SP1=="JUOS" || SP1=="JUMO") safsrm <- "T239"
    #PONDEROSA PINE (SAF 237)
    if(SP1=="PIPO" || SP1=="PIPOS" || SP1=="PIPOS2") safsrm <- "T237"
    #SALT CEDAR TREE FORM (NON-SRM TSCD)
    if(SP1=="TARA") safsrm <- "TSCD"
    #SCOTCH PINE (SAF ???)
    if(SP1=="PISY") safsrm <- "TSCP"
    #SPRUCE / FIR (SAF 206)
    if(SP1=="PIEN" || SP1=="ABLA" || SP1=="ABLAA" || SP1=="ABAR2" ||
       SP1=="ABBI2") safsrm <- "T206"
    #SOUTHWESTERN WHITE PINE (SAF ???)
    if(SP1=="PIST3") safsrm <- "TSWP"
    #WHITE FIR (SAF 211 - pure white fir and not any Douglas-fir)
    if(SP1=="ABCO"){
      if(SP2!="PSME" && SP2!="PSMEG" && SP3!="PSME" && SP3!="PSMEG") safsrm <- "T211"
    }
    #WHITEBARK PINE (SAF 208)
    if(SP1=="PIAL") safsrm <- "T208"
    #WHITE SPRUCE (SAF 201)
    if(SP1=="PIGL") safsrm <- "T201"

  #TREE GROUPING OVERRIDES (SAF)
    #SPRUCE / FIR (SAF 206)
    if(SP1=="PIEN" || SP1=="ABLA" || SP1=="ABLAA" || SP1=="ABAR2" ||
       SP1=="ABBI2") totsf <- totsf + DOM_TYPE_R2_CC1
    if(SP2=="PIEN" || SP2=="ABLA" || SP2=="ABLAA" || SP2=="ABAR2" ||
       SP2=="ABBI2") totsf <- totsf + DOM_TYPE_R2_CC2
    if(SP3=="PIEN" || SP3=="ABLA" || SP3=="ABLAA" || SP3=="ABAR2" ||
       SP3=="ABBI2") totsf <- totsf + DOM_TYPE_R2_CC3
    if(totsf > DOM_TYPE_R2_CC1 && totsf > DOM_TYPE_R2_CC2 &&
       totsf > DOM_TYPE_R2_CC3 && totsf > 0) safsrm <- "T206"
    #PINYON/JUNIPER (SAF 239)
    if(SP1=="PIED" || SP1=="JUSC2" || SP1=="SAUT3" || SP1=="JUNIP" ||
       SP1=="JUOS" || SP1=="JUMO") totpj <- totpj + DOM_TYPE_R2_CC1
    if(SP2=="PIED" || SP2=="JUSC2" || SP2=="SAUT3" || SP2=="JUNIP" ||
       SP2=="JUOS" || SP2=="JUMO") totpj <- totpj + DOM_TYPE_R2_CC2
    if(SP3=="PIED" || SP3=="JUSC2" || SP3=="SAUT3" || SP3=="JUNIP" ||
       SP3=="JUOS" || SP3=="JUMO") totpj <- totpj + DOM_TYPE_R2_CC3
    if(totpj > DOM_TYPE_R2_CC1 && totpj > DOM_TYPE_R2_CC2 &&
       totpj > DOM_TYPE_R2_CC3 && totpj > 0) safsrm <- "T239"
    #DOUGLAS-FIR (SAF 210)
    if(SP1=="PSME" || SP1=="PSMEG" || SP1=="ABCO") totdf <- totdf + DOM_TYPE_R2_CC1
    if(SP2=="PSME" || SP2=="PSMEG" || SP2=="ABCO") totdf <- totdf + DOM_TYPE_R2_CC2
    if(SP3=="PSME" || SP3=="PSMEG" || SP3=="ABCO") totdf <- totdf + DOM_TYPE_R2_CC3
    if(totdf > DOM_TYPE_R2_CC1 && totdf > DOM_TYPE_R2_CC2 &&
       totdf > DOM_TYPE_R2_CC3 && totdf > 0) safsrm <- "T210"
    if(SP1=="2TB" || SP1=="2TN") safsrm <- "T999"
    saf2R2 <- c(
      "T217"="TAA","T206"="TSF","T210"="TDF","T235"="TCW","T239"="TPJ",
      "T211"="TWF","T216"="TBS","T218"="TLP","T219"="TLI","T237"="TPP",
      "T209"="TBC","T236"="TBO","TASH"="TAE","T045"="TER","T001"="TJP",
      "T247"="TJF","T252"="TPB","TSCD"="TSC","TSCP"="TSP","TSWP"="TWP",
      "T208"="TWB","T201"="TWS","S413"="TGO","T999"="OTH"
    )
    COVERTYPE_R2 <- as.character(saf2R2[as.character(safsrm)])

    #Calculate TREE_SIZE_CLASS
    #Initialize canopy cover by diameter class vector
    CCByDiameterClass <- vector("numeric",6)
    #Populate CCByDiameterClass vector
    for (i in 1:Ntrees){
      if(data$DBH[i] >= 0.0 && data$DBH[i] < 1.0)
        CCByDiameterClass[1] <- CCByDiameterClass[1] + data$TREECC[i]
      if(data$DBH[i] >= 1.0 && data$DBH[i] < 5.0)
        CCByDiameterClass[2] <- CCByDiameterClass[2] + data$TREECC[i]
      if(data$DBH[i] >= 5.0 && data$DBH[i] < 9.0)
        CCByDiameterClass[3] <- CCByDiameterClass[3] + data$TREECC[i]
      if(data$DBH[i] >= 9.0 && data$DBH[i] < 16.0)
        CCByDiameterClass[4] <- CCByDiameterClass[4] + data$TREECC[i]
      if(data$DBH[i] >= 16.0)
        CCByDiameterClass[5] <- CCByDiameterClass[5] + data$TREECC[i]
    }
    covTsize_E <- CCByDiameterClass[1]
    covTsize_S <- CCByDiameterClass[2]
    covTsize_M <- CCByDiameterClass[3]
    covTsize_L <- CCByDiameterClass[4]
    covTsize_V <- CCByDiameterClass[5]

    grpSizeClass<-NA
    GroupES <- covTsize_E + covTsize_S
    GroupM <- covTsize_M
    GroupLV <- covTsize_L + covTsize_V

    #If the 9"+ category is greater than 0, grpSizeClass <- "lv"
    if(GroupLV > 0) grpSizeClass <- "lv" #may be reset below

    #If the 5-9" category is greater than 9"+, grpSizeClass <- "m"
    if(covTsize_M > GroupLV) grpSizeClass <- "m"

    #If the 0-5" category is greater than 5-9" category, grpSizeClass <- "es"
    if(GroupES > GroupM) grpSizeClass <- "es"

    if(!is.na(grpSizeClass) && grpSizeClass=="lv")grpSizeClass <- if(covTsize_V >= covTsize_L) "v" else "l"
    if(!is.na(grpSizeClass) && grpSizeClass=="es")grpSizeClass <- if(covTsize_S >= covTsize_E) "s" else "e"

    TREE_SIZE_CLASS <- if(is.na(grpSizeClass) && STCC < 10) "n" else grpSizeClass

    #Calculate CROWN_CLASS
    if(STCC >= 10 && STCC < 40) CROWN_CLASS <- 1
    if(STCC >= 40 && STCC < 70) CROWN_CLASS <- 2
    if(STCC >= 70) CROWN_CLASS <- 3
  }
# #Calculate TREE_SIZE_CLASS: MODIFIED MPSG METHOD TEST
#   #Initialize canopy cover by diameter class vector
#   CCByDiameterClass <- vector("numeric",6)
#   #Populate CCByDiameterClass vector
#   for (i in 1:Ntrees){
#     if(data$DBH[i] >= 0.0 && data$DBH[i] < 5.0)
#       CCByDiameterClass[1] <- CCByDiameterClass[1] + data$TREECC[i]
#     if(data$DBH[i] >= 5.0 && data$DBH[i] < 10.0)
#       CCByDiameterClass[2] <- CCByDiameterClass[2] + data$TREECC[i]
#     if(data$DBH[i] >= 10.0 && data$DBH[i] < 15.0)
#       CCByDiameterClass[3] <- CCByDiameterClass[3] + data$TREECC[i]
#     if(data$DBH[i] >= 15.0 && data$DBH[i] < 20.0)
#       CCByDiameterClass[4] <- CCByDiameterClass[4] + data$TREECC[i]
#     if(data$DBH[i] >= 20.0 && data$DBH[i] < 25.0)
#       CCByDiameterClass[5] <- CCByDiameterClass[5] + data$TREECC[i]
#     if(data$DBH[i] >= 25.0)
#       CCByDiameterClass[6] <- CCByDiameterClass[6] + data$TREECC[i]
#   }
#
#   Cs2Remove <- list()
#   #Identify possible classes to remove from the treelist
#   LT10s <- which(CCByDiameterClass <10 & CCByDiameterClass>0)
#   if(length(LT10s)>0){
#     for(lt in 1:length(LT10s)){
#      ithDClass <- LT10s[lt]
#      if(ithDClass==1){#If it's the 1st class
#        #and the 1st and 2nd class sum to less than 10PCC, record the class for removal
#        if((CCByDiameterClass[ithDClass] + CCByDiameterClass[ithDClass+1])<10) Cs2Remove <- c(Cs2Remove,ithDClass)
#      }else if (ithDClass==6){#If it's the 6th class
#        #and the 6th and the 5th class sum to less than 10PCC, record the class for removal
#        if((CCByDiameterClass[ithDClass] + CCByDiameterClass[ithDClass-1])<10) Cs2Remove <- c(Cs2Remove,ithDClass)
#      }else{#If it's any of the middle classes
#        #and either the class before or the class after sum to less than 10PCC, record the class for removal
#        if((CCByDiameterClass[ithDClass] + CCByDiameterClass[ithDClass-1])<10 &&
#          (CCByDiameterClass[ithDClass] + CCByDiameterClass[ithDClass+1])<10) Cs2Remove <- c(Cs2Remove,ithDClass)
#      }
#     }
#   }
#
#   Datafull <- data
#   #Subset out dead tree records from data
#   data <- subset(data, MortPA==0)
#   #Subset the tree list, if necessary, before calculating MPSG_TSC (BAWTDBH)
#   if(length(Cs2Remove)>0){
#     for(Rmv in 1:length(Cs2Remove)){
#       if(Cs2Remove[Rmv]==1) data <- subset(data,DBH>=5.0)
#       if(Cs2Remove[Rmv]==2) data <- subset(data,DBH<5.0 | DBH>=10.0)
#       if(Cs2Remove[Rmv]==3) data <- subset(data,DBH<10.0 | DBH>=15.0)
#       if(Cs2Remove[Rmv]==4) data <- subset(data,DBH<15.0 | DBH>=20.0)
#       if(Cs2Remove[Rmv]==5) data <- subset(data,DBH<20.0 | DBH>=25.0)
#       if(Cs2Remove[Rmv]==6) data <- subset(data,DBH<25.0)
#     }
#   }
#
#   #Calculate MPSG_TSC (BAWTDBH) using the (possibly) subset data
#   if(nrow(data)==0 && nrow(Datafull)>0) data <- Datafull
#   data$BADBH <- 0
#   for(bawd in 1:nrow(data)){
#     data$TREEBA[bawd] <- data$DBH[bawd]^2 * data$TPA[bawd] * 0.005454154
#     data$BADBH[bawd] <- data$DBH[bawd]*data$TREEBA[bawd]
#   }
#   MPSG_TSC <- if(BA>0)round((sum(data$BADBH)/BA),2) else 0

# #Calculate CANOPY_LAYERS
#   #Initialize basal area by diameter class vector
#   BAByDiameterClass <- vector("numeric",6)
#
#   #Populate BAByDiameterClass vector
#   for (i in 1:nrow(data)){
#     if(data$DBH[i] >= 0.0 && data$DBH[i] < 5.0)
#       BAByDiameterClass[1] <- BAByDiameterClass[1] + data$TREEBA[i]
#     if(data$DBH[i] >= 5.0 && data$DBH[i] < 10.0)
#       BAByDiameterClass[2] <- BAByDiameterClass[2] + data$TREEBA[i]
#     if(data$DBH[i] >= 10.0 && data$DBH[i] < 15.0)
#       BAByDiameterClass[3] <- BAByDiameterClass[3] + data$TREEBA[i]
#     if(data$DBH[i] >= 15.0 && data$DBH[i] < 20.0)
#       BAByDiameterClass[4] <- BAByDiameterClass[4] + data$TREEBA[i]
#     if(data$DBH[i] >= 20.0 && data$DBH[i] < 25.0)
#       BAByDiameterClass[5] <- BAByDiameterClass[5] + data$TREEBA[i]
#     if(data$DBH[i] >= 25.0)
#       BAByDiameterClass[6] <- BAByDiameterClass[6] + data$TREEBA[i]
#   }
#
#   #Weight BAByDiameterClass vector by BA
#   for (jdiam in 1:length(BAByDiameterClass) ){
#     BAByDiameterClass[jdiam] <- round((BAByDiameterClass[jdiam]/BA),2)
#   }
#
#   #compute VERTICAL_STRUCTURE
#   if(BA < 20 && TPA < 100){
#     VERTICAL_STRUCTURE <- "NONE"
#   }
#   else if(BA < 20 && TPA >= 100){
#     VERTICAL_STRUCTURE <- 1
#   }
#   else {
#     VERTICAL_STRUCTURE <- computeVerticalStructure(BAByDiameterClass)
#   }

#NOTE: the commented code below is no longer used.
# #Calculate HAB_STR_STAGE (translation of hss_wi5.kcp, found
# #at https://www.fs.usda.gov/fvs/software/addfiles/hss_wi5.zip)
#   vol1 <- "TCuFt"
#   vol2 <- "MCuFt"
#   vol3 <- "BdFt"
#   #Stand-level TPA
#   TPA0  <- round(plotvals[["ALL"]]["TPA"][[1]][1],2)
#   #Stand-level BA
#   BA0 <- round(plotvals[["ALL"]]["BA"][[1]][1],2)
#   #BA in trees > 1" DBH
#   BA1 <- plotAttr(data,region = 2,min = 1,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
#   #BA in trees >= 5" DBH
#   BA5 <- plotAttr(data,region = 2,min = 5,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
#   #BA in trees >= 9" DBH
#   BA9 <- plotAttr(data,region = 2,min = 9,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
#   #BA in trees >= 16" DBH
#   BA16 <- plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
#   #BA in trees >= 1" DBH and < 5" DBH
#   BA125 <- plotAttr(data,region = 2,min = 1,max=5,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
#   #BA in trees >= 5" DBH and < 9" DBH
#   BA529 <- plotAttr(data,region = 2,min = 5,max=9,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
#   #QMD in trees >= 1" DBH
#   QMD <- plotAttr(data,region = 2,min = 1,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["QMD"][[1]][1]
#   BARAT <- 0
#   TPAMAX <- 0
#   TPAMIN <- 300
#   BAMAX <- 0
#   BAMIN <- 0
#   CHESS <- 10
#   OGSC <- 0
#   TSC <- 0
#   PHS_H <- 0
#   A <- B <- C <- D <- E <- F2 <- G <- H <- I <- J <- K <- L <- 0
#
#   if(PHS_H==0)PHS_H <- PHS_H + 1
#   if(PHS_H==1 && BA9>0)BARAT <- BA16/BA9
#   if(PHS_H==1 && QMD>0){
#     TPAMAX <- 18641.0/(QMD**1.659925)
#     BAMAX <- 101.67*(QMD**0.34007)
#     BAMIN <- BAMAX*0.10
#   }
#   if(PHS_H==1)PHS_H <- PHS_H + 1
#   if(PHS_H==2 && BAMIN<20){
#     BAMIN <- 20
#     BA5 <- BAMIN
#   }
#   if(PHS_H==2 && BAMIN >= 20 && BAMIN >= BA5 && STCC > 10)BAMIN <- BA5
#   if(PHS_H==2)PHS_H <- PHS_H + 1
#
#   #Tree Size Class = Very Large
#   if(PHS_H==3 && BA5 >= BAMIN && BA5 >= BA125 && BA9 > 0 && BA9 >= BA529 &&
#      BARAT > 0.50){
#     TSC <- 6
#     PHS_H <- PHS_H + 1
#   }
#   #Tree Size Class = Large
#   if(PHS_H==3 && BA5 >= BAMIN && BA5 >= BA125 && BA9 > 0 && BA9 >= BA529 &&
#      BARAT <= 0.50){
#     TSC <- 5
#     PHS_H <- PHS_H + 1
#   }
#   #Tree Size Class = Medium
#   if(PHS_H==3 && BA5 >= BAMIN && BA5 >= BA125 && BA9 < BA529){
#     TSC <- 4
#     PHS_H <- PHS_H + 1
#   }
#   #Tree Size Class = Small
#   if(PHS_H==3 && BA5 >= BAMIN && BA5 < BA125){
#     TSC <- 3
#     PHS_H <- PHS_H + 1
#   }
#   #Tree Size Class = Established Seedlings
#   if(PHS_H==3 && BA5 < BAMIN && TPA >= TPAMIN){
#     TSC <- 2
#     PHS_H <- PHS_H + 1
#   }
#   #Tree Size Class = Grass/Forbs
#   if(PHS_H==3 && STCC < 10){
#     TSC <- 1
#     PHS_H <- PHS_H + 1
#   }
#
#   #Habitat Structural Stage - Mature (Code 4)
#   if(PHS_H==4 && (TSC==5 || TSC==6)){
#     HSS1 <- 4
#     # dfHSS2 <- data.frame(x=c(0,40,70,100),
#     #                       y=c(1,2,3,3))
#     # modelHSS2 <- lm(y~x,data=dfHSS2)
#     # HSS2 <- floor(approx(dfHSS2$x,dfHSS2$y,xout = STCC, rule=2)[[2]])
#     if(STCC < 40) HSS2 <- 1
#     if(STCC >= 40 && STCC < 70) HSS2 <- 2
#     if(STCC >= 70 && STCC < 100 || STCC > 100) HSS2 <- 3
#     CHESS <- HSS1*10+HSS2
#     PHS_H <- PHS_H + 1
#   }
#   #Habitat Structural Stage - Poles/Saplings (Code 3)
#   if(PHS_H==4 && (TSC==3 || TSC==4)){
#     HSS1 <- 3
#     # dfHSS2 <- data.frame(x=c(0,40,70,100),
#     #                       y=c(1,2,3,3))
#     # modelHSS2 <- lm(y~x,data=dfHSS2)
#     # HSS2 <- floor(approx(dfHSS2$x,dfHSS2$y,xout = STCC, rule=2)[[2]])
#     if(STCC < 40) HSS2 <- 1
#     if(STCC >= 40 && STCC < 70) HSS2 <- 2
#     if(STCC >= 70 && STCC < 100 || STCC > 100) HSS2 <- 3
#     CHESS <- HSS1*10+HSS2
#   }
#   #Habitat Structural Stage - Seedlings/Shrubs (Code 2)
#   if(PHS_H==4 && TSC==2){
#     HSS1 <- 2
#     HSS2 <- 0
#     CHESS <- HSS1*10+HSS2
#   }
#   #Habitat Structural Stage - Non-Stocked (Code 1)
#   if(PHS_H==4 && HSS1!=2 && STCC <= 10){
#     HSS1 <- 1
#     HSS2 <- 0
#     CHESS <- HSS1*10+HSS2
#   }
#
#   PPpres <- any(data$SpeciesPLANTS=="PIPO")
#   WSpres <- any(data$SpeciesPLANTS=="PIGL")
#   QApres <- any(data$SpeciesPLANTS=="POTR5")
#
#   #Habitat Structural Stage - Mature very large trees (Code 5)
#   if(PHS_H==5 && HSS1==4){
#     PPBA <- if(PPpres) plotAttr(data,region = 2,min = 0.1,vol1=vol1,vol2=vol2,
#                                 vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
#     WSBA <- if(WSpres) plotAttr(data,region = 2,min = 0.1,vol1=vol1,vol2=vol2,
#                                 vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
#     QABA <- if(QApres) plotAttr(data,region = 2,min = 0.1,vol1=vol1,vol2=vol2,
#                                 vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
#     SPBA <- PPBA + WSBA + QABA
#     PPCT <- if(PPBA > 0.001) 1 else 0
#     WSCT <- if(WSBA > 0.001) 1 else 0
#     QACT <- if(QABA > 0.001) 1 else 0
#     SPCT <- PPCT + WSCT + QACT
#     WSPC <- if(WSBA/SPBA > 0.500 && SPBA > 0) 1 else 0
#     A <- SPCT + WSPC
#     B <- A
#     C <- B
#     D1 <- plotAttr(data,region = 2,min = 0.1,vol1=vol1,vol2=vol2,vol3=vol3)[["PIPO"]]["CC"][[1]][1]
#     if(is.null(D1)) D1 <- 0
#     D1 <- D1 + WSBA + QABA
#     # dfD <- data.frame(x=c(0,10,30,50,70,100),
#     #                       y=c(1,2,3,4,5,5))
#     # modelD <- lm(y~x,data=dfD)
#     # D <- floor(approx(dfD$x,dfD$y,xout = D1, rule=2)[[2]])
#     if(D1 < 10) D <- 1
#     if(D1 >= 10 && D1 < 30) D <- 2
#     if(D1 >= 30 && D1 < 50) D <- 3
#     if(D1 >= 50 && D1 < 70) D <- 4
#     if(D1 >= 70 && D1 < 100 || D1 > 100) D <- 5
#     #OBA16
#     OBA16_PP <- if(PPpres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
#                                  vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
#     OBA16_WS <- if(WSpres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
#                                  vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
#     OBA16_QA <- if(QApres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
#                                  vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
#     OBA16 <- OBA16_PP + OBA16_WS + OBA16_QA
#     #OBA13
#     OBA13_PP <- if(PPpres) plotAttr(data,region = 2,min = 13,max=16,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
#     OBA13_WS <- if(WSpres) plotAttr(data,region = 2,min = 13,max=16,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
#     OBA13_QA <- if(QApres) plotAttr(data,region = 2,min = 13,max=16,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
#     OBA13 <- OBA13_PP + OBA13_WS + OBA13_QA
#     #OBA10
#     OBA10_PP <- if(PPpres) plotAttr(data,region = 2,min = 10,max=13,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
#     OBA10_WS <- if(WSpres) plotAttr(data,region = 2,min = 10,max=13,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
#     OBA10_QA <- if(QApres) plotAttr(data,region = 2,min = 10,max=13,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
#     OBA10 <- OBA10_PP + OBA10_WS + OBA10_QA
#     #OBA07
#     OBA07_PP <- if(PPpres) plotAttr(data,region = 2,min = 7,max=10,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
#     OBA07_WS <- if(WSpres) plotAttr(data,region = 2,min = 7,max=10,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
#     OBA07_QA <- if(QApres) plotAttr(data,region = 2,min = 7,max=10,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
#     OBA07 <- OBA07_PP + OBA07_WS + OBA07_QA
#     #OBA00
#     OBA00_PP <- if(PPpres) plotAttr(data,region = 2,min = 0,max=7,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
#     OBA00_WS <- if(WSpres) plotAttr(data,region = 2,min = 0,max=7,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
#     OBA00_QA <- if(QApres) plotAttr(data,region = 2,min = 0,max=7,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
#     OBA00 <- OBA00_PP + OBA00_WS + OBA00_QA
#
#     OBAS <- c(OBA00,OBA07,OBA10,OBA13,OBA16)
#     G <- if(length(unique(OBAS))==1) 0 else which(OBAS==max(OBAS))
#     #MBA09
#     MBA09_PP <- if(PPpres) plotAttr(data,region = 2,min = 9,max=16,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
#     MBA09_WS <- if(WSpres) plotAttr(data,region = 2,min = 9,max=16,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
#     MBA09_QA <- if(QApres) plotAttr(data,region = 2,min = 9,max=16,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
#     MBA09 <- MBA09_PP + MBA09_WS + MBA09_QA
#     #MBA06
#     MBA06_PP <- if(PPpres) plotAttr(data,region = 2,min = 6,max=9,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
#     MBA06_WS <- if(WSpres) plotAttr(data,region = 2,min = 6,max=9,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
#     MBA06_QA <- if(QApres) plotAttr(data,region = 2,min = 6,max=9,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
#     MBA06 <- MBA06_PP + MBA06_WS + MBA06_QA
#     #MBA03
#     MBA03_PP <- if(PPpres) plotAttr(data,region = 2,min = 3,max=6,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
#     MBA03_WS <- if(WSpres) plotAttr(data,region = 2,min = 3,max=6,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
#     MBA03_QA <- if(QApres) plotAttr(data,region = 2,min = 3,max=6,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
#     MBA03 <- MBA03_PP + MBA03_WS + MBA03_QA
#     #MBA00
#     MBA00_PP <- if(PPpres) plotAttr(data,region = 2,min = 0,max=3,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
#     MBA00_WS <- if(WSpres) plotAttr(data,region = 2,min = 0,max=3,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
#     MBA00_QA <- if(QApres) plotAttr(data,region = 2,min = 0,max=3,vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
#     MBA00 <- MBA00_PP + MBA00_WS + MBA00_QA
#     H <- 0
#     #SQMD
#     SQMD_PP <-  if(PPpres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIPO"]]["QMD"][[1]][1] else 0
#     SQMD_WS <-  if(WSpres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["PIGL"]]["QMD"][[1]][1] else 0
#     SQMD_QA <-  if(QApres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
#                       vol3=vol3)[["POTR5"]]["QMD"][[1]][1] else 0
#     SQMD <- SQMD_PP + SQMD_WS + SQMD_QA
#     # dfI <- data.frame(x=c(0,7,10,13,16,999),
#     #                       y=c(0,2,3,4,5,5))
#     # modelI <- lm(y~x,data=dfI)
#     # I <- floor(approx(dfI$x,dfI$y,xout = SQMD, rule=2)[[2]])
#     if(SQMD < 7) I <- 0
#     if(SQMD >= 7 && SQMD < 10) I <- 2
#     if(SQMD >= 10 && SQMD < 13) I <- 3
#     if(SQMD >= 13 && SQMD < 16) I <- 4
#     if(SQMD >= 16 && SQMD < 999) I <- 5
#     #STPA
#     STPA_PP <- if(PPpres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
#                      vol3=vol3)[["PIPO"]]["TPA"][[1]][1] else 0
#     STPA_WS <- if(WSpres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
#                      vol3=vol3)[["PIGL"]]["TPA"][[1]][1] else 0
#     STPA_QA <- if(QApres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
#                      vol3=vol3)[["POTR5"]]["TPA"][[1]][1] else 0
#     STPA <- STPA_PP + STPA_WS + STPA_QA
#     # dfJ <- data.frame(x=c(0,0.1,4,6,999),
#     #                       y=c(0,3,4,5,5))
#     # modelJ <- lm(y~x,data=dfJ)
#     # J <- floor(approx(dfJ$x,dfJ$y,xout = STPA, rule=2)[[2]])
#     if(STPA < 0.1) J <- 0
#     if(STPA >= 0.1 && STPA < 4) J <- 3
#     if(STPA >= 4 && STPA < 6) J <- 4
#     if(STPA >= 6 && STPA < 999) J <- 5
#     K <- I
#     L <- J
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==6 && HSS1==4 && SPCT==3){
#     A <- A + WSPC + 1
#     B <- A
#     C <- B
#     PHS_H <- PHS_H + 1
#   }
#
#   if(PHS_H==6 && HSS1==4 && G==5 && (MBA00+MBA03+MBA06+MBA09) > 0){
#     MBAS <- c(MBA00,MBA03,MBA06,MBA09)
#     H <- if(length(unique(MBAS))==1) 0+1 else which(MBAS==max(MBAS))+1
#     #E1
#     E1_PP <- if(PPpres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
#     E1_WS <- if(WSpres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
#     E1_QA <- if(QApres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
#     E1 <- E1_PP + E1_WS + E1_QA
#     # dfE <- data.frame(x=c(0,1,10,30,100),
#     #                       y=c(0,3,4,5,5))
#     # modelE <- lm(y~x,data=dfE)
#     # E <- floor(approx(dfE$x,dfE$y,xout = E1, rule=2)[[2]])
#     if(E1 < 1) E <- 0
#     if(E1 >= 1 && E1 < 10) E <- 3
#     if(E1 >= 10 && E1 < 30) E <- 4
#     if((E1 >= 30 && E1 <= 100) || E1 > 100) E <- 5
#     PHS_H <- PHS_H + 1
# }
#   if(PHS_H==6 && HSS1==4 && G==4 && (MBA00+MBA03+MBA06) > 0){
#     MBAS <- c(MBA00,MBA03,MBA06)
#     H <- if(length(unique(MBAS))==1) 0+1 else which(MBAS==max(MBAS))+1
#     #E1
#     E1_PP <- if(PPpres) plotAttr(data,region = 2,min = 13,max = 16,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
#     E1_WS <- if(WSpres) plotAttr(data,region = 2,min = 13,max = 16,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
#     E1_QA <- if(QApres) plotAttr(data,region = 2,min = 13,max = 16,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
#     E1 <- E1_PP + E1_WS + E1_QA
#     # dfE <- data.frame(x=c(0,1,10,30,100),
#     #                       y=c(0,3,4,5,5))
#     # modelE <- lm(y~x,data=dfE)
#     # E <- floor(approx(dfE$x,dfE$y,xout = E1, rule=2)[[2]])
#     if(E1 < 1) E <- 0
#     if(E1 >= 1 && E1 < 10) E <- 3
#     if(E1 >= 10 && E1 < 30) E <- 4
#     if((E1 >= 30 && E1 <= 100) || E1 > 100) E <- 5
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==6 && HSS1==4 && G==3 && (MBA00+MBA03) > 0){
#     MBAS <- c(MBA00,MBA03)
#     H <- if(length(unique(MBAS))==1) 0+1 else which(MBAS==max(MBAS))+1
#     #E1
#     E1_PP <- if(PPpres) plotAttr(data,region = 2,min = 10,max = 13,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
#     E1_WS <- if(WSpres) plotAttr(data,region = 2,min = 10,max = 13,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
#     E1_QA <- if(QApres) plotAttr(data,region = 2,min = 10,max = 13,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
#     E1 <- E1_PP + E1_WS + E1_QA
#     # dfE <- data.frame(x=c(0,1,10,30,100),
#     #                       y=c(0,3,4,5,5))
#     # modelE <- lm(y~x,data=dfE)
#     # E <- floor(approx(dfE$x,dfE$y,xout = E1, rule=2)[[2]])
#     if(E1 < 1) E <- 0
#     if(E1 >= 1 && E1 < 10) E <- 3
#     if(E1 >= 10 && E1 < 30) E <- 4
#     if((E1 >= 30 && E1 <= 100) || E1 > 100) E <- 5
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==6 && HSS1==4 && G==2 && MBA00 > 0){
#     # MBAS <- c(MBA00)
#     # H <- if(length(unique(MBAS))==1) 0+1 else which(MBAS==max(MBAS))+1
#     H <- 2
#     #E1
#     E1_PP <- if(PPpres) plotAttr(data,region = 2,min = 7,max = 10,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
#     E1_WS <- if(WSpres) plotAttr(data,region = 2,min = 7,max = 10,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
#     E1_QA <- if(QApres) plotAttr(data,region = 2,min = 7,max = 10,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
#     E1 <- E1_PP + E1_WS + E1_QA
#     # dfE <- data.frame(x=c(0,1,10,30,100),
#     #                       y=c(0,3,4,5,5))
#     # modelE <- lm(y~x,data=dfE)
#     # E <- floor(approx(dfE$x,dfE$y,xout = E1, rule=2)[[2]])
#     if(E1 < 1) E <- 0
#     if(E1 >= 1 && E1 < 10) E <- 3
#     if(E1 >= 10 && E1 < 30) E <- 4
#     if((E1 >= 30 && E1 <= 100) || E1 > 100) E <- 5
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==6 && HSS1==4 && G==1){
#     H <- 0
#     #E1
#     E1_PP <- if(PPpres) plotAttr(data,region = 2,min = 0,max = 7,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
#     E1_WS <- if(WSpres) plotAttr(data,region = 2,min = 0,max = 7,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
#     E1_QA <- if(QApres) plotAttr(data,region = 2,min = 0,max = 7,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
#     E1 <- E1_PP + E1_WS + E1_QA
#     # dfE <- data.frame(x=c(0,1,10,30,100),
#     #                       y=c(0,3,4,5,5))
#     # modelE <- lm(y~x,data=dfE)
#     # E <- floor(approx(dfE$x,dfE$y,xout = E1, rule=2)[[2]])
#     if(E1 < 1) E <- 0
#     if(E1 >= 1 && E1 < 10) E <- 3
#     if(E1 >= 10 && E1 < 30) E <- 4
#     if((E1 >= 30 && E1 <= 100) || E1 > 100) E <- 5
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==7 && HSS1==4 && H==5){
#     #F1
#     F1_PP <- if(PPpres) plotAttr(data,region = 2,min = 9,max = 16,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
#     F1_WS <- if(WSpres) plotAttr(data,region = 2,min = 9,max = 16,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
#     F1_QA <- if(QApres) plotAttr(data,region = 2,min = 9,max = 16,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
#     F1 <- F1_PP + F1_WS + F1_QA
#     # dfF <- data.frame(x=c(0,1,10,20,100),
#     #                       y=c(0,3,4,5,5))
#     # modelF <- lm(y~x,data=dfF)
#     # F2 <- floor(approx(dfF$x,dfF$y,xout = F1, rule=2)[[2]]) #R reserves "F" as a pre-defined variable, hence the "F2"
#     if(F1 < 1) F2 <- 0
#     if(F1 >= 1 && F1 < 10) F2 <- 3
#     if(F1 >= 10 && F1 < 20) F2 <- 4
#     if((F1 >= 20 && F1 <= 100) || F1 > 100) F2 <- 5
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==7 && HSS1==4 && H==4){
#     #F1
#     F1_PP <- if(PPpres) plotAttr(data,region = 2,min = 6,max = 9,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
#     F1_WS <- if(WSpres) plotAttr(data,region = 2,min = 6,max = 9,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
#     F1_QA <- if(QApres) plotAttr(data,region = 2,min = 6,max = 9,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
#     F1 <- F1_PP + F1_WS + F1_QA
#     # dfF <- data.frame(x=c(0,1,10,20,100),
#     #                       y=c(0,3,4,5,5))
#     # modelF <- lm(y~x,data=dfF)
#     # F2 <- floor(approx(dfF$x,dfF$y,xout = F1, rule=2)[[2]]) #R reserves "F" as a pre-defined variable, hence the "F2"
#     if(F1 < 1) F2 <- 0
#     if(F1 >= 1 && F1 < 10) F2 <- 3
#     if(F1 >= 10 && F1 < 20) F2 <- 4
#     if((F1 >= 20 && F1 <= 100) || F1 > 100) F2 <- 5
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==7 && HSS1==4 && H==3){
#     #F1
#     F1_PP <- if(PPpres) plotAttr(data,region = 2,min = 3,max = 6,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
#     F1_WS <- if(WSpres) plotAttr(data,region = 2,min = 3,max = 6,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
#     F1_QA <- if(QApres) plotAttr(data,region = 2,min = 3,max = 6,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
#     F1 <- F1_PP + F1_WS + F1_QA
#     # dfF <- data.frame(x=c(0,1,10,20,100),
#     #                       y=c(0,3,4,5,5))
#     # modelF <- lm(y~x,data=dfF)
#     # F2 <- floor(approx(dfF$x,dfF$y,xout = F1, rule=2)[[2]]) #R reserves "F" as a pre-defined variable, hence the "F2"
#     if(F1 < 1) F2 <- 0
#     if(F1 >= 1 && F1 < 10) F2 <- 3
#     if(F1 >= 10 && F1 < 20) F2 <- 4
#     if((F1 >= 20 && F1 <= 100) || F1 > 100) F2 <- 5
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==7 && HSS1==4 && H==2){
#     #F1
#     F1_PP <- if(PPpres) plotAttr(data,region = 2,min = 0,max = 3,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
#     F1_WS <- if(WSpres) plotAttr(data,region = 2,min = 0,max = 3,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
#     F1_QA <- if(QApres) plotAttr(data,region = 2,min = 0,max = 3,,vol1=vol1,vol2=vol2,
#                    vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
#     F1 <- F1_PP + F1_WS + F1_QA
#     # dfF <- data.frame(x=c(0,1,10,20,100),
#     #                       y=c(0,3,4,5,5))
#     # modelF <- lm(y~x,data=dfF)
#     # F2 <- floor(approx(dfF$x,dfF$y,xout = F1, rule=2)[[2]]) #R reserves "F" as a pre-defined variable, hence the "F2"
#     if(F1 < 1) F2 <- 0
#     if(F1 >= 1 && F1 < 10) F2 <- 3
#     if(F1 >= 10 && F1 < 20) F2 <- 4
#     if((F1 >= 20 && F1 <= 100) || F1 > 100) F2 <- 5
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==7 && HSS1==4 && (H==1 || H==0)){
#     # F1 <- plotAttr(data,region = 2,min = 0,max = 0,vol1=vol1,vol2=vol2,vol3=vol3)[["PIPO"]]["CC"][[1]][1] +
#     #       plotAttr(data,region = 2,min = 0,max = 0,vol1=vol1,vol2=vol2,vol3=vol3)[["PIGL"]]["CC"][[1]][1] +
#     #       plotAttr(data,region = 2,min = 0,max = 0,vol1=vol1,vol2=vol2,vol3=vol3)[["POTR5"]]["CC"][[1]][1]
#     # dfF <- data.frame(x=c(0,1,10,20,100),
#     #                       y=c(0,3,4,5,5))
#     # modelF <- lm(y~x,data=dfF)
#     # F2 <- floor(approx(dfF$x,dfF$y,xout = F1)[[2]])
#     F2 <- 0 #Same result as above (and KCP this is translated from)
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==8 && HSS1==4){
#     OGSC <- A + B + C + D + E + F2 + G + H + I + J + K + L
#     PHS_H <- PHS_H + 1
#   }
#   if(PHS_H==9 && HSS1==4 && OGSC >= 42){
#     CHESS <- 50
#     PHS_H <- PHS_H + 1
#   }
#
#   # #Translate SIZE_CLASS
#   # TSCtrans <- c(
#   #     "0"="NONE","1"="N","2"="E","3"="S",
#   #   "4"="M","5"="L","6"="V"
#   # )
#   # TSC <- as.character(TSCtrans[as.character(TSC)])
#
#   #Translate CHESS
#   CHESStrans <- c(
#     "10"="1","20"="2","31"="3A","32"="3B","33"="3C",
#     "41"="4A","42"="4B","43"="4C","50"="5"
#   )
#   HAB_STR_STAGE <- as.character(CHESStrans[as.character(CHESS)])

# #New algorithm from R2Calcs.py to calculate HAB_STR_STAGE
#   if(grpSizeClass=="N") HAB_STR_STAGE <- "1T"
#   if(grpSizeClass=="E") HAB_STR_STAGE <- "2T"
#   if(grpSizeClass=="S" || grpSizeClass=="M"){
#     if(STCC < 40 && STCC > 0) HAB_STR_STAGE <- "3A"
#     if(STCC >= 40 && STCC < 70) HAB_STR_STAGE <- "3B"
#     if(STCC >= 70) HAB_STR_STAGE <- "3C"
#   }
#   if(grpSizeClass=="L" || grpSizeClass=="V"){
#     if(STCC < 40 && STCC > 0) HAB_STR_STAGE <- "4A"
#     if(STCC >= 40 && STCC < 70) HAB_STR_STAGE <- "4B"
#     if(STCC >= 70) HAB_STR_STAGE <- "4C"
#   }

  HSS1_4C <- HSS(HSStype = 1,
                   data = data,
                   TPA = TPA,
                   CC = CC,
                   plotvals = plotvals)

  HSS1_5 <- HSS(HSStype = 2,
                  data = data,
                  TPA = TPA,
                  CC = CC,
                  plotvals = plotvals)

  results=list("DOM_TYPE_R2" = DOM_TYPE_R2,
               "DOM_TYPE_R2_CC1" = DOM_TYPE_R2_CC1,
               "DOM_TYPE_R2_CC2" = DOM_TYPE_R2_CC2,
               "DOM_TYPE_R2_CC3" = DOM_TYPE_R2_CC3,
               "COVERTYPE_R2" = COVERTYPE_R2,
               "TREE_SIZE_CLASS" = TREE_SIZE_CLASS,
               "CROWN_CLASS" = CROWN_CLASS,
               "HSS1_4C" = HSS1_4C,
               "HSS1_5" = HSS1_5)

  return(results)
}
