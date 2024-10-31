################################################################################
#Function: HSS
#
#Calculates two different habitat structural stage variables. One based on the
#logic found in the R2Calcs and the other based on the hss_wi5.kcp, found
#at https://www.fs.usda.gov/fvs/software/addfiles/hss_wi5.zip
#
#Arguments
#
#HSStype: habitat structural stage type (1= 1-4C type, and 2= 1-5 type)
#
#data:    Data frame containing tree records from a single stand or plot. Data
#         frame must contain a column corresponding to stand/plot ID, DBH,
#         species code (USDA plant symbol), expansion factor, and height
#         for each tree record.
#
#TPA:     TPA of stand/plot.
#
#CC:      Percent canopy cover of stand/plot.
#
#plotvals: Named list containing plot attributes for each species in plot/stand
#         and/or across all species in plot/stand.
#
#Return value
#
#Named list containing
# - Dominance type reported as the top 3 species (DOM_TYPE_R2)
# - Percent canopy cover of the most dominant species in the stand (DOM_TYPE_R2_CC1)
# - Percent canopy cover of the 2nd most dominant species in the stand (DOM_TYPE_R2_CC2)
# - Percent canopy cover of the 3rd most dominant species in the stand (DOM_TYPE_R2_CC3)
# - R2 cover type (COVERTYPE_R2)
# - R2 tree size class (TREE_SIZE_CLASS_R2)
# - R2 percent canopy cover class (CROWN_CLASS_R2)
# - Habitat structural stage calculated and reported for the 1 through 4C classes (HSS1_4C)
# - Habitat structural stage calculated and reported for the 1 through 5 classes (HSS1_5)
################################################################################
#'@export
HSS<-function(HSStype,
                  data,
                  TPA,
                  CC,
                  plotvals){

  STCC <- CC
  HSS1 <- 0
  if(HSStype==1){#Calculate TREE_SIZE_CLASS from R2Calcs.py V1.4 (HSS1_4C)
    grpSizeClass<-NA
    if(STCC < 10){
      grpSizeClass <- "N"
    }else {
      #Initialize canopy cover by diameter class vector
      CCByDiameterClass <- vector("numeric",5)
      #Populate CCByDiameterClass vector
      for (i in 1:nrow(data)){
        if(data$DBH[i] >= 0.0 && data$DBH[i] < 1)
          CCByDiameterClass[1] <- CCByDiameterClass[1] + data$TREECC[i]
        if(data$DBH[i] >= 1.0 && data$DBH[i] < 5)
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
      GroupES <- covTsize_E + covTsize_S
      GroupLV <- covTsize_L + covTsize_V

      if(GroupLV > 0) grpSizeClass <- "LV" #may be reset below
      if(covTsize_M > GroupLV) grpSizeClass <- "M"
      if(GroupES > covTsize_M && GroupES > GroupLV) grpSizeClass <- "ES"
      if(!is.na(grpSizeClass) && grpSizeClass=="LV"){
        grpSizeClass <- if(covTsize_V >= covTsize_L) "V" else "L"
      }
      if(!is.na(grpSizeClass) && grpSizeClass=="ES"){
        grpSizeClass <- if(covTsize_S >= covTsize_E) "S" else "E"
      }
    }
    if(is.na(grpSizeClass) &&  STCC < 10) grpSizeClass <- "N"

    #Algorithm from R2Calcs.py to calculate HAB_STR_STAGE
    if(grpSizeClass=="N") HAB_STR_STAGE <- "1"
    if(grpSizeClass=="E") HAB_STR_STAGE <- "2"
    if(grpSizeClass=="S" || grpSizeClass=="M"){
      if(STCC < 40 && STCC > 0) HAB_STR_STAGE <- "3A"
      if(STCC >= 40 && STCC < 70) HAB_STR_STAGE <- "3B"
      if(STCC >= 70) HAB_STR_STAGE <- "3C"
    }
    if(grpSizeClass=="L" || grpSizeClass=="V"){
      if(STCC < 40 && STCC > 0) HAB_STR_STAGE <- "4A"
      if(STCC >= 40 && STCC < 70) HAB_STR_STAGE <- "4B"
      if(STCC >= 70) HAB_STR_STAGE <- "4C"
    }
  }#END HSS1_4C
  else {#BEGIN HSS1_5
#Calculate HAB_STR_STAGE (translation of hss_wi5.kcp, found
#at https://www.fs.usda.gov/fvs/software/addfiles/hss_wi5.zip)
  vol1 <- "TCuFt"
  vol2 <- "MCuFt"
  vol3 <- "BdFt"
  #Stand-level TPA
  TPA0  <- round(plotvals[["ALL"]]["TPA"][[1]][1],2)
  #Stand-level BA
  BA0 <- round(plotvals[["ALL"]]["BA"][[1]][1],2)
  #BA in trees > 1" DBH
  BA1 <- plotAttr(data,region = 2,min = 1,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
  #BA in trees >= 5" DBH
  BA5 <- plotAttr(data,region = 2,min = 5,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
  #BA in trees >= 9" DBH
  BA9 <- plotAttr(data,region = 2,min = 9,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
  #BA in trees >= 16" DBH
  BA16 <- plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
  #BA in trees >= 1" DBH and < 5" DBH
  BA125 <- plotAttr(data,region = 2,min = 1,max=5,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
  #BA in trees >= 5" DBH and < 9" DBH
  BA529 <- plotAttr(data,region = 2,min = 5,max=9,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["BA"][[1]][1]
  #QMD in trees >= 1" DBH
  QMD <- plotAttr(data,region = 2,min = 1,vol1=vol1,vol2=vol2,vol3=vol3)[["ALL"]]["QMD"][[1]][1]
  BARAT <- 0
  TPAMAX <- 0
  TPAMIN <- 300
  BAMAX <- 0
  BAMIN <- 0
  CHESS <- 10
  OGSC <- 0
  TSC <- 0
  PHS_H <- 0
  A <- B <- C <- D <- E <- F2 <- G <- H <- I <- J <- K <- L <- 0

  if(PHS_H==0)PHS_H <- PHS_H + 1
  if(PHS_H==1 && BA9>0)BARAT <- BA16/BA9
  if(PHS_H==1 && QMD>0){
    TPAMAX <- 18641.0/(QMD**1.659925)
    BAMAX <- 101.67*(QMD**0.34007)
    BAMIN <- BAMAX*0.10
  }
  if(PHS_H==1)PHS_H <- PHS_H + 1
  if(PHS_H==2 && BAMIN<20){
    BAMIN <- 20
    BA5 <- BAMIN
  }
  if(PHS_H==2 && BAMIN >= 20 && BAMIN >= BA5 && STCC > 10)BAMIN <- BA5
  if(PHS_H==2)PHS_H <- PHS_H + 1

  #Tree Size Class = Very Large
  if(PHS_H==3 && BA5 >= BAMIN && BA5 >= BA125 && BA9 > 0 && BA9 >= BA529 &&
     BARAT > 0.50){
    TSC <- 6
    PHS_H <- PHS_H + 1
  }
  #Tree Size Class = Large
  if(PHS_H==3 && BA5 >= BAMIN && BA5 >= BA125 && BA9 > 0 && BA9 >= BA529 &&
     BARAT <= 0.50){
    TSC <- 5
    PHS_H <- PHS_H + 1
  }
  #Tree Size Class = Medium
  if(PHS_H==3 && BA5 >= BAMIN && BA5 >= BA125 && BA9 < BA529){
    TSC <- 4
    PHS_H <- PHS_H + 1
  }
  #Tree Size Class = Small
  if(PHS_H==3 && BA5 >= BAMIN && BA5 < BA125){
    TSC <- 3
    PHS_H <- PHS_H + 1
  }
  #Tree Size Class = Established Seedlings
  if(PHS_H==3 && BA5 < BAMIN && TPA >= TPAMIN){
    TSC <- 2
    PHS_H <- PHS_H + 1
  }
  #Tree Size Class = Grass/Forbs
  if(PHS_H==3 && STCC < 10){
    TSC <- 1
    PHS_H <- PHS_H + 1
  }

  #Habitat Structural Stage - Mature (Code 4)
  if(PHS_H==4 && (TSC==5 || TSC==6)){
    HSS1 <- 4
    # dfHSS2 <- data.frame(x=c(0,40,70,100),
    #                       y=c(1,2,3,3))
    # modelHSS2 <- lm(y~x,data=dfHSS2)
    # HSS2 <- floor(approx(dfHSS2$x,dfHSS2$y,xout = STCC, rule=2)[[2]])
    if(STCC < 40) HSS2 <- 1
    if(STCC >= 40 && STCC < 70) HSS2 <- 2
    if(STCC >= 70 && STCC < 100 || STCC > 100) HSS2 <- 3
    CHESS <- HSS1*10+HSS2
    PHS_H <- PHS_H + 1
  }
  #Habitat Structural Stage - Poles/Saplings (Code 3)
  if(PHS_H==4 && (TSC==3 || TSC==4)){
    HSS1 <- 3
    # dfHSS2 <- data.frame(x=c(0,40,70,100),
    #                       y=c(1,2,3,3))
    # modelHSS2 <- lm(y~x,data=dfHSS2)
    # HSS2 <- floor(approx(dfHSS2$x,dfHSS2$y,xout = STCC, rule=2)[[2]])
    if(STCC < 40) HSS2 <- 1
    if(STCC >= 40 && STCC < 70) HSS2 <- 2
    if(STCC >= 70 && STCC < 100 || STCC > 100) HSS2 <- 3
    CHESS <- HSS1*10+HSS2
  }
  #Habitat Structural Stage - Seedlings/Shrubs (Code 2)
  if(PHS_H==4 && TSC==2){
    HSS1 <- 2
    HSS2 <- 0
    CHESS <- HSS1*10+HSS2
  }
  #Habitat Structural Stage - Non-Stocked (Code 1)
  if(PHS_H==4 && HSS1!=2 && STCC <= 10){
    HSS1 <- 1
    HSS2 <- 0
    CHESS <- HSS1*10+HSS2
  }

  PPpres <- any(data$SpeciesPLANTS=="PIPO")
  WSpres <- any(data$SpeciesPLANTS=="PIGL")
  QApres <- any(data$SpeciesPLANTS=="POTR5")

  #Habitat Structural Stage - Mature very large trees (Code 5)
  if(PHS_H==5 && HSS1==4){
    PPBA <- if(PPpres) plotAttr(data,region = 2,min = 0.1,vol1=vol1,vol2=vol2,
                                vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
    WSBA <- if(WSpres) plotAttr(data,region = 2,min = 0.1,vol1=vol1,vol2=vol2,
                                vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
    QABA <- if(QApres) plotAttr(data,region = 2,min = 0.1,vol1=vol1,vol2=vol2,
                                vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
    SPBA <- PPBA + WSBA + QABA
    PPCT <- if(PPBA > 0.001) 1 else 0
    WSCT <- if(WSBA > 0.001) 1 else 0
    QACT <- if(QABA > 0.001) 1 else 0
    SPCT <- PPCT + WSCT + QACT
    WSPC <- if(WSBA/SPBA > 0.500 && SPBA > 0) 1 else 0
    A <- SPCT + WSPC
    B <- A
    C <- B
    D1 <- plotAttr(data,region = 2,min = 0.1,vol1=vol1,vol2=vol2,vol3=vol3)[["PIPO"]]["CC"][[1]][1]
    if(is.null(D1)) D1 <- 0
    D1 <- D1 + WSBA + QABA
    # dfD <- data.frame(x=c(0,10,30,50,70,100),
    #                       y=c(1,2,3,4,5,5))
    # modelD <- lm(y~x,data=dfD)
    # D <- floor(approx(dfD$x,dfD$y,xout = D1, rule=2)[[2]])
    if(D1 < 10) D <- 1
    if(D1 >= 10 && D1 < 30) D <- 2
    if(D1 >= 30 && D1 < 50) D <- 3
    if(D1 >= 50 && D1 < 70) D <- 4
    if(D1 >= 70 && D1 < 100 || D1 > 100) D <- 5
    #OBA16
    OBA16_PP <- if(PPpres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
                                 vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
    OBA16_WS <- if(WSpres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
                                 vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
    OBA16_QA <- if(QApres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
                                 vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
    OBA16 <- OBA16_PP + OBA16_WS + OBA16_QA
    #OBA13
    OBA13_PP <- if(PPpres) plotAttr(data,region = 2,min = 13,max=16,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
    OBA13_WS <- if(WSpres) plotAttr(data,region = 2,min = 13,max=16,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
    OBA13_QA <- if(QApres) plotAttr(data,region = 2,min = 13,max=16,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
    OBA13 <- OBA13_PP + OBA13_WS + OBA13_QA
    #OBA10
    OBA10_PP <- if(PPpres) plotAttr(data,region = 2,min = 10,max=13,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
    OBA10_WS <- if(WSpres) plotAttr(data,region = 2,min = 10,max=13,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
    OBA10_QA <- if(QApres) plotAttr(data,region = 2,min = 10,max=13,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
    OBA10 <- OBA10_PP + OBA10_WS + OBA10_QA
    #OBA07
    OBA07_PP <- if(PPpres) plotAttr(data,region = 2,min = 7,max=10,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
    OBA07_WS <- if(WSpres) plotAttr(data,region = 2,min = 7,max=10,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
    OBA07_QA <- if(QApres) plotAttr(data,region = 2,min = 7,max=10,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
    OBA07 <- OBA07_PP + OBA07_WS + OBA07_QA
    #OBA00
    OBA00_PP <- if(PPpres) plotAttr(data,region = 2,min = 0,max=7,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
    OBA00_WS <- if(WSpres) plotAttr(data,region = 2,min = 0,max=7,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
    OBA00_QA <- if(QApres) plotAttr(data,region = 2,min = 0,max=7,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
    OBA00 <- OBA00_PP + OBA00_WS + OBA00_QA

    OBAS <- c(OBA00,OBA07,OBA10,OBA13,OBA16)
    G <- if(length(unique(OBAS))==1) 0 else which(OBAS==max(OBAS))
    #MBA09
    MBA09_PP <- if(PPpres) plotAttr(data,region = 2,min = 9,max=16,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
    MBA09_WS <- if(WSpres) plotAttr(data,region = 2,min = 9,max=16,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
    MBA09_QA <- if(QApres) plotAttr(data,region = 2,min = 9,max=16,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
    MBA09 <- MBA09_PP + MBA09_WS + MBA09_QA
    #MBA06
    MBA06_PP <- if(PPpres) plotAttr(data,region = 2,min = 6,max=9,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
    MBA06_WS <- if(WSpres) plotAttr(data,region = 2,min = 6,max=9,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
    MBA06_QA <- if(QApres) plotAttr(data,region = 2,min = 6,max=9,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
    MBA06 <- MBA06_PP + MBA06_WS + MBA06_QA
    #MBA03
    MBA03_PP <- if(PPpres) plotAttr(data,region = 2,min = 3,max=6,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
    MBA03_WS <- if(WSpres) plotAttr(data,region = 2,min = 3,max=6,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
    MBA03_QA <- if(QApres) plotAttr(data,region = 2,min = 3,max=6,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
    MBA03 <- MBA03_PP + MBA03_WS + MBA03_QA
    #MBA00
    MBA00_PP <- if(PPpres) plotAttr(data,region = 2,min = 0,max=3,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIPO"]]["BA"][[1]][1] else 0
    MBA00_WS <- if(WSpres) plotAttr(data,region = 2,min = 0,max=3,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIGL"]]["BA"][[1]][1] else 0
    MBA00_QA <- if(QApres) plotAttr(data,region = 2,min = 0,max=3,vol1=vol1,vol2=vol2,
                      vol3=vol3)[["POTR5"]]["BA"][[1]][1] else 0
    MBA00 <- MBA00_PP + MBA00_WS + MBA00_QA
    H <- 0
    #SQMD
    SQMD_PP <-  if(PPpres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIPO"]]["QMD"][[1]][1] else 0
    SQMD_WS <-  if(WSpres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
                      vol3=vol3)[["PIGL"]]["QMD"][[1]][1] else 0
    SQMD_QA <-  if(QApres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
                      vol3=vol3)[["POTR5"]]["QMD"][[1]][1] else 0
    SQMD <- SQMD_PP + SQMD_WS + SQMD_QA
    # dfI <- data.frame(x=c(0,7,10,13,16,999),
    #                       y=c(0,2,3,4,5,5))
    # modelI <- lm(y~x,data=dfI)
    # I <- floor(approx(dfI$x,dfI$y,xout = SQMD, rule=2)[[2]])
    if(SQMD < 7) I <- 0
    if(SQMD >= 7 && SQMD < 10) I <- 2
    if(SQMD >= 10 && SQMD < 13) I <- 3
    if(SQMD >= 13 && SQMD < 16) I <- 4
    if(SQMD >= 16 && SQMD < 999) I <- 5
    #STPA
    STPA_PP <- if(PPpres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
                     vol3=vol3)[["PIPO"]]["TPA"][[1]][1] else 0
    STPA_WS <- if(WSpres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
                     vol3=vol3)[["PIGL"]]["TPA"][[1]][1] else 0
    STPA_QA <- if(QApres) plotAttr(data,region = 2,min = 7,expf="MortPA",vol1=vol1,vol2=vol2,
                     vol3=vol3)[["POTR5"]]["TPA"][[1]][1] else 0
    STPA <- STPA_PP + STPA_WS + STPA_QA
    # dfJ <- data.frame(x=c(0,0.1,4,6,999),
    #                       y=c(0,3,4,5,5))
    # modelJ <- lm(y~x,data=dfJ)
    # J <- floor(approx(dfJ$x,dfJ$y,xout = STPA, rule=2)[[2]])
    if(STPA < 0.1) J <- 0
    if(STPA >= 0.1 && STPA < 4) J <- 3
    if(STPA >= 4 && STPA < 6) J <- 4
    if(STPA >= 6 && STPA < 999) J <- 5
    K <- I
    L <- J
    PHS_H <- PHS_H + 1
  }

  if(PHS_H==6 && HSS1==4 && SPCT==3){
    A <- A + WSPC + 1
    B <- A
    C <- B
    PHS_H <- PHS_H + 1
  }

  if(PHS_H==6 && HSS1==4 && G==5 && (MBA00+MBA03+MBA06+MBA09) > 0){
    MBAS <- c(MBA00,MBA03,MBA06,MBA09)
    H <- if(length(unique(MBAS))==1) 0+1 else which(MBAS==max(MBAS))+1
    #E1
    E1_PP <- if(PPpres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
    E1_WS <- if(WSpres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
    E1_QA <- if(QApres) plotAttr(data,region = 2,min = 16,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
    E1 <- E1_PP + E1_WS + E1_QA
    # dfE <- data.frame(x=c(0,1,10,30,100),
    #                       y=c(0,3,4,5,5))
    # modelE <- lm(y~x,data=dfE)
    # E <- floor(approx(dfE$x,dfE$y,xout = E1, rule=2)[[2]])
    if(E1 < 1) E <- 0
    if(E1 >= 1 && E1 < 10) E <- 3
    if(E1 >= 10 && E1 < 30) E <- 4
    if((E1 >= 30 && E1 <= 100) || E1 > 100) E <- 5
    PHS_H <- PHS_H + 1
}
  if(PHS_H==6 && HSS1==4 && G==4 && (MBA00+MBA03+MBA06) > 0){
    MBAS <- c(MBA00,MBA03,MBA06)
    H <- if(length(unique(MBAS))==1) 0+1 else which(MBAS==max(MBAS))+1
    #E1
    E1_PP <- if(PPpres) plotAttr(data,region = 2,min = 13,max = 16,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
    E1_WS <- if(WSpres) plotAttr(data,region = 2,min = 13,max = 16,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
    E1_QA <- if(QApres) plotAttr(data,region = 2,min = 13,max = 16,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
    E1 <- E1_PP + E1_WS + E1_QA
    # dfE <- data.frame(x=c(0,1,10,30,100),
    #                       y=c(0,3,4,5,5))
    # modelE <- lm(y~x,data=dfE)
    # E <- floor(approx(dfE$x,dfE$y,xout = E1, rule=2)[[2]])
    if(E1 < 1) E <- 0
    if(E1 >= 1 && E1 < 10) E <- 3
    if(E1 >= 10 && E1 < 30) E <- 4
    if((E1 >= 30 && E1 <= 100) || E1 > 100) E <- 5
    PHS_H <- PHS_H + 1
  }
  if(PHS_H==6 && HSS1==4 && G==3 && (MBA00+MBA03) > 0){
    MBAS <- c(MBA00,MBA03)
    H <- if(length(unique(MBAS))==1) 0+1 else which(MBAS==max(MBAS))+1
    #E1
    E1_PP <- if(PPpres) plotAttr(data,region = 2,min = 10,max = 13,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
    E1_WS <- if(WSpres) plotAttr(data,region = 2,min = 10,max = 13,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
    E1_QA <- if(QApres) plotAttr(data,region = 2,min = 10,max = 13,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
    E1 <- E1_PP + E1_WS + E1_QA
    # dfE <- data.frame(x=c(0,1,10,30,100),
    #                       y=c(0,3,4,5,5))
    # modelE <- lm(y~x,data=dfE)
    # E <- floor(approx(dfE$x,dfE$y,xout = E1, rule=2)[[2]])
    if(E1 < 1) E <- 0
    if(E1 >= 1 && E1 < 10) E <- 3
    if(E1 >= 10 && E1 < 30) E <- 4
    if((E1 >= 30 && E1 <= 100) || E1 > 100) E <- 5
    PHS_H <- PHS_H + 1
  }
  if(PHS_H==6 && HSS1==4 && G==2 && MBA00 > 0){
    # MBAS <- c(MBA00)
    # H <- if(length(unique(MBAS))==1) 0+1 else which(MBAS==max(MBAS))+1
    H <- 2
    #E1
    E1_PP <- if(PPpres) plotAttr(data,region = 2,min = 7,max = 10,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
    E1_WS <- if(WSpres) plotAttr(data,region = 2,min = 7,max = 10,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
    E1_QA <- if(QApres) plotAttr(data,region = 2,min = 7,max = 10,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
    E1 <- E1_PP + E1_WS + E1_QA
    # dfE <- data.frame(x=c(0,1,10,30,100),
    #                       y=c(0,3,4,5,5))
    # modelE <- lm(y~x,data=dfE)
    # E <- floor(approx(dfE$x,dfE$y,xout = E1, rule=2)[[2]])
    if(E1 < 1) E <- 0
    if(E1 >= 1 && E1 < 10) E <- 3
    if(E1 >= 10 && E1 < 30) E <- 4
    if((E1 >= 30 && E1 <= 100) || E1 > 100) E <- 5
    PHS_H <- PHS_H + 1
  }
  if(PHS_H==6 && HSS1==4 && G==1){
    H <- 0
    #E1
    E1_PP <- if(PPpres) plotAttr(data,region = 2,min = 0,max = 7,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
    E1_WS <- if(WSpres) plotAttr(data,region = 2,min = 0,max = 7,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
    E1_QA <- if(QApres) plotAttr(data,region = 2,min = 0,max = 7,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
    E1 <- E1_PP + E1_WS + E1_QA
    # dfE <- data.frame(x=c(0,1,10,30,100),
    #                       y=c(0,3,4,5,5))
    # modelE <- lm(y~x,data=dfE)
    # E <- floor(approx(dfE$x,dfE$y,xout = E1, rule=2)[[2]])
    if(E1 < 1) E <- 0
    if(E1 >= 1 && E1 < 10) E <- 3
    if(E1 >= 10 && E1 < 30) E <- 4
    if((E1 >= 30 && E1 <= 100) || E1 > 100) E <- 5
    PHS_H <- PHS_H + 1
  }
  if(PHS_H==7 && HSS1==4 && H==5){
    #F1
    F1_PP <- if(PPpres) plotAttr(data,region = 2,min = 9,max = 16,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
    F1_WS <- if(WSpres) plotAttr(data,region = 2,min = 9,max = 16,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
    F1_QA <- if(QApres) plotAttr(data,region = 2,min = 9,max = 16,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
    F1 <- F1_PP + F1_WS + F1_QA
    # dfF <- data.frame(x=c(0,1,10,20,100),
    #                       y=c(0,3,4,5,5))
    # modelF <- lm(y~x,data=dfF)
    # F2 <- floor(approx(dfF$x,dfF$y,xout = F1, rule=2)[[2]]) #R reserves "F" as a pre-defined variable, hence the "F2"
    if(F1 < 1) F2 <- 0
    if(F1 >= 1 && F1 < 10) F2 <- 3
    if(F1 >= 10 && F1 < 20) F2 <- 4
    if((F1 >= 20 && F1 <= 100) || F1 > 100) F2 <- 5
    PHS_H <- PHS_H + 1
  }
  if(PHS_H==7 && HSS1==4 && H==4){
    #F1
    F1_PP <- if(PPpres) plotAttr(data,region = 2,min = 6,max = 9,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
    F1_WS <- if(WSpres) plotAttr(data,region = 2,min = 6,max = 9,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
    F1_QA <- if(QApres) plotAttr(data,region = 2,min = 6,max = 9,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
    F1 <- F1_PP + F1_WS + F1_QA
    # dfF <- data.frame(x=c(0,1,10,20,100),
    #                       y=c(0,3,4,5,5))
    # modelF <- lm(y~x,data=dfF)
    # F2 <- floor(approx(dfF$x,dfF$y,xout = F1, rule=2)[[2]]) #R reserves "F" as a pre-defined variable, hence the "F2"
    if(F1 < 1) F2 <- 0
    if(F1 >= 1 && F1 < 10) F2 <- 3
    if(F1 >= 10 && F1 < 20) F2 <- 4
    if((F1 >= 20 && F1 <= 100) || F1 > 100) F2 <- 5
    PHS_H <- PHS_H + 1
  }
  if(PHS_H==7 && HSS1==4 && H==3){
    #F1
    F1_PP <- if(PPpres) plotAttr(data,region = 2,min = 3,max = 6,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
    F1_WS <- if(WSpres) plotAttr(data,region = 2,min = 3,max = 6,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
    F1_QA <- if(QApres) plotAttr(data,region = 2,min = 3,max = 6,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
    F1 <- F1_PP + F1_WS + F1_QA
    # dfF <- data.frame(x=c(0,1,10,20,100),
    #                       y=c(0,3,4,5,5))
    # modelF <- lm(y~x,data=dfF)
    # F2 <- floor(approx(dfF$x,dfF$y,xout = F1, rule=2)[[2]]) #R reserves "F" as a pre-defined variable, hence the "F2"
    if(F1 < 1) F2 <- 0
    if(F1 >= 1 && F1 < 10) F2 <- 3
    if(F1 >= 10 && F1 < 20) F2 <- 4
    if((F1 >= 20 && F1 <= 100) || F1 > 100) F2 <- 5
    PHS_H <- PHS_H + 1
  }
  if(PHS_H==7 && HSS1==4 && H==2){
    #F1
    F1_PP <- if(PPpres) plotAttr(data,region = 2,min = 0,max = 3,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIPO"]]["CC"][[1]][1] else 0
    F1_WS <- if(WSpres) plotAttr(data,region = 2,min = 0,max = 3,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["PIGL"]]["CC"][[1]][1] else 0
    F1_QA <- if(QApres) plotAttr(data,region = 2,min = 0,max = 3,vol1=vol1,vol2=vol2,
                   vol3=vol3)[["POTR5"]]["CC"][[1]][1] else 0
    F1 <- F1_PP + F1_WS + F1_QA
    # dfF <- data.frame(x=c(0,1,10,20,100),
    #                       y=c(0,3,4,5,5))
    # modelF <- lm(y~x,data=dfF)
    # F2 <- floor(approx(dfF$x,dfF$y,xout = F1, rule=2)[[2]]) #R reserves "F" as a pre-defined variable, hence the "F2"
    if(F1 < 1) F2 <- 0
    if(F1 >= 1 && F1 < 10) F2 <- 3
    if(F1 >= 10 && F1 < 20) F2 <- 4
    if((F1 >= 20 && F1 <= 100) || F1 > 100) F2 <- 5
    PHS_H <- PHS_H + 1
  }
  if(PHS_H==7 && HSS1==4 && (H==1 || H==0)){
    # F1 <- plotAttr(data,region = 2,min = 0,max = 0,vol1=vol1,vol2=vol2,vol3=vol3)[["PIPO"]]["CC"][[1]][1] +
    #       plotAttr(data,region = 2,min = 0,max = 0,vol1=vol1,vol2=vol2,vol3=vol3)[["PIGL"]]["CC"][[1]][1] +
    #       plotAttr(data,region = 2,min = 0,max = 0,vol1=vol1,vol2=vol2,vol3=vol3)[["POTR5"]]["CC"][[1]][1]
    # dfF <- data.frame(x=c(0,1,10,20,100),
    #                       y=c(0,3,4,5,5))
    # modelF <- lm(y~x,data=dfF)
    # F2 <- floor(approx(dfF$x,dfF$y,xout = F1)[[2]])
    F2 <- 0 #Same result as above (and KCP this is translated from)
    PHS_H <- PHS_H + 1
  }
  if(PHS_H==8 && HSS1==4){
    OGSC <- A + B + C + D + E + F2 + G + H + I + J + K + L
    PHS_H <- PHS_H + 1
  }
  if(PHS_H==9 && HSS1==4 && OGSC >= 42){
    CHESS <- 50
    PHS_H <- PHS_H + 1
  }

  #Translate CHESS
  CHESStrans <- c(
    "10"="1","20"="2","31"="3A","32"="3B","33"="3C",
    "41"="4A","42"="4B","43"="4C","50"="5"
  )
  HAB_STR_STAGE <- as.character(CHESStrans[as.character(CHESS)])
}#END HSS1_5
  return(HAB_STR_STAGE)
}
