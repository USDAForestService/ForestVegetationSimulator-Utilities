################################################################################
#Function: computeDominance6040
#
#Calculates dominance group 6040
#
#Arguments
#
#dTotal:  Stand-level trees per acre
#
#spec1:   Character vector of species in the stand (USDA Plants 4-digit code(s))
#
#prop1:   Numeric vector of the proportions of TPA/BA by species in the stand
#
#diam1:   Numeric vector of the basal area weighted average diameters by species
#         in the stand
#
#height1: Numeric vector of the arithmetic/basal area weighted average heights
#         by species in the stand
#
#Return value
#
# - DOM6040
################################################################################


# create data dictionaries for species, dominance class and covertype mapping
     MapSpecies <- c(
       "GF"="ABGR", "BA"="POPUL", "CW"="POPUL", "NC"="POPUL",
       "PW"="POPUL", "GA"="FRPE", "AF"="ABLA", "L"="LAOC",
       "WB"="PIAL", "S"="PIEN", "LP"="PICO", "WP"="PIMO3",
       "PP"="PIPO", "DF"="PSME", "C"="THPL", "WH"="TSHE",
       "LM"="PIFL2", "LL"="LALY", "JU"="JUNIP","RM"="JUNIP",
       "PB"="BEPA", "CO"="POPUL", "AS"="POTR5", "OH"="2TREE",
       "OS"="2TREE", "MH"="TSME", "PI"="2TREE", "OT"="2TREE",
       "WL"="LAOC", "ES"="PIEN", "RC"="THPL", "MM"="ACER",
       "ACGL"="ACER", "ACGR3"="ACER","PY"="TABR2", "TABR2"="TABR2",
       "ABGR"= "ABGR", "ABCO"="ABGR", "ABLA"="ABLA", "LAOC"="LAOC",
       "PIAL"="PIAL", "PIEN"="PIEN", "PIGL"="PIEN","PICO"="PICO",
       "PIMO3"="PIMO3", "PIPO"="PIPO", "PSME"="PSME", "THPL"="THPL",
       "TSHE"="TSHE", "PIFL2"="PIFL2", "LALY"="LALY", "JUNIP"="JUNIP",
       "JUOC"="JUNIP", "JUOS"="JUNIP", "JUSC2"="JUNIP", "BEPA"="BEPA",
       "BEGL"="BEPA", "BEOC2"="BEPA", "BETUL"="BEPA", "POPUL"="POPUL",
       "POBA2"="POPUL", "PODEM"="POPUL", "POTR15"="POPUL", "POAN3"="POPUL",
       "POBAT"="POPUL", "POTR5"="POTR5", "2TREE"="2TREE", "2TE"="2TREE",
       "2DE"="2TREE", "CELE3"="CELE3", "TSME"="TSME", "FRPE"="FRPE",
       "ALRU2"="ALDER", "ALRH"="ALDER"
      )
      MapDominance6040SpeciestoSubclass <- c(
       "2TD"="HMIX", "2TE"="IMIX", "2TREE"="IMIX", "ABCO"="TMIX",
       "ABGR"="TMIX", "ABLA"="TMIX", "ACER"="HMIX", "ALDER"="HMIX",
       "ALRH2"="HMIX", "ALRU2"="HMIX", "BEGL"="HMIX", "BEOC2"="HMIX",
       "BEPA"="HMIX", "BETUL"="HMIX", "CELE3"="IMIX", "FRPE"="HMIX",
       "JUNIP"="IMIX", "JUOC"="IMIX", "JUOS"="IMIX", "JUSC2"="IMIX",
       "LALY"="IMIX", "LAOC"="IMIX", "PIAL"="IMIX", "PICO"="IMIX",
       "PIEN"="TMIX", "PIFL2"="IMIX", "PIGL"="TMIX", "PIMO3"="IMIX",
       "PIPO"="IMIX", "POAN3"="HMIX", "POBA2"="HMIX", "POBAT"="HMIX",
       "PODEM"="HMIX", "POPUL"="HMIX", "POTR15"="HMIX", "POTR5"="HMIX",
       "PSME"="IMIX", "TABR2"="TMIX", "THPL"="TMIX", "TSHE"="TMIX",
       "TSME"="TMIX", "AF"="TMIX", "AS"="HMIX", "BA"="HMIX",
       "CW"="HMIX", "DF"="IMIX", "ES"="TMIX", "GA"="HMIX",
       "GF"="TMIX", "LL"="IMIX", "LM"="IMIX", "LP"="IMIX",
       "MH"="TMIX", "MM"="HMIX", "NC"="HMIX", "OH"="HMIX",
       "OS"="IMIX", "PB"="HMIX", "PP"="IMIX", "PW"="HMIX",
       "PY"="TMIX", "RC"="TMIX", "RM"="IMIX", "WB"="IMIX",
       "WH"="TMIX", "WL"="IMIX", "WP"="IMIX","NONE"="NONE"
      )

computeDominance6040 <- function(dTotal,spec1,prop1,diam1,height1){

  subclasscode <- ""
  collapsedproportion <- 0.0
  # This orders the stands based on proportion, with proportion of 1 being
  # the largest
  collapsedproportion <- 0.0
  if(length(prop1)>1){
    for(index1 in 1:(length(prop1)-1)){
      for(index2 in (index1+1):(length(prop1))){
        if(prop1[index1]< prop1[index2]){
          num1 <- prop1[index1]
          prop1[index1] <- prop1[index2]
          prop1[index2] <- num1
          num2 <- height1[index1]
          height1[index1] <- height1[index2]
          height1[index2] <- num2
          num3 <- diam1[index1]
          diam1[index1] <- diam1[index2]
          diam1[index2] <- num3
          Str <- spec1[index1]
          spec1[index1] <- spec1[index2]
          spec1[index2] <- Str
        } #if proportions are the same between 2 species, break ties based on height
        if(prop1[index1]==prop1[index2] && height1[index1] < height1[index2]){
          num4 <- prop1[index1]
          prop1[index1] <- prop1[index2]
          prop1[index2] <- num4
          num5 <- height1[index1]
          height1[index1] <- height1[index2]
          height1[index2] <- num5
          num6 <- diam1[index1]
          diam1[index1] <- diam1[index2]
          diam1[index2] <- num6
          str <- spec1[index1]
          spec1[index1] <- spec1[index2]
          spec1[index2] <- str
        }
      }
    }
  }
  if(prop1[1] >= 0.6){
    DOM6040 <- as.character(MapSpecies[spec1[1]])
    if(DOM6040=="NA" || is.na(DOM6040))DOM6040 <- "NONE"
    collapsedproportion <- prop1[1]
  }
  else{
    TmixProp <- 0.0
    ImixProp <- 0.0
    HmixProp <- 0.0
    TotalMixProp <- 0.0
    str <- ""
    for(index in 1:(length(prop1))){
      TotalMixProp <- TotalMixProp + prop1[index]
      check <- match(spec1[index],as.character(MapSpecies))
      if (!is.na(check) && check > 0){
        key <- as.character(MapSpecies[spec1[index]])
        if(as.character(MapDominance6040SpeciestoSubclass[key])=="TMIX"){
          TmixProp <- TmixProp + prop1[index]
        }
        if(as.character(MapDominance6040SpeciestoSubclass[key])=="IMIX"){
          ImixProp <- ImixProp + prop1[index]
        }
        if(as.character(MapDominance6040SpeciestoSubclass[key])=="HMIX"){
          HmixProp <- HmixProp + prop1[index]
        }
      }
    }

    num11 <- 0.0
    if(HmixProp/TotalMixProp >= 0.4){
      str <- "HMIX"
      num11 <- HmixProp
    }
    if(round((HmixProp + ImixProp)/TotalMixProp,1) >= 0.5){
      str <- "IMIX"
      num11 <- ImixProp
    }
    if(round((HmixProp + ImixProp)/TotalMixProp,1) < 0.5){
      str <- "TMIX"
      num11 <- TmixProp
    }

    subclasscode <- str
    if(prop1[1] >= 0.4){
      key <- spec1[1] #default
      if(prop1[1]==prop1[2]){#proportions are equal - go to diameter as tie breaker
        if(diam1[1]==diam1[2]){#diameters are equal - go to height as tie breaker
          if(height1[1] < height1[2]) key <- spec1[2]
          if(height1[1]==height1[2]){#heights are equal
            check1 <- match(spec1[1],as.character(MapSpecies))#get index if spec[1] in MapSpecies
            check2 <- match(spec1[2],as.character(MapSpecies))#get index if spec[2] in MapSpecies
            #if both checks are not NA and spec1[1] follows spec[2] on the
            # MapSpecies list, go with alphabetical order
            if(!any(is.na(c(check1,check2))) && check1 > check2) key <- spec1[2]
          }
        }
        else{
          if(diam1[1] < diam1[2]) key <- spec1[2] #diameter tie breaker
          check1 <- match(spec1[1],as.character(MapSpecies))#get index if spec[1] in MapSpecies
          check2 <- match(spec1[2],as.character(MapSpecies))#get index if spec[2] in MapSpecies
          #if diameters are equal, both checks are not NA, and spec1[1] follows
          #spec[2] on the MapSpecies list, go with alphabetical order
          if(diam1[1]==diam1[2] && !any(is.na(c(check1,check2))) && check1 > check2) key <- spec1[2]
        }
      }
      DOM6040_1 <- as.character(MapSpecies[key])
      if(DOM6040_1=="NA" || is.na(DOM6040_1)){
        DOM6040 <- "NONE"
        } else DOM6040 <- paste0(DOM6040_1,"-",str)
      collapsedproportion <- prop1[1]
    }
    else {
      DOM6040 <- str
      collapsedproportion <- num11
    }
  }
  if(DOM6040=="NA" || is.na(DOM6040) || length(grep("^NA",DOM6040))) DOM6040 <- "NONE"
  subclasscode <- DOM6040
  return(DOM6040)
}
