################################################################################
#Function: computeVerticalStructure
#
#Calculates the number of canopy layers
#
#Argument
#
#BAByDiameterClass: vector containing the amount of basal area by diameter class.
#         Classes are 0-4.9", 5-9.9", 10-14.9", 15-19.9", 20-24.9", and 25"+
#
#Return value
#
# - number of Canopy layers (VERTICAL STRUCTURE)--1,2,3 or continuous (C)
################################################################################
#'@export
computeVerticalStructure <- function(BAByDiameterClass){
      num35 <- 1
      for(index9 in 6:3){
        if(BAByDiameterClass[index9] >= 0.02 && BAByDiameterClass[index9-2] >= 0.02 &&
           BAByDiameterClass[index9] >= (1.8*BAByDiameterClass[index9-1]) &&
           BAByDiameterClass[index9-2] >= (1.8*BAByDiameterClass[index9-1])) num35 <- num35+1
      }
      if(num35==1){
        for(index10 in 6:4){
          if(abs(BAByDiameterClass[index10-1]-BAByDiameterClass[index10-2]) <= 0.1 &&
             BAByDiameterClass[index10] >= 0.02 &&
             BAByDiameterClass[index10-3] >= 0.02 &&
             0.9*(BAByDiameterClass[index10-1]+BAByDiameterClass[index10-2]) <= BAByDiameterClass[index10] &&
             0.9*BAByDiameterClass[index10-1]+BAByDiameterClass[index10-2] <= BAByDiameterClass[index10-3]) num35 <- num35+1
        }
      }
      if(num35==1){
        num36 <- 0
        for(index11 in 1:length(BAByDiameterClass)){
          if(BAByDiameterClass[index11] >= 0.02) num36 <- num36+1
          VERTICAL_STRUCTURE <- "C"
          if(num36 < 5 &&
           (BAByDiameterClass[1] < 0.02 ||
            BAByDiameterClass[2] < 0.02 ||
            BAByDiameterClass[3] < 0.02)) VERTICAL_STRUCTURE <- as.character(num35)
        }
      }
      else {
        VERTICAL_STRUCTURE <- "C"
        if(num35 <= 3) VERTICAL_STRUCTURE <- as.character(num35)
      }
      return(VERTICAL_STRUCTURE)
}
