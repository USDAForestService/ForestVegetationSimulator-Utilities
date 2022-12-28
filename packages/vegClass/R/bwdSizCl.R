################################################################################
#Function bwdSizCl
#
#This function calculates basal area weighted diameter size class using basal
#area weighted diameter for seedlings and saplings. This value is calculated
#using USFS region 1 and 4 rule sets described in Vandendriesche, D., 2013. A
#Compendium of NFS Regional Vegetation Classification Algorithms. USDA Forest
#Service. Fort Collins, CO R1-9 and R4-6.
#
#Arguments
#
#bawtdia: Basal area weighted diameter of stand/plot.
#
#bawtht:  Basal area weighted height of stand/plot.
#
#tpa:     Trees per acre of stand/plot.
#
#region:  USFS region code. Valid values are 1, 2, 3, 4, 5, 6, 8, 9, or 10.
#
#debug:   logical variable indicating if debug statements should be printed. By
#         default this value is set to FALSE.
#
#value
#
#Basal area weighted diameter class (BWD_SIZCL)
#0: 9          (Nonstocked)
#0: 0   - 0.1" (Seedling)
#1: 0.1 - 5"   (Sapling)
#2: 5   - 10"  (Small Tree)
#3: 10  - 15"  (Medium Tree)
#4: 15  - 20"  (Large Tree)
#5: 20  - 25"  (Very Large Tree)
#6: 25"+       (Giant Tree)
################################################################################

#'@export
bwdSizCl <- function(bawtdia,
                     bawtht,
                     tpa,
                     debug = F)
{
  #Do debug
  if(debug)
  {
    cat("In function bwdSizCl", "\n")
  }

  #Non stocked: tpa is less than 100
  if(tpa < 100)
  {
    BWD_SIZCL = 9
  }

  #Seedling
  else if(bawtdia <= 0.1 & bawtht < 4.5)
  {
    BWD_SIZCL = 0
  }

  #Sapling
  else if(bawtdia >= 0.1 & bawtdia < 5)
  {
    BWD_SIZCL = 1
  }

  #Small Tree
  else if(bawtdia >= 5 & bawtdia < 10)
  {
    BWD_SIZCL = 2
  }

  #Medium Tree
  else if(bawtdia >= 10 & bawtdia < 15)
  {
    BWD_SIZCL = 3
  }

  #Large Tree
  else if(bawtdia >= 15 & bawtdia < 20)
  {
    BWD_SIZCL = 4
  }

  #Very Large Tree
  else if(bawtdia >= 20 & bawtdia < 25)
  {
    BWD_SIZCL = 5
  }

  #Giant Tree
  else
  {
    BWD_SIZCL = 6
  }

  #Do debug
  if(debug)
  {
    cat("BAWTDIA:", bawtdia, "\n")
    cat("BAWTHT:", bawtht, "\n")
    cat("TPA:", tpa, "\n")
    cat("BWD_SIZCL:", BWD_SIZCL, "\n")
  }

  return(BWD_SIZCL)
}

