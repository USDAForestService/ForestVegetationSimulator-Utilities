################################################################################
#Function qmdSizCl
#
#This function calculates QMD size class for stand/plot using QMD for seedlings
#and saplings. This value is calculated using USGS region 6 rule sets described
#in Vandendriesche, D., 2013. A Compendium of NFS Regional Vegetation
#Classification Algorithms. USDA Forest Service. Fort Collins, CO
#
#Arguments
#
#qmd:    Quadratic mean diameter of the stand/plot.
#
#debug:  logical variable indicating if debug statements should be printed. By
#        default this value is set to FALSE.
#
#value
#
#0: 0  - 1"  QMD (Nonstocked)
#1: 1  - 5"  QMD (Seed/Sap)
#2: 5  - 10" QMD (Small Tree)
#3: 10 - 15" QMD (Medium Tree)
#4: 15 - 20" QMD (Large Tree)
#5: 20 +"    QMD (Very Large Tree)
################################################################################

#'@export
qmdSizCl <- function(qmd,
                     debug = F)
{
  #Do debug
  if(debug)
  {
    cat("In function qmdSizCl", "\n", "\n")
  }

  #Nonstocked
  if(qmd < 1)
  {
    QMD_SIZCL <- 0
  }

  #Seed/Sap
  else if(qmd >= 1 & qmd < 5)
  {
    QMD_SIZCL <- 1
  }

  #Small Tree
  else if(qmd >= 5 & qmd < 10)
  {
    QMD_SIZCL <- 2
  }

  #Medium Tree
  else if(qmd >= 10 & qmd < 15)
  {
    QMD_SIZCL <- 3
  }

  #Large Tree
  else if(qmd >= 15 & qmd < 20)
  {
    QMD_SIZCL <- 4
  }

  #Very Large Tree
  else
  {
    QMD_SIZCL <- 5
  }

  #Do debug
  if(debug)
  {
    cat("QMD:", qmd, "\n")
    cat("QMDSIZCL:", QMD_SIZCL, "\n")
  }

  return(QMD_SIZCL)

}
