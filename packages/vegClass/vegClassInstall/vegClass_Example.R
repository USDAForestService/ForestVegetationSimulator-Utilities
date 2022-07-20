#############################################################################
#vegClass_Example.R
#
#This script provides an example of how to call the main function from the
#vegClass package.
#############################################################################

#Attach vegClass package
library(vegClass)

#Example call to main function
main(input = "C:/Veg_Classification/vegClassInstall/FVSOut.db",
     output = "C:/Veg_Classification/vegClassInstall/FVSOut.csv",
     overwriteOut = F,
     groupTag = "eru=",
     runTitles = c("MCD Run", "MEW Run"),
     allRuns = F)

#You can call function main as many times as you want in an R script or R
#project. You can simply copy and paste the code above and change the arguments
#as needed.

