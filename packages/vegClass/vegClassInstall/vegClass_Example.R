################################################################################
#vegClass_Example.R
#
#This script provides an example of how to call the main function from the
#vegClass package.
################################################################################

#How to run all lines of code in script (.R file):

#RStudio: press Crtl + A key and then press Ctrl + Enter. Alternatively, after
#         selecting all lines of code with Ctrl + A, you can press the Run
#         button.

#RGui: press Crtl + A key and then press Ctrl + R.

#Attach vegClass package
library(vegClass)

#Example call to main function in vegClass package
main(input = "C:/Veg_Classification/vegClassInstall/FVSOut.db",
     output = "C:/Veg_Classification/vegClassInstall/FVSOut.csv",
     removeTag = "eru=",
     runTitles = c("MCD Run", "Mew Run"),
     addCompute = T,
     addPotFire = T,
     startYear = 2022)

