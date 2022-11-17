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
main(input = "C:/FVS/VegClass Example/FVSOut.db",
     output = "C:/Veg_Classification/vegClassOut.csv",
     runTitles = c("MCD Run", "Mew Run"),
     addCompute = T,
     addPotFire = T,
     addFuels = T,
     addCarbon = T,
     addVolume = T,
     vol1DBH = 5,
     vol2DBH = 5,
     vol3DBH = 9,
     startYear = 2022)

#You can call the main function as many times as you want in an R script or R
#project. You can simply copy and paste the code above and change the function
#arguments as needed.

