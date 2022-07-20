#############################################################################
#Install.R
#
#This script can be used to install the stateTrans package to a users PC.
#It only needs to be run once, unless user wants to install the package to
#multiple locations on their PC or if an update is made to the vegClass
#package.
#############################################################################

#============================================================================
#STEP 1: Install dependencies for stateTrans package (OPTIONAL see comments
#below for further details)
#============================================================================

#If you are using the version of R that comes installed with FVS, then skip to
#STEP 2.

#If you are NOT using the version of R that is distributed with FVS and you
#DON'T have the RSQLite package already installed, then run the line of code
#below. R code can be run by highlighting code or placing mouse cursor before
#the line of code and pressing ctrl + enter keys.

install.packages(c("RSQLite"))

#============================================================================
#Step 2: Installing stateTrans R package (REQUIRED)
#============================================================================

#Specify the directory where the stateTrans package installation is located
#(stateTrans_0.1.0.tar.gz). Simply replace the directory in the call to the
#install.packages function below.

#NOTE:Two backslashes (\\) or one forward slash (/) should be used for
#specifying directory paths in R. Examples of valid directory paths:
#C:/Veg_Classification/vegClassInstall/vegClass_0.1.0.tar.gz
#C:\\Veg_Classification\\vegClassInstall\\vegClass_0.1.0.tar.gz

#Run the code below by highlighting code or placing mouse cursor before the line
#of code and pressing ctrl + enter keys.

install.packages("C:/Veg_Classification/vegClassInstall/vegClass_0.1.0.tar.gz",
                 repos = NULL,
                 type = "source")
