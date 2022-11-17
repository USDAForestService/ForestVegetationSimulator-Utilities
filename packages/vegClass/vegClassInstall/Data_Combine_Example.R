################################################################################
#Data_Combine_Example.R
#
#This script shows an example of how to combine the FVS-ready database tables
#from the Arizona and New Mexico FIA data sets into a single output SQLite
#database (FIADB_AZNM.db).
################################################################################

#Load vegClass package
library(vegClass)

#Change directory paths for dbIn and dbOut to match those on your computer.
#Use forward slash (/) or double backslash when specifying directory paths (\\).
#The addEru = T argument will crosswalk PV Codes to USFS Region 3 ERU codes and
#add them as grouping codes to the GROUPS field of the FVS_STANDINIT_PLOT, 
#FVS_STANDINIT_COND, and FVS_PLOTINIT_PLOT database tables.

dbCombine(dbIn = c("C:/Veg_Classification/FIA2FVS_Databases/SQLite_FIADB_AZ/FIADB_AZ.db",
                   "C:/Veg_Classification/FIA2FVS_Databases/SQLite_FIADB_NM/FIADB_NM.db"),
          dbOut = c("C:/Veg_Classification/FIADB_AZNM.db"),
          addERU = T)


