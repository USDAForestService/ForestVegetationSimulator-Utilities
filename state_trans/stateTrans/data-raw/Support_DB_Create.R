#############################################################################
#Species_DB_Create.R
#
#This script is used to create the R3 support SQLite database.
#############################################################################

library(readxl)
library(RSQLite)
library(devtools)

source("C:/State_Transition/stateTrans/data-raw/Species_Functions.R")

#============================================================================
# Derive SPEICES_INFO table
#============================================================================

#Read in FIA REF_SPECIES.csv
supportSP<-read.csv("C:/State_Transition/Species_Documentation/REF_SPECIES.csv")

#Extract SPCD, COMMON_NAME, GENUS, SPECIES, and SPECIES_SYMBOL
supportSP<-supportSP[c("SPCD","COMMON_NAME","GENUS","SPECIES", "SPECIES_SYMBOL")];head(supportSP)

#Capitalize columns and create column for scientific species names
#(GENUS + SPECIES)
supportSP$COMMON_NAME<-toupper(supportSP$COMMON_NAME);head(supportSP)
supportSP$GENUS<-toupper(supportSP$GENUS);head(supportSP)
supportSP$SPECIES<-toupper(supportSP$SPECIES);head(supportSP)
supportSP$SPECIES_SYMBOL<-toupper(supportSP$SPECIES_SYMBOL);head(supportSP)
supportSP$SCI_SPEICES<-paste(supportSP$GENUS, supportSP$SPECIES);head(supportSP)

#Create HW_SW column
supportSP$TYPE<-mapply(spGetHWSW, supportSP$SPCD);head(supportSP)

#Conditionally set values of 0 to EVERGREEN and values 1 to DECIDUOUS
supportSP$TYPE<-ifelse(supportSP$TYPE == 0, "EVERGREEN", "DECIDUOUS");head(supportSP)

#Remove FIA species codes less than 1000
supportSP<-subset(supportSP, SPCD < 1000);head(supportSP)

#Read in R3 species spreadsheet
r3.data<-read_excel("C:/State_Transition/stateTrans/data-raw/FIA_FVS_Tables_Ryan_Nov2021.xlsx",
           sheet = "default tree assignments")

colnames(r3.data)[names(r3.data)=='PLANTS Code']<-'SPECIES_SYMBOL';names(r3.data)

#Merge Shade tolerance and diameter measure to R3 support data using plants symbol as key
supportSP<-merge(supportSP, subset(r3.data, select = c("SPECIES_SYMBOL", "Shade Tolerance",
                                                 "Diameter Measure", "Leaf Retention")),
                 by = "SPECIES_SYMBOL", all.x = T)

#Rename column headers in fia.table
colnames(supportSP)<-c("SpeciesPLANTS", "SPCD", "COMMON_NAME", "GENUS", "SPECIES",
                    "SCI_NAME", "FIA_TYPE", "R3_SHADE_TOL", "R3_DIA_MEAS", "LEAF_RETEN")
head(supportSP)

#Reorder column headers
supportSP<-supportSP[,c("SpeciesPLANTS", "SPCD", "GENUS", "FIA_TYPE", "LEAF_RETEN",
                  "R3_SHADE_TOL", "R3_DIA_MEAS")]
head(supportSP)

#Clean R3_DIA_MEAS values
supportSP$R3_DIA_MEAS<-ifelse(supportSP$R3_DIA_MEAS == 'n/a', NA, supportSP$R3_DIA_MEAS)
table(supportSP$R3_DIA_MEAS)

#Re-code shade tolerance values to the following:
#intolerant - INT
#mod intolerant - MINT
#mod tolerant - MTOL
#tolerant - TOL
#very intolerant - VINT
#very tolerant - VTOL
supportSP$R3_SHADE_TOL<-ifelse(supportSP$R3_SHADE_TOL == 'intolerant', 'INT',
                     ifelse(supportSP$R3_SHADE_TOL == 'mod intolerant', 'INT',
                     ifelse(supportSP$R3_SHADE_TOL == 'mod tolerant', 'TOL',
                     ifelse(supportSP$R3_SHADE_TOL == 'tolerant', 'TOL',
                     ifelse(supportSP$R3_SHADE_TOL == 'very intolerant', 'INT',
                     ifelse(supportSP$R3_SHADE_TOL == 'very tolerant', 'TOL',
                            supportSP$R3_SHADE_TOL))))))
table(supportSP$R3_SHADE_TOL)

#Capitalize strings in leaf retention column
supportSP$LEAF_RETEN<-toupper(supportSP$LEAF_RETEN)

#If LEAF_RETEN is NA set to value of TYPE
supportSP$LEAF_RETEN<-ifelse(is.na(supportSP$LEAF_RETEN), supportSP$FIA_TYPE, supportSP$LEAF_RETEN)

#Sort supportSP on SPCD
# supportSP<-supportSP[order(supportSP$SPCD),];head(supportSP)

#============================================================================
# Derive ERU_INFO table
#============================================================================

r3.eru<-read_excel("C:/State_Transition/stateTrans/data-raw/R3_Habitat_Types_Working.xlsx",
                    sheet = "habitat type x eru")

#Remove duplicate rows based on ERU
r3.eru<-r3.eru[!duplicated(r3.eru$ERU_CODE),]

#Extract SYSTEM_TYPE and ERU_CODE columns
r3.eru<-r3.eru[c("SYSTEM_TYPE", "ERU_CODE")]

#Rename column headers
colnames(r3.eru)<-c("SYSTEM_TYPE", "ERU");names(r3.eru)

#Reorder columns
r3.eru<-r3.eru[,c("ERU","SYSTEM_TYPE")];head(r3.eru)

#Make SYSTEM_TYPE values uppercase
r3.eru$SYSTEM_TYPE<-toupper(r3.eru$SYSTEM_TYPE);table(r3.eru$SYSTEM_TYPE)

#============================================================================
#Send data to data folder
#============================================================================\

# usethis::use_data(supportSP)

#============================================================================
#Save fia.sp as Rdata.
#============================================================================

save(supportSP, file= "C:/State_Transition/stateTrans/data/supportSP.RData")

#============================================================================
#Write fia.sp as csv.
#============================================================================

# write.csv(fia.sp, file= "C:/R3_StateTransition_Modeling/R_Code/SupportDB.csv",
#           row.names = F)

#============================================================================
#Write out .db with SPECIES_INFO and ERU_INFO tables
#============================================================================

# #Write out species db
# con<-dbConnect(SQLite(), "C:/R3_StateTransition_Modeling/R_Code/Support.db")
#
# dbWriteTable(con,
#              name = "SPECIES_INFO",
#              fia.sp,
#              overwrite = T)
#
# dbWriteTable(con,
#              name = "ERU_INFO",
#              r3.eru,
#              overwrite = T)
#
# dbDisconnect(con)
