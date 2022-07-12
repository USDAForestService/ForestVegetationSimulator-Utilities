#############################################################################
#Function: fvsGaak
#
#This function creates a gaak table with the SQL statements for various
#grouping codes.
#
#Argument
#
#dbName: Name of database used in gaak table.
#
#type:   Variable to determine what grouping codes are included in GAAK table.
#        1 = Standard FVS grouping codes (All_Stands, All_Plots)
#        2 = FIA grouping codes (All_FIA_Conditions, All_FIA_Plots,
#            All_FIA_Subplots)
#        3 = Both standard FVS grouping codes and FIA grouping codes
#        Default value for type argument is 2.
#
#Return value
#
#Dataframe containing gaak table
#############################################################################

#'@export
fvsGaak<-function(dbName="FVS_Data", type = 2)
{
  #Capture invalid type argument values
  if(type < 1 | type > 3) type = 2

  #Create dataframe containg FVS_GroupAddfilesAndKeywords table
  gaak<-data.frame(GROUPS = c("All_Stands","All_Plots","All_FIA_Conditions","All_FIA_Plots", "All_FIA_Subplots"),
                   ADDFILES = c("","","","",""),
                   FVSKEYWORDS = c(paste("Database", "DSNin", paste0(dbName, ".db"), "StandSQL",
                                         "SELECT *", "FROM  FVS_StandInit", "WHERE Stand_ID = '%StandID%'",
                                         "EndSQL","TreeSQL", "SELECT *", "FROM FVS_TreeInit",
                                         "WHERE Stand_ID ='%StandID%'", "EndSQL", "END", sep = "\n"),
                                   paste("Database", "DSNin", paste0(dbName, ".db"), "StandSQL",
                                         "SELECT *", "FROM  FVS_PlotInit", "WHERE StandPlot_ID = '%StandID%'",
                                         "EndSQL","TreeSQL", "SELECT *", "FROM FVS_TreeInit",
                                         "WHERE StandPlot_ID ='%StandID%'", "EndSQL", "END", sep = "\n"),
                                   paste("Database", "DSNin", paste0(dbName, ".db"), "StandSQL",
                                         "SELECT *", "FROM  FVS_StandInit_Cond", "WHERE Stand_CN = '%Stand_CN%'",
                                         "EndSQL","TreeSQL", "SELECT *", "FROM FVS_TreeInit_Cond",
                                         "WHERE Stand_CN ='%Stand_CN%'", "EndSQL", "END", sep = "\n"),
                                   paste("Database", "DSNin", paste0(dbName, ".db"), "StandSQL",
                                         "SELECT *", "FROM  FVS_StandInit_Plot", "WHERE Stand_CN = '%Stand_CN%'",
                                         "EndSQL","TreeSQL", "SELECT *", "FROM FVS_TreeInit_Plot",
                                         "WHERE Stand_CN ='%Stand_CN%'", "EndSQL", "END", sep = "\n"),
                                   paste("Database", "DSNin", paste0(dbName, ".db"), "StandSQL",
                                         "SELECT *", "FROM  FVS_PlotInit_Plot", "WHERE StandPlot_CN = '%Stand_CN%'",
                                         "EndSQL","TreeSQL", "SELECT *", "FROM FVS_TreeInit_Plot",
                                         "WHERE StandPlot_CN ='%Stand_CN%'", "EndSQL", "END", sep = "\n")))

  #If type is 1, return GAAK with just FVS grouping codes
  if(type == 1)
  {
    gaak<-gaak[1:2,]
  }

  #If type is 2, return GAAK with just FIA grouping codes
  if(type == 2)
  {
    gaak<-gaak[3:5,]
  }

  return(gaak)
}

#############################################################################
#Function: pvCodes
#
#This function returns a vector of PV Codes (R3 PV Codes)
#
#Argument
#
#None
#
#Return value
#
#Vector containing PV Codes
#############################################################################

#Region 3 PV codes dimensioned by 7 values per row
pvCodes<-function(){
  PVCODE<-c(
  "006090",  "238300",  "012330",  "012331",  "012332",  "012430",  "01239",
  "01203",   "012320",  "012140",  "012141",  "012143",  "012142",  "012341",
  "012340",  "01213",   "12999",   "01241",   "012380",  "012350",  "012361",
  "012360",  "012362",  "012420",  "001040",  "001041",  "001042",  "001130",
  "001120",  "001022",  "001021",  "001150",  "001050",  "001052",  "001054",
  "001053",  "001051",  "001090",  "001141",  "001140",  "001111",  "011130",
  "001060",  "4999",    "3999",    "003300",  "003301",  "003090",  "003370",
  "003320",  "003111",  "003112",  "003110",  "003310",  "003080",  "003350",
  "003060",  "003240",  "003231",  "003200",  "003203",  "003201",  "003202",
  "00604",   "004320",  "00435",   "004351",  "004350",  "004340",  "004060",
  "004061",  "004062",  "004300",  "004330",  "004310",  "004360",  "00415",
  "004152",  "004151",  "001100",  "238040",  "238310",  "006130",  "006060",
  "006080",  "006010",  "006070",  "006071",  "012333",  "01231",   "240300",
  "001080",  "001081",  "001160",  "001020",  "011092",  "011093",  "011091",
  "011090",  "011470",  "011380",  "011030",  "011032",  "011033",  "011031",
  "011350",  "011400",  "011330",  "011460",  "011340",  "011341",  "011390",
  "011391",  "011392",  "011035",  "011210",  "011215",  "011211",  "011212",
  "011214",  "011213",  "011216",  "011500",  "640999",  "032010",  "32999",
  "032030",  "033020",  "033010",  "033030",  "011410",  "011411",  "011034",
  "011440",  "011361",  "011360",  "011430",  "011420",  "011220",  "011320",
  "011370",  "03101",   "630030",  "630050",  "630040",  "630043",  "232060",
  "232050",  "630010",  "2040302", "2040301", "204360",  "9000042", "20431",
  "204023",  "204022",  "204021",  "204024",  "204370",  "20406",   "204999",
  "20411",   "20410",   "204300",  "204350",  "204500",  "231020",  "232030",
  "23204",   "232999",  "233999",  "233020",  "233021",  "233022",  "2025002",
  "2040303", "204050",  "20404",   "204321",  "202500",  "232330",  "233330",
  "204330",  "232020",  "204320",  "231010",  "03102",   "232070",  "230030",
  "230999",  "230040",  "230042",  "230041",  "233050",  "233030",  "233010",
  "233041",  "233040",  "233042",  "204400",  "204011",  "204010",  "204012",
  "20441",   "202020",  "231030",  "201040",  "201350",  "201020",  "201331",
  "201332",  "201333",  "201340",  "201400",  "20140",   "202320",  "202321",
  "202999",  "9000043", "201999",  "231021",  "231050",  "231040",  "201010",
  "201011",  "231999",  "201410",  "202331",  "202330",  "31999",   "632999",
  "630041",  "630042",  "620040",  "620030",  "620999",  "620010",  "620020",
  "620021",  "630020",  "610020",  "610010",  "6500103", "650999",  "201420",
  "201430",  "210999")

  return(PVCODE)
}

#############################################################################
#Function: pvCodes
#
#This function returns a vector of ERU codes (R3 ERU Codes)
#
#Argument
#
#None
#
#Return value
#
#Vector containing ERU Codes
#############################################################################

#Region 3 ERU codes dimensioned by 7 values per row
eru<-function(){
  ERU<-c(
  "MCD", "MCD", "MCD", "MCD", "MCD", "MCD", "MCD",
  "MCD", "MCD", "MCD", "MCD", "MCD", "MCD", "MCD",
  "MCD", "MCD", "MCD", "MCD", "MCD", "MCD", "MCD",
  "MCD", "MCD", "MCD", "MCD", "MCD", "MCD", "MCD",
  "MCD", "MCD", "MCD", "MCD", "MCD", "MCD", "MCD",
  "MCD", "MCD", "MCD", "MCD", "MCD", "MCD", "MCD",
  "MCD", "SFP", "SFP", "SFP", "SFP", "SFP", "SFP",
  "SFP", "SFP", "SFP", "SFP", "SFP", "SFP", "SFP",
  "SFP", "SFP", "SFP", "SFP", "SFP", "SFP", "SFP",
  "SFM", "SFM", "SFM", "SFM", "SFM", "SFM", "SFM",
  "SFM", "SFM", "SFM", "SFM", "SFM", "SFM", "SFM",
  "SFM", "SFM", "SFM", "BPi", "BPi", "MCW", "MCW",
  "MCW", "MCW", "MCW", "MCW", "MCW", "MCW", "MCW",
  "MCW", "MCW", "MCW", "MCW", "PPG", "PPG", "PPG",
  "PPG", "PPG", "PPG", "PPG", "PPG", "PPG", "PPG",
  "PPG", "PPG", "PPG", "PPG", "PPG", "PPG", "PPG",
  "PPG", "PPG", "PPO", "PPO", "PPO", "PPO", "PPO",
  "PPO", "PPO", "PPO", "PPO", "GAM", "PPE", "PPE",
  "PPE", "PPE", "PPE", "PPE", "PPE", "PPE", "PPE",
  "PPE", "PPE", "PPE", "PPE", "PPE", "PPE", "PPE",
  "PPE", "MPO", "MPO", "MPO", "MPO", "MPO", "MPO",
  "MPO", "MPO", "MPO", "MPO", "MPO", "PJG", "PJG",
  "PJG", "PJG", "PJG", "PJG", "PJG", "PJG", "PJG",
  "PJG", "PJG", "PJG", "PJG", "PJG", "PJG", "PJG",
  "PJG", "PJG", "PJG", "PJG", "PJG", "PJG", "PJO",
  "PJO", "PJO", "PJO", "PJO", "PJO", "PJO", "PJO",
  "PJO", "PJO", "PJO", "PJC", "PJC", "PJC", "PJC",
  "PJC", "PJC", "PJC", "PJC", "PJC", "PJC", "PJC",
  "PJC", "PJC", "PJC", "PJC", "PJS", "PJS", "PJS",
  "PJS", "PJS", "JUG", "JUG", "JUG", "JUG", "JUG",
  "JUG", "JUG", "JUG", "JUG", "JUG", "JUG", "JUG",
  "JUG", "JUG", "JUG", "JUG", "JUG", "JUG", "JUG",
  "JUG", "JUG", "JUG", "JUG", "JUG", "MEW", "MEW",
  "MEW", "MEW", "MEW", "MEW", "MEW", "MEW", "MEW",
  "MEW", "MEW", "MEW", "MEW", "MEW", "MEW", "SDG",
  "SDG", "SDG")

  return(ERU)
}

#############################################################################
#Function pvConvert
#
#This function converts PV codes to USFS Region 3 ERU codes.
#
#Arguments
#
#pv: input PV code.
#
#Return value
#
#ERU code.
#############################################################################

#'@export
pvConvert<-function(pv)
{
  #Search for pv in pvCodes
  pvIndex<-match(pv, pvCodes())

  #If pvIndex is not NA then extract eru based on pvIndex
  if(!is.na(pvIndex))
  {
    value<-eru()[pvIndex]
  }

  #Else assign value as NA
  else
  {
    value = NA
  }

  return(value)
}
