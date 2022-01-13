#############################################################################
#FVS_ST_Functions.R
#############################################################################

library(RSQLite)
library(dplyr)
library(DBI)

#Region 3 PV codes dimensioned by 7 values per row
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

#Region 3 ERU codes dimensioned by 7 values per row
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

#############################################################################
#Function pvConvert
#
#This function converts PV codes to USFS Region 3 ERU codes.
#
#Arguments
#
#pv: input PV code.
#############################################################################

pvConvert<-function(pv)
{
  #Search for pv in pvCodes
  pvIndex<-match(pv, PVCODE)
  
  #If pvIndex is not NA then extract eru based on pvIndex
  if(!is.na(pvIndex))
  {
    value<-ERU[pvIndex]
  }
  
  #Else assign value as NA
  else
  {
    value = NA
  }
  
  return(value)
}

pvtest<-data.frame(PV_CODE = PVCODE);pvtest
pvtest$ERU<-mapply(pvConvert, pvtest$PV_CODE);head(pvtest)

#############################################################################
#Function getPvEru
#
#This function returns a dataframe containing Region 3 PV codes and ERUs.
#This dataframe can be used to merge PV codes or ERUs to another dataframe.
#
#Arguments: this function does not contain any arguments; simply call
#           getPvEru().
#############################################################################

getPvEru<-function()
{
  data<-data.frame(PV_CODE = PVCODE,
                   ERU = ERU)
  
  return(data)
}

#############################################################################
#Function: validateDBInputs
#
#This function performs a series of checks to see if incoming FVS output
#database is ready for processing. A message is returned from the function
#which will determine if processing is terminated or continued.
#
#con:       Connection to input .db
#runTitle:  Name of FVS run title that will be used to query input db defined
#           by con.
#groupCode: String variable pertaining to code that will be pulled from 
#           FVS_Compute table found in con argument.
#############################################################################

# con<-dbConnect(SQLite(), "C:/FVS/R3_Work/FVSOut.db")
# dbDisconnect(con)
validateDBInputs<-function(con, runTitle, groupCode)
{
  
  #Assign value of PASS to message
  message<-"PASS"
  
  #==========================================================================
  #If FVS_TreeList, FVS_Cases, or FVS_Summary 2 is missing then create an
  #error message.
  #==========================================================================
  
  if(is.element(F, c("FVS_TreeList", "FVS_Cases", "FVS_Summary2") %in%
                dbListTables(con)))
  {
    message<-"One of the following tables was not found in incoming database:
    FVS_TreeList, FVS_Cases, and FVS_Summary2."
    
    return(message)
  }
  
  #==========================================================================
  #If runTitle is not NA then make sure the input run title is found in 
  #FVS_Cases table. If it is not found, then create an error message.
  #==========================================================================
  
  if(!is.na(runTitle) & 
          (! runTitle %in% unique(dbGetQuery(con, "SELECT RunTitle FROM FVS_Cases")[,1])))
  {
    message<-(paste("Run title", paste0("\'",runTitle,"\'"), "was not found in input database. Please ensure that run title
                   is spelled correctly."))
    return(message)
  }
  
  #==========================================================================
  #Execute series of tests if groupCode is not NA.
  #==========================================================================
  
  if(!is.na(groupCode))
  {
    #If FVS_Compute table is not found, then create an error message.
    if(!dbExistsTable(con, "FVS_Compute"))
    {
      message<-paste("Group code", paste0("\'",groupCode,"\'"), "could not be retrieved because FVS_Compute table was not
      found in input database.")
      return(message)
    }
    
    #If group code is not found, then create an error message 
    if(! groupCode %in% dbListFields(con, "FVS_Compute"))
    {
      message <- paste("Group code", paste0("\'",groupCode,"\'"),"is specified but was not found in FVS_Compute 
                 table. Please make sure it is spelled correctly.")
      return(message)
    }
  }
  
  #==========================================================================
  #Database has passed checks. Return value of 'PASS'.
  #==========================================================================

  return(message)
}

# cat(validateDBInputs(con, "Run 2", NA))

#############################################################################
#Function: buildQuery
#
#This function builds and returns an SQL query that is used to read in data
#from an output FVS database. The returned database is the one that will be
#used to derive variables for state and transition models.
#
#Arguments
#mgmtID:    Code corresponding to management ID that will be used to query
#           input db defined by con.
#groupCode: String variable pertaining to code that will be pulled from 
#           FVS_Compute table found in con argument.
#############################################################################

buildQuery<-function(con, runTitle, groupCode)
{
  
  #A conditional WHERE query may need to be added for RunTitle in above query
  #A conditional JOIN may need to be added to bring groupCode variable into the
  #query above
  #FVS_TreeList.RunTitle =", RunTitle)
  
  query<-
    paste("SELECT TL.CaseID, TL.StandID, TL.Year,
              TL.SpeciesPLANTS, TL.TPA, TL.DBH,
              0.005454*TL.DBH*TL.DBH*TL.TPA as BA,
              (3.141593*TL.CrWidth/2*TL.CrWidth/2)*TL.TPA/43560 * 100 AS TREECC
              FVS_Cases.RunTitle, FVS_Summary2.Age
FROM FVS_TreeList TL INNER JOIN FVS_Cases
 ON FVS_TreeList.CaseID = FVS_Cases.CaseID
INNER JOIN FVS_Summary2
 ON FVS_TreeList.CaseID = FVS_Summary2.CaseID AND
    FVS_TreeList.Year = FVS_Summary2.Year")
  
  #Initialize an empty query. This blank query will be returned if runTitle or
  #groupCode is invalid.
  query<-""
  
  #Define prelimenary query
  query<- paste()
}

#############################################################################
#Function: calcCanClassDC
#
#This function takes in a DBH value and returns a diameter class based on R3
#midscale mapping, timber dominance type, or woodland  dominance types.
#These diameter classes are described in NFS_Reg_Veg_Class.pdf. The type
#argument defined below determines which type of diameter class to return.
#
#Arguments
#TYPE: Indicator variable used to determine which type of diameter class to
#      return
#      1 - Midscale mapping (default and will be used if any value other than 
#          2 or 3 is entered for TYPE arugment.)
#      2 - Timberland dominance type
#      3 - Woodland dominance type
#
#DBH:  diameter of tree record
#
#Possible return values when TYPE = 1
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium and large tree canopy cover
#4 = very large tree canopy cover
#5 - giant tree canopy cover
#
#Possible return values when TYPE = 2
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium tree canopy cover
#4 = large - giant tree cover
#
#Possible return values when TYPE = 3
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium - giant tree cover
#############################################################################

calcCanSizeDC<-function(TYPE = 1, DBH)
{
  #Define lower and upper DBH limits for midscale mapping
  #Lower diameter limit
  diameters<-c(0, 5, 10, 20, 30)
    
  #Upper diameter limit
  # upper<-c(5, 10, 20, 30, 30)
    
  #Diameter class values
  dcls<-seq(from = 1, to = 5, by = 1)
  
  #Define lower and upper DBH limits for timber dominance types
  if(TYPE == 2)
  {
    #Lower diameter limit
    diameters<-c(0, 5, 10, 20)
      
    #Upper diameter limit
    # upper<-c(5, 10, 20, 20)
      
    #Diameter class values
    dcls<-seq(from = 1, to = 4, by = 1)
  }
  
  #Define lower and upper DBH limits for woodland dominance types
  if(TYPE == 3)
  {
    #Lower diameter limit
    diameters<-c(0, 5, 10)
    
    #Upper diameter limit
    # upper<-c(5, 10, 10)
    
    #Diameter class values
    dcls<-seq(from = 1, to = 3, by = 1)
  }
  
  #Find diameter class
  dc<-findCategory(DBH, diameters, dcls)

  return(dc)
}

#############################################################################
#Function: findCategory
#
#This function returns a category or classification from input argument 
#validOutputs based on the value of input argument x. The following logic
#is used to determine a value from validOutputs using x and/or inputValues
#arguments:
#
#1) If inputValues has only one value, then first value in outputValues is 
#   returned.
#
#2) If x is less than first item in inputValues, then value specified in
#   invalidReturn arugment is returned. If useLowerBound is true, then
#   value in the first item of outputValues is returned.
#
#3) If x is greater than last value in inputValues, the last item in
#   validOutputs is returned.
#
#4) When 1 and 2 are not true, then x is compared against all i-th and ith+1
#   values in inputValues. If x is a value GE to ith value and LT ith + 1
#   value in inputValues, then ith value in outputValues is returned.
#
#Arguments
#x:             Incoming value to evaluate. Must be number and not NA.
#inputValues:   Values to compare x against. This argument has to be the same
#               length as outputValues.
#outputValues:  Classification values to return containing depending on value
#               x. This arugment has to be the same length as inputValues.
#invalidReturn: Default value to return when:
#               1) x is not a valid value
#               2) InputValues is empty, a character vector, or contains any
#                  NA values.
#               3) Length of inputValues does not equal outputValues.
#############################################################################

findCategory<-function(x, inputValues = 0, outputValues = 0, invalidReturn = 0,
                       useLowerBound = F)
{
  
  #Initialize return category (cat) to invalidReturn
  cat = invalidReturn
  
  #If x is character or NA, return.
  if(is.character(x) | is.na(x))
  {
    return(cat)
  }
  
  #If InputValues is less than length 1, a character vector, or contains any NA
  #values, then return.
  if(length(inputValues) < 1 | is.character(inputValues) | NA %in% inputValues)
  {
    return(cat)
  }
  
  #If inputValues and outputValues are not the same length, then return.
  if(length(inputValues) != length(outputValues))
  {
    return(cat)
  }
  
  #===========================
  #Look for category
  #===========================
  
  #If x is less than first item in inputValues then cat is assigned invalidReturn
  #If useLowerBound == T then cat is assigned the first item in outputValues.
  if(x < inputValues[1])
  {
    cat = invalidReturn
    if(useLowerBound == T) cat = outputValues[1]
  }
  
  #Test if x is greater than last value in outputValues
  else if (x >= inputValues[length(inputValues)])
  {
    cat = outputValues[length(outputValues)]
  }
  
  #Else, loop across length of inputValues vector and determine diameter class
  #for x value.
  else
  {
    done = F
    i = 1
    while(done == F)
    {
      
      #If at the end of inputValues vector, then done becomes true. This is
      #a precautionary condition if a valid output is not found for x during
      #search. This condition should rarely, if ever, evaluate as true...
      if(i == length(inputValues))
      {
        done = T
      }
      
      #If x is between the ith and ith+ 1 items in inputValues, then cat is 
      #assigned the ith value in outputValues vector and done becomes true.
      else if(x >= inputValues[i] & x < inputValues[i + 1])
      {
        cat = outputValues[i]
        done = T
      }
      
      #Else keep searching
      else
      {
        i = i + 1
      }
    }
  }
  
  return(cat)
}


# findDC(5)
# test<-findCategory(20, inputValues = c(0, 5, 10, 15, 20), outputValues = c(1,2,3,4,5),
#                    999, useLowerBound = T);test

#############################################################################
#Function: calcCanSizCl
#
#This function takes in a dataframe containing mid scale mapping diameter
#class and treecc and returns a canopy size class value ranging from 1-5.
#Canopy size class is determined by calculating percent canopy cover for 
#each mid scale mapping diameter class. The diameter class with the largest
#amount of percent canopy cover is returned.
#
#Argument
#dat: dataframe containing diameter class (DC) and canopy percent cover of
#tree record (TREECC).
#
#Possible return values
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium and large tree canopy cover
#4 = very large tree canopy cover
#5 - giant tree canopy cover
#############################################################################

calcCanSizeCl<-function(dat)
{
  #Summarize CC by midscale diameter class
  can.sum<- dat %>%
    group_by(DC) %>%
    summarize(CC = sum(TREECC))
  
  #Extract cansizcl associated with maximum CC
  cansizcl<-can.sum$DC[which.max(can.sum$CC)]
  
  return(cansizcl)
}
