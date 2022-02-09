#############################################################################
#ST_Functions.R
#
#This script contains a set of functions that will be used to process FVS
#output databases and create output required for state and transition (ST)
#models.
#############################################################################

#Packages required for use of functions
library(RSQLite)
library(dplyr)
library(DBI)

#############################################################################
#Function: validateDBInputs
#CURRENT STATUS: This function is a work in progress.
#
#This function performs a series of checks to see if incoming FVS output
#database is ready for processing. A message is returned from the function
#which will determine if processing of input database in main function 
#is terminated or continued.
#
#Arguments
#con:       Connection to input .db
#runTitle:  Name of FVS run title that will be searched for in FVS_Cases
#           table if not NA.
#groupCode: Grouping code that will be searched for in FVS_Compute table if
#           not NA.
#############################################################################

# con<-dbConnect(SQLite(), "C:/FVS/R3_Work/FVSOut.db")
# dbDisconnect(con)
validateDBInputs<-function(con, runTitle, groupCode)
{
  
  #Assign value of PASS to message
  message<-"PASS"
  
  #==========================================================================
  #If FVS_TreeList, FVS_Cases, or FVS_Summary 2 is missing then create an
  #error message and return..
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
  #FVS_Cases table. If it is not found, then create an error message and
  #return.
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
    #If FVS_Compute table is not found, then create an error message and return.
    if(!dbExistsTable(con, "FVS_Compute"))
    {
      message<-paste("Group code", paste0("\'",groupCode,"\'"), "could not be retrieved because FVS_Compute table was not
      found in input database.")
      return(message)
    }
    
    #If group code is not found, then create an error message and return. 
    if(! groupCode %in% dbListFields(con, "FVS_Compute"))
    {
      message <- paste("Group code", paste0("\'",groupCode,"\'"),"is specified but was not found in FVS_Compute 
                 table. Please make sure it is spelled correctly.")
      return(message)
    }
  }
  
  #==========================================================================
  #Database has passed checks. Return value of 'PASS' as defined previously.
  #==========================================================================

  return(message)
}

# cat(validateDBInputs(con, "Run 2", NA))

#############################################################################
#Function: buildQuery
#CURRENT STATUS: This function is a work in progress and currently does
#nothing.
#
#This function builds and returns an SQL query that is used to read in data
#from an output FVS database in main function.
#
#Arguments
#runTitle:  Name of FVS run title that will be used to query input data found
#           in con. If this value is NA, then data found in con will not be
#           queried by run title.
#groupCode: Grouping code that will be joined to treelist data that is
#           is processed in main function. If this value is NA, then no 
#           grouping code will be joined to treelist data.
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
          (3.141593*TL.CrWidth/2*TL.CrWidth/2)*TL.TPA/43560 * 100 AS TREECC,
          FVS_Cases.RunTitle, FVS_Summary2.Age 
          FROM FVS_TreeList TL 
          INNER JOIN FVS_Cases 
          ON TL.CaseID = FVS_Cases.CaseID 
          INNER JOIN FVS_Summary2 
          ON TL.CaseID = FVS_Summary2.CaseID AND
          TL.Year = FVS_Summary2.Year")
  
  #Initialize an empty query. This blank query will be returned if runTitle or
  #groupCode is invalid.
  query<-""
  
  #Define prelimenary query
  query<- paste()
}

#############################################################################
#Function: calcCanClassDC
#CURRENT STATUS: This function could use a little more testing. Will remove
#the commented out upper vectors at some point.
#
#This function takes in a DBH value and returns a diameter class based on R3
#midscale mapping, timber dominance type, or woodland  dominance types criteria.
#These different diameter classes are described in NFS_Reg_Veg_Class.pdf.
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
#CURRENT STATUS: I think this function is mostly working but could use a
#little more testing.
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
#4) When 1 - 3 are not true, then x is compared against all i-th and ith+1
#   values in inputValues. If x is a value GE to ith value and LT ith + 1
#   value in inputValues, then ith value in outputValues is returned.
#
#Arguments
#x:             Incoming value to evaluate. Must be numeric and not NA.
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
#CURRENT STATUS: This function could use some more work. Not sure what
#happens yet if dataframe with all dead trees is passed in or if there
#is a value to return if total stand percent canopy cover is below a certain
#value.
#
#This function takes in a dataframe containing diameter class and treecc and
#returns a canopy size class value ranging from 1-5. Canopy size class is 
#determined by calculating percent canopy cover for each mid scale mapping
#diameter class. The diameter class with the largest amount of percent 
#canopy cover is returned.
#
#Argument
#dat: tree level dataframe containing diameter class (DC) and canopy percent
#cover of each tree record (TREECC).
#
#Possible return values
#1 - 5
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
