library(RSQLite)
library(DBI)
library(dplyr)

#############################################################################   
#Start processing - I think this would initiate with a function call
#lets just name this function, main, for the sake of this example.

#Possible function arguments:
#Input:        Directory path or directory path with database name.
#Output:       Directory path or directory path with xlsx name.
#Overwrite:    boolean to determine if current xlsx file should have data 
#              overwritten or have new data appended to existing data.
#groupingCode: this is character string pertaining to a code that data will
#              be summarized by. This could be an ERU or something else.
#              This variable is searched for in FVS_Compute table
#mgmtID:       mgmtID code. This code specifies what data to read in if a 
#              project has multiple runs.If left as NA, then all data is read
#              in. Not sure for the time being how this will be implemented.
#############################################################################

main<- function()
{
  
  #ERROR CHECK: Test existence of .db file. If .db does not exist
  #then return from function.
  
  #PRINT MESSAGE: Message that connection to database is being established if
  #.db is legitimate.
  
  #ERROR CHECK: Test if FVS_TreeList, FVS_Summary2, and FVS_Compute?
  #is found in table. If one of these is missing, then return with an error
  #message and disconnect from database. 
  
  #Test if we need to pull in a grouping variable from the FVS_Compute table
  #based on argument groupingCode.
  
  #Logic for reading in tree list from .db, joining other pertinent tables
  #and removing unneeded tree records etc. starts here. Stand age will need to
  #be joined to tree level data from FVS_StandInit. Contemplating having a 
  #function return a query (string variable) that is passed into dbGetQuery.
  baseDF<- dbGetQuery("SQL logic")
  
  #Subset data based on mgmtID argument if mgmtID is not NA.
  if(!is.na(mgmtID))
  {
    if(! mgmtID %in% unique(baseDF$MgmtID))
    {
      stop("Specified management ID (mgmtID) not found in input database")
    }
    
    else
    {
      baseDF<-baseDF[baseDF$MgmtID == mgmtID]
    }
  }
  
  #After last subset, test if baseDF has no rows for any reason. If this is the
  #case, then return from function with an error.
  
  #Logic for connecting to species support .db
  #Read in the following columns with query: 
  #SPECIES_SYMBOL, GENUS, LEAF_RETEN, R3SHADE_TOL, and R3_DIA_MEAS
  #Logic for connecting to species support .db
  #We don't need to join this information to baseDF
  con<-dbConnect(SQLite(), "C:/R3_StateTransition_Modeling/Species_Documentation/Species_Support.db")
  sp.info<-dbGetQuery(con, "SELECT SPECIES_SYMBOL AS SpeciesFVS, GENUS, LEAF_RETEN,
                         R3_SHADE_TOL, R3_DIA_MEAS
                         FROM SPECIES_INFO")
  dbDisconnnect(con)
  
  #Determine unique stands and initialize a list for storing dataframes for each
  #stand that is processed. The size of the initialized list will have the same
  #length as the stands vector determined by: unique(baseDF$StandID)
  
  stands<- unique(baseDF$StandID)
  stands.output<-vector(mode = "list", length(stands))
  
  #Begin loop across standIDs
  #May want to consider using stand in stands for loop syntax
  for(i in 1:length(stands))
  {
    #Extract stand ID i...
    #PRINT: Message detailing which stand is being processed
    standDF<-baseDF[baseDF$StandID == stands[i], ]
    years<-unique(standDF$Year)
    
    #May consider ordering years. I am not sure if years will ever be out of
    #order for a given stand. Years will need to be in sequential
    #order so we can derive cycle number correctly.
    
    #Initialize list that will store output on a year-by-year basis for standDF
    #The length of this list will match the length of years vector.
    stand.yr.output<-vector(mode = "list", length(years))
    
    #Merge species information, as well as, forest system
    standDF<-merge(standDF, sp.info, by = "SpeciesPlants", all.x = T)
    
    #Derive forest system for ERU using R3 crosswalk function
    
    #Now we enter loop across years for the stand
    for(j in 1:length(years))
    {
      
      #Extract rows for year j
      stdYrFrame<- standDF[standDF$Year == years[j],]
      
      #PRINT: Message indicating which year is being process for stand I
      
      #Create dataframe that will store output for the stand in a given year.
      #There may be more variables that I am missing in dataframe defined below.
      yr.output<-data.frame(StandID = unique(stdYrFrame$StandID),
                            ERU = unqiue(stdYrFrame$ERU),
                            Year = years[j],
                            CycleNum = j)
      
      #Now we calculate helper variables and all the variables requested by
      #region 3. In my mind, the hypothetical functions below will return a 
      #single value for the stand/year combination. These functions will also
      #take in arguments BUT I don't know what they will be yet (quite possibly
      #entire stdYrFrame or specific columns from this dataframe). The names of
      #the functions are all hypothetical too.
      
      yr.output$BA<-sum(stdYrFrame$DBH^2 * 0.005454 * stdYrFrame$TPA)
      yr.output$BA<-standBAFunction()
      yr.output$CC<-standCCFunction()
      yr.output$DOMTYPE<-domTypeFunction()
      yr.output$CANSIZCL<-canSizClsFunction()
      yr.output$BASTORY<-baStoryFunction()
      
      #Once we have derived our variables, we will store yr.output in 
      #stand.yr.output. Each item in stand.yr.output will be a dataframe with a
      #single row of information for the stand/year combination.
      stand.yr.output[[j]]<-yr.output
      
      #Loop continues until we process all the years for the stand
    }
    
    #Combine all year-by-year information for standID i into a single dataframe
    stand.out<-do.call("rbind", stand.yr.output)
    
    #Now we add stand.out dataframe to our list of stands
    stands.output[[i]]<-stand.out
    
    #PRINT: Message indicating that stand i has been processed.
  
  #Loop continues until we process all the stands
    
  }
  
  #Now we combine the information in the stand.output list into a single
  #dataframe
  stand.data<-do.call("rbind", stands.output)
  
  #PRINT: Message indicating that all stands have been processed.
  
  #Send out the stand.data to an .xlsx, .csv, or.db. This will be executed
  #in a function call.
  
  #PRINT: Message that output file is being created
  
  #PRINT: Message when output file has been written
  
  #PRINT: Message that process is complete.
  
  #Return from function main
  return()
}
