#############################################################################
#ST_Processing_Structure_Template.R
#
#This script provides a template in code/pseudo code for how FVS output
#databases will be used to produce output for stand and transition (ST)
#models.
#############################################################################

library(RSQLite)
library(DBI)
library(dplyr)

#############################################################################   
#Processing sequence - I think this would initiate with a function call
#lets just name this function, main, for the sake of this example.

#Potential function arguments:
#input:        Directory path or directory path with database name.
#output:       Directory path or directory path with xlsx name.
#overwrite:    boolean to determine if output file should have data 
#              overwritten or have new data appended to existing data.
#groupCode:    this is a grouping code that data will be summarized by. This
#              could be an ERU or other type of code. This variable will be
#              stored and retrieved from the FVS_Compute table.
#runTitle:     FVS runtitle which specifies what data to prcoess if an input 
#              project has multiple runs.If left as NA, then all data is 
#              processed from input database defined in argument input.
#############################################################################

main<- function(input, output, overwrite, groupCode, runTitle)
{
  
  #ERROR CHECK: Test existence of .db file. If .db does not exist
  #then return from function.
  
  #ERROR CHECK: Probably need to test if output path (output argument) is valid.
  #Not sure how to implement or if it is 100% necessary.
  
  #PRINT MESSAGE: Message that connection to database is being established if
  #.db is legitimate.
  
  #Perform database validation done in validateDBInput function (see 
  #ST_Functions.R).
  message<-validateDBInputs(con, runTitle, groupCode)
  
  #If validateDBInput function returns a message that is not 'PASS' then
  #halt execution of main function and print error message to console.
  if(message != 'PASS')
  {
    stop(cat(message))
  }
  
  #Generate a query that will be created by buildQuery function (see 
  #ST_Functions.R).
  dbQuery<-buildQuery(runTitle, groupCode)
  
  #Execute SQL query to obtain input data
  baseDF<-dbGetQuery(con, dbQuery)
  
  #Test if baseDF has no rows (or is null?) for any reason. If this is the case,
  #then return from main function with an error.
  
  #Read in support .db. We may want to hardcode this db as a dataframe so
  #we can avoid distributing a SQL database with this information. We could 
  #simply call a function that returns this dataframe. This would be worth 
  #discussing. Logic shown below may become obsolete.
  
  #Read in the following columns:
  #SPECIES_SYMBOL, GENUS, LEAF_RETEN, R3SHADE_TOL, and R3_DIA_MEAS
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
  for(i in 1:length(stands))
  {
    #Extract stand ID i...
    #PRINT: Message detailing which stand is being processed
    standDF<-baseDF[baseDF$StandID == stands[i], ]

    #Determine years that will be evaluated
    years<-unique(standDF$Year)
    
    #May consider ordering years. I am not sure if years will ever be out of
    #order for a given stand. Years will need to be in sequentialorder so we
    #can derive cycle number correctly as shown further below.
    
    #Initialize list that will store output on a year-by-year basis for standDF
    #The length of this list will match the length of years vector.
    standYrOutput<-vector(mode = "list", length(years))
    
    #We could maybe do look ups for the next two indented comments. Also not 
    #even sure if we need forest system for any calculations currently.
    
      #Merge species information, as well as, forest system
      # standDF<-merge(standDF, sp.info, by = "SpeciesPlants", all.x = T)
    
      #Derive forest system for ERU using R3 crosswalk function
    
    #Now we enter loop across years for the stand
    for(j in 1:length(years))
    {
      
      #Extract rows for year j
      stdYrFrame<- standDF[standDF$Year == years[j],]
      
      #PRINT: Message indicating which year is being process for stand I
      
      #Create dataframe that will store output for the stand in a given year.
      #There may be more variables that I am missing in dataframe defined below.
      #We need to find a way to conditional add groupCode column if user is 
      #assuming one (i.e. groupCode argument is not NA)
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
      
      #Stand level BA
      yr.output$BA<-sum(stdYrFrame$DBH^2 * 0.005454 * stdYrFrame$TPA)
      
      #Stand percent canopy cover
      yr.output$CC<-standCCFunction()
      
      #Dominance type of stand
      yr.output$DOMTYPE<-domTypeFunction()
      
      #Canopy size class
      yr.output$CANSIZCL<-canSizClsFunction()
      
      #BA storiedness
      yr.output$BASTORY<-baStoryFunction()
      
      #Once we have derived our variables, we will store yr.output in 
      #stand.yr.output. Each item in stand.yr.output will be a dataframe with a
      #single row of information for the stand/year combination.
      stand.yr.output[[j]]<-yr.output
      
      #Loop continues until we process all the years for the stand
    }
    
    #Combine all year-by-year information for standID i into a single dataframe.
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
  
  #Send out the stand.data to an .xlsx, .csv, or.db. This will likely be executed
  #in a function call.
  
  #PRINT: Message that output file is being created
  
  #PRINT: Message when output file has been written
  
  #PRINT: Message that process is complete.
  
  #Return from function main
  return()
}
