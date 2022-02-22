#############################################################################
#main.R
#
#This script contains the main function which will be used to derive output
#for state and transition models.
#############################################################################

#NOTE:
#If we end up making a package for this work then the library and source
#statements will not be included in this file.

library(RSQLite)
library(DBI)
library(dplyr)
library(openxlsx)

source("C:/OpenFVS/utility/state_trans/canSizCls.R")
source("C:/OpenFVS/utility/state_trans/dbInput.R")
source("C:/OpenFVS/utility/state_trans/domType.R")

#############################################################################   
#Function : main.R
#
#Arguments:
#input:        Directory path and file name to a SQLite database (.db). Path
#              name must be surrounded with double quotes "" and double
#              back slashes or single forward slashes need to be used for 
#              specifying paths. Database defined in input must contain the
#              following tables: FVS_Summary2, FVS_TreeList, and FVS_Cases.
# 
#              Examples of valid input formats:
#              "C:/FVS/R3_Work/FVSOut.db"
#              "C:\\FVS\\R3_Work\\FVSOut.db"
#
#output:       Directory path and filename to an .csv or .xlsx file. Path
#              name must be surrounded with double quotes "" and double
#              back slashes or single forward slashes need to be used for 
#              specifying paths.
#              
#              Examples of valid output formats:
#              "C:/FVS/R3_Work/FVSOut.xlsx"
#              "C:\\FVS\\R3_Work\\FVSOut.xlsx"
#              "C:/FVS/R3_Work/FVSOut.csv"
#              "C:\\FVS\\R3_Work\\FVSOut.csv"
#
#overwriteOut: Boolean variable used to determine if output file should have
#              data overwritten or have new data appended to an existing file
#              as determined by the output argument. If value is T, any 
#              information existing in output will be overwritten with new
#              information. If value is F, then the new ouput will be appended
#              to existing file. The default value of this argument is F.
#
#groupTag:     This is a grouping tag that will be used to extract a grouping
#              code (such as ERU) from a set of FVS group labels. For instance,
#              if you have a group label such as ERU=MCD, then the groupTag
#              would be ERU= and you would have a value of MCD returned in 
#              the output. The default value of this argument is NA. The value
#              for groupTag must be surrounded in double quotes.
#              
#              Example group tag:
#              "ERU="
#
#runTitle:     FVS runTitle which will be used to query the input database
#              as defined by input argument. When run title is specified,
#              then output will only be produced for this runtitle. If
#              runTitle is not specified, then output will be produced for
#              all data in input database. The value of runTitle must be 
#              surrounded with double quotes. The contents in runTitle 
#              argument need to be spelled correctly and use approporiate case
#              sensitivity.If the value of runTitle is not NA, and is spelled
#              incorrectly the execution of main function will terminate.
#
#              Example run title:
#              "Run 1"
#############################################################################

main<- function(input, output, overwriteOut = F, groupTag = NA, runTitle = NA)
{
  #Mark: I have commented out the sub statement below. R will not allow a user
  #to specify single \ in a directory path because it is an escape
  #character (annoying). A user will have to specify a path with \\ or / for 
  #input and output arguments (unless there is some other workaround).
  
  #replace backslashes into forward slashes in path
  #May need to require user to input path as forward slashes 
  # input<-sub("\\","/", input)
  
  #NEED TO FIGURE OUT RELATIVE PATHS!!!!
  #Test if input file extension is valid.
  if (!(file.exists(input))){
    stop('Invalid input database file. Program Terminated.')
  }
  
  #Extract file extension.
  fileExt<-sub("(.*)\\.","",output)
  
  #Test if output file extension is valid (.xlsx or .csv).
  if(!fileExt %in% c("xlsx", "csv"))
  {
    stop("Output argument does not have a valid file extension. File extension must be
         either .xlsx or .csv.")
  }
  
  #Connect to input database
  con<-dbConnect(RSQLite::SQLite(), input)
  cat("Connected to input database:", input, "\n")
  
  #Perform database validation
  message<-validateDBInputs(con, runTitle)
  
  #If validateDBInput function returns a message that is not 'PASS' then
  #disconnect from con and send error message to console.
  if(message != 'PASS')
  {
    #Disconnect from con
    dbDisonnct(con)
    
    #Print error message
    stop(message)
  }
  
  #Read in info from SupportDB.csv. The path here will have to be changed in order
  #to reflect the path where this data will be stored in R package.
  spInfo<-read.csv("C:/OpenFVS/utility/state_trans/SupportDB.csv")
  
  #Obtain unique list of stand ids to process
  stands<-unique(dbGetQuery(con, "SELECT StandID FROM FVS_Summary2")[,1])
  cat("Total number of stands to prcoess:", length(stands), "\n")
  
  #Split stand vector into list containing vectors of stand IDs (length 250, this
  #could be user defined)
  standList<-split(stands, ceiling(seq_along(stands)/250))
  # cat("List of stands for processing created.", "\n")
  
  #Initialize list for storing stand output. Will have the same length as the 
  #standList.
  allStandsOutput<-vector(mode = "list", length(standList))
  # cat("Length of allStandsOutput:", length(allStandsOutput), "\n")
  
  #==============================
  #Begin loop across standList
  #==============================
  
  #Initialize standSum. This will keep track of number of stands processed.
  standSum<-0
  
  for(i in 1:length(standList))
  {

    #Select stands to process
    standSelect<-standList[[i]]
    cat("Length of standSelect:", length(standSelect), "\n")
    
    #Generate a query that will be created by buildQuery function (see 
    #ST_Functions.R).
    dbQuery<-buildQuery(standSelect, runTitle)
    
    #Execute SQL query to obtain list of stand data
    dfDat<-dbGetQuery(con, dbQuery)
    dfList<-split(dfDat, dfDat$StandID)
    rm(dfDat)
    cat("Length of dfList", length(dfList), "\n")
    
    #Initialize list that will store output for group of stands i.
    standSelectOutput<-vector(mode="list",length(dfList))
    cat("Length of standSelectOutput:", length(standSelectOutput), "\n")
    
    #==============================
    #Begin loop across dfList
    #==============================
    
    for(j in 1:length(dfList))
    {
      #Extract stand ID i...
      #PRINT: Message detailing which stand is being processed
      standDF<-dfList[[j]]
      cat("Processing stand:", unique(standDF$StandID),"\n")
      
      #Determine years that will be evaluated
      years<-unique(standDF$Year)
      
      #Sort years
      years<-sort(years)
      
      #Create list that will store output for stand j for all years
      standYrOutput<-vector(mode = "list", length(years))
      
      #Merge species information, as well as, forest system
      standDF<-merge(standDF, spInfo, by = "SpeciesPLANTS", all.x = T)
      
      #==============================
      #Begin loop across years
      #==============================
      
      for(k in 1:length(years))
      {
        
        #Extract rows for year j
        standYrDF<- standDF[standDF$Year == years[k],]
        
        #Determine group to report in yrOutput
        group<-getGroup(standYrDF["Groups"][[1]][1], groupTag)
        
        #Create dataframe that will store output for the stand in a given year.
        yrOutput<-data.frame(GROUP = group,
                             PLOT_ID = unique(standYrDF$StandID),
                             CY = k,
                             PROJ_YEAR = years[k])
        
        #Now we calculate helper variables and all the variables requested by
        #region 3. In my mind, the hypothetical functions below will return a 
        #single value for the stand/year combination. These functions will also
        #take in arguments BUT I don't know what they will be yet (quite possibly
        #entire stdYrFrame or specific columns from this dataframe). The names of
        #the functions are all hypothetical too.
        
        #Calculate tree BA
        standYrDF$TREEBA<-standYrDF$DBH^2 * standYrDF$TPA * 0.005454
        
        #Calculate tree CC
        standYrDF$TREECC<-pi * (standYrDF$CrWidth/2)^2 *(standYrDF$TPA/43560) * 100 
        
        #Calculate standBA
        standBA<-sum(standYrDF$TREEBA)
        
        #Calculate stand percent canopy cover (uncorrected)
        standCC<-sum(standYrDF$TREECC)
        yrOutput$CAN_COV<-standCC
        
        # #Dominance type of stand
        yrOutput$DOM_TYPE<-domType(standYrDF)
        
        #Canopy size class - midscale mapping
        yrOutput$CAN_SIZCL<-canSizeCl(standYrDF[c("DBH", "TREECC")], 1)
        
        #Canopy size class - timberland
        yrOutput$CAN_SZTMB<-canSizeCl(standYrDF[c("DBH", "TREECC")], 2)
        
        #Canopy size class - woodland
        yrOutput$CAN_SZWDL<-canSizeCl(standYrDF[c("DBH", "TREECC")], 3)
        
        # #BA storiedness
        # yrOutput$BASTORY<-baStoryFunction()
        
        #Add yrOutput to standYrOutput
        standYrOutput[[k]]<-yrOutput
        
        #==================================
        #End of loop across years
        #==================================
      }
      
      #Combine all year-by-year information for standID i into a single dataframe.
      standOut<-bind_rows(standYrOutput)
      
      #Now we add stand.out dataframe to our list of stands
      standSelectOutput[[j]]<-standOut
      
      #==================================
      #End of loop across selected stands
      #==================================
    }
    
    #Update standSum and send to console
    standSum<-standSum + length(dfList)
    cat(standSum, "stands processed out of", length(stands), "\n")
    
    #Combine data for selected stands
    standSelectOut<-bind_rows(standSelectOutput)
    
    #Added data for group of stands to allStandsOutput.
    allStandsOutput[[i]]<-standSelectOut
    
    #==================================
    #End of loop across all stands
    #==================================
  }
  
  #Print out how many stands had zero tree records
  zeroTree<-length(stands) - standSum
  cat(zeroTree, "stands contained no tree records.", "\n")
  
  #Disconnect from con
  dbDisconnect(con)
  cat("Disconnected from input database:", input, "\n")
  
  #Combine all output from allStandsOutput
  allOut<-bind_rows(allStandsOutput)
  
  #Message indicating that data is being sent to output.
  cat("Sending data to output file:", output, "\n")
  
  #Determine file extension for specified output file.
  if(fileExt == 'xlsx')
  {
    #Determine if file exist. If file exists and overwrite is false, then
    #append data to existing file.
    if(file.exists(output) & overwriteOut == F)
    {
      #Read in existing data from output file. Here we assume that old output
      #data is in worksheet 1.
      oldData<-readWorkbook(output)
      
      #Combine new results with old data
      allOut<-bind_rows(oldData, allOut)
      
      #Send updated results back to workbook
      write.xlsx(allOut, output, overwrite = T)
    }
    
    #Else overwrite existing file.
    else
    {
      write.xlsx(allOut, output, overwrite = T)
    }
  }
  
  #Determine file extension for specified output file.
  if(fileExt == 'csv')
  {
    
    #Add tab to standIDs. This avoids the problems of stand IDs being
    #converted to scientific notation in Excel.
    allOut$PLOT_ID<-paste0(allOut$PLOT_ID, "\t")
    
    #Determine if file exist. If file exists and overwrite is false, then
    #append data to existing file.
    if(file.exists(output) & overwriteOut == F)
    {
      
      #Send updated results back to workbook
      write.table(allOut, output, sep = ",", append = T, row.names = F)
    }
    
    #Else overwrite existing file.
    else
    {
      write.table(allOut, output, sep = ",", row.names = F)
    }
  }
  
  #PRINT: Message when output file has been written
  cat("Data sent to output file:", output, "\n")
  
  #Return from function main
  return(cat("End of program.", "\n"))
}
