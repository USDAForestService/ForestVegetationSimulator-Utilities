#############################################################################
#main.R
#
#This script contains the main function which will be used to derive
#vegetation classifications.
#############################################################################

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
#output:       Directory path and filename to a .csv file. Path name must be
#              surrounded with double quotes "" and double back slashes or
#              single forward slashes need to be used for specifying paths.
#
#              Examples of valid output formats:
#              "C:/FVS/R3_Work/FVSOut.csv"
#              "C:\\FVS\\R3_Work\\FVSOut.csv"
#
#overwriteOut: Boolean variable used to determine if output file should have
#              data overwritten or have new data appended to an existing file
#              as determined by the output argument. If value is T, any
#              information existing in output will be overwritten with new
#              information. If value is F, then the new output will be appended
#              to existing file. The default value of this argument is F.
#
#groupTag:     This is a grouping tag that will be used to extract a grouping
#              code (such as ERU) from a set of FVS group labels. For instance,
#              if you have a group label such as ERU=MCD, then the groupTag
#              would be ERU= and you would have a value of MCD returned in
#              the output. The default value of this argument is ---. The value
#              for groupTag must be surrounded in double quotes.
#
#              Example group tag:
#              "ERU="
#
#runTitles:    Vector of FVS runTitles that will be processed. If
#              runTitles is left as NULL, execution of main function will
#              terminate. Each run title in runTitles must be surrounded by
#              double quotes. The values specified for runTitles argument need
#              to be spelled correctly. If any of the values in runTitles are
#              spelled incorrectly, the execution of main function will
#              terminate.
#
#              Example of how to specify single run title:
#              runTitles = "Run 1"
#
#              Example of how to specify multiple run titles:
#              runTitles = c("Run 1", "Run 2",...)
#              Note the use of the c(...) when processing multiple runs.
#
#allRuns:      Boolean variable that is used to determine if all runs in
#              argument input should be processed. If value is TRUE (T), then
#              all runs will be processed and any runs specified in argument
#              runTitles will be ignored. By default this argument is set
#              to FALSE (F).
#
#numToQuery:   Integer value corresponding to the number of stands that are
#              queried from the input argument at a single time. In general,
#              higher values will lead to more RAM being used in R but faster
#              processing time. Lower values will lead to less RAM being used
#              in R but slower processing time. By default this argument is
#              set to 50.
#############################################################################

#'@export
main<- function(input,
                output,
                overwriteOut = F,
                groupTag = "---",
                runTitles = "Run 1",
                allRuns = F,
                numToQuery = 50)
{

  #Including this statement to avoid NOTE that occurs when vegClass package
  #is built.
  supportSP<-NULL

  #Change \\ to / in input and output arguments
  input <- gsub("\\\\", "/", input)
  output <- gsub("\\\\", "/", output)

  ###########################################################################
  #Check input arguments
  ###########################################################################

  #Test existence of input database.
  if (!(file.exists(input))){
    stop(paste("Input database not found. Make sure directory path and file name",
               "in input are spelled correctly."))
  }

  #Extract file extension for input argument.
  fileExtIn<-sub("(.*)\\.","",input)

  #Make sure input database is SQLite (.db).
  if (!fileExtIn %in% "db"){
    stop(paste("Input argument does not have a valid file extension. File",
               "extension must be .db."))
  }

  #Extract path to output by extract all characters before the last / in output.
  outPath <- gsub("/[^/]+$", "", output)

  #Test existence of output path and if it does not exist report error.
  if (!(file.exists(outPath))){
    stop(paste("Path to output:", outPath, "was not found.",
    "Make sure directory path to output is spelled correctly."))
  }

  #Extract file extension for output argument.
  fileExtOut<-sub("(.*)\\.","",output)

  #Test if output file extension is valid (.csv).
  if(!fileExtOut %in% c("csv"))
  {
    stop("Output argument does not have a valid file extension. File extension must be
         .csv.")
  }

  #Capitalize runTitles and groupTag
  runTitles<-toupper(runTitles)
  groupTag<-toupper(groupTag)

  #Connect to input database
  con<-RSQLite::dbConnect(RSQLite::SQLite(), input)
  cat("Connected to input database:", input, "\n")

  ###########################################################################
  #Perform checks on input database (con)
  ###########################################################################

  #Perform database validation
  message<-validateDBInputs(con, runTitles, allRuns)

  #If validateDBInput function returns a message that is not 'PASS' then
  #disconnect from con and send error message to console.
  if(message != 'PASS')
  {
    #Disconnect from con
    RSQLite::dbDisconnect(con)

    #Print error message
    stop(message)
  }

  #If the output file exists and overWriteOut is true, unlink the file
  if(file.exists(output) & overwriteOut) unlink(output)

  #If all runs is in effect, extract all unique runTitles from input (con).
  if(allRuns)
  {
    runTitles<-unique(RSQLite::dbGetQuery(con, "SELECT RunTitle FROM FVS_Cases")[,1])
    runTitles<-toupper(runTitles)
  }

  #Store supportSp in dataframe
  supportSP<-vegClass::supportSP

  #==============================
  #Begin loop across runTitles
  #==============================

  for(r in 1:length(runTitles))
  {
    #Extract run from runTitles
    run<-runTitles[r]

    #Print run that is being processed
    cat(paste0(rep("*", 75), collapse = ""), "\n")
    cat("*", "Processing run:", run, "\n")
    cat(paste0(rep("*", 75), collapse = ""), "\n")

    #Obtain unique list of stand ids to process for run.
    standQuery<- paste("SELECT FVS_Cases.StandID
                      FROM FVS_Cases
                      WHERE RunTitle LIKE ", paste0("'%",run,"%'"))
    stands<-RSQLite::dbGetQuery(con, standQuery)[,1]

    cat("Total number of stands to process for run", paste0(run,":"),
        length(stands),"\n")

    #Split stand vector into list containing vectors of stand IDs
    standList<-split(stands,
                     ceiling(seq_along(stands)/numToQuery))

    #Initialize standSum. This will keep track of number of total stands processed.
    standSum<-0

    #Initialize noLiveTrees. This variable keeps track of number of stands that
    #have no live trees during the simulation time frame.
    noLiveTrees<-0

    #Initialize noValidRecords. This variable is used for determining number of
    #stands that have no valid tree records (live or dead). This occurs if a
    #stand/plot has a record in the STANDINIT/PLOTINIT table but not records in
    #the corresponding TREEINIT table of the input FVS dataset.
    noValidRecords<-0

    #Initialize invalidStands. This variable is used to keep track of number of
    #stands that are flagged as invalid during processing. Stands are considered
    #invalid when the inventory is comprised entirely of dead trees.
    invalidStands <- 0

    #==============================
    #Begin loop across standList
    #==============================

    for(i in 1:length(standList))
    {
      #Select stands to process
      standSelect<-standList[[i]]
      cat("Number of stands to query:", length(standSelect), "\n")

      #Generate a query that will be used to read data from input argument (FVS
      #output database)
      dbQuery<-buildQuery(standSelect, run)

      cat("Querying stands...", "\n")

      #Execute SQL query to obtain list of stand data
      dfDat<-RSQLite::dbGetQuery(con, dbQuery)

      cat("Stand query complete.", "\n")

      #If query yields no results, skip to next iteration of loop across
      #standList
      if(nrow(dfDat) <= 0)
      {
        cat("No valid tree records found in stand query.", "\n")
        next
      }

      #==============================
      #Begin loop across standSelect
      #==============================

      for(j in 1:length(standSelect))
      {
        #Extract stand from dfDat
        standDF<-dfDat[dfDat$StandID %in% standSelect[j],]
        cat("Processing stand:", standSelect[j],"\n")

        #Initialize invalidStand. This variable is used to determine if a stand
        #is invalid and should not be written to output argument.
        invalidStand = F

        #Skip to next stand if standDF has no nrows. This would occur if stand
        #has no live or dead records associated with it.
        if(nrow(standDF) <= 0)
        {
          noValidRecords <- noValidRecords + 1
          cat("Stand:", standSelect[j], "has no valid tree records.", "\n")
          next
        }

        #If stand only contains dead tree records for the duration of the'
        #simualtion timeframe, increment noLiveTrees and move to next iteration
        #of loop.
        if(max(standDF$TPA) <= 0)
        {
          noLiveTrees <- noLiveTrees + 1
          cat("Stand:", standSelect[j], "has no live tree records.", "\n")
          next
        }

        #Determine years that will be evaluated
        years<-unique(standDF$Year)

        #Sort years
        years<-sort(years)

        #Create list that will store output for stand j for all years
        standYrOutput<-vector(mode = "list",
                              length(years))

        #Merge species information to standDF
        standDF<-merge(standDF,
                       supportSP,
                       by = "SpeciesPLANTS",
                       all.x = T)

        #=====================================
        #Begin loop across years in standDF
        #=====================================

        for(k in 1:length(years))
        {
          #Initialize standAttr vector which stores BA, totalCC, and TPA of
          #stand/plot.
          standAttr<-c("BA" = 0, "TOTALCC" = 0, "TPA" = 0)

          #Extract rows for year k
          standYrDF<- standDF[standDF$Year == years[k],]
          cat("Year:", years[k],"\n")

          #Determine group to report in yrOutput
          group<-getGroup(toupper(standYrDF["Groups"][[1]][1]), groupTag)

          #Create dataframe that will store output for the stand in a given year.
          yrOutput<-data.frame(RUN = run,
                               GROUP = group,
                               PLOT_ID = unique(standYrDF$StandID),
                               CY = k,
                               PROJ_YEAR = years[k],
                               ST_AGE = unique(standYrDF$Age))

          #Calculate trees per acre for stand/plot.
          standAttr["TPA"]<-sum(standYrDF$TPA)

          #If the initial inventory year (k == 1) has no live trees, set
          #invalidStand to T. Stand will be processed but not sent to output.
          if(k == 1 & standAttr["TPA"] <= 0)
          {
            invalidStands <- invalidStands + 1
            invalidStand = T
          }

          #Calculate tree BA
          standYrDF$TREEBA<-standYrDF$DBH^2 * standYrDF$TPA * 0.005454

          #Calculate tree CC
          standYrDF$TREECC<-pi * (standYrDF$CrWidth/2)^2 *(standYrDF$TPA/43560) * 100

          #Calculate BA for stand/plot
          standAttr["BA"]<-sum(standYrDF$TREEBA)

          #Calculate stand percent canopy cover (uncorrected) for stand/plot.
          standCC<-sum(standYrDF$TREECC)
          yrOutput$CAN_COV<-round(standCC, 2)
          standAttr["TOTALCC"]<-yrOutput$CAN_COV

          #Calculate DomType, dcc1, dcc2, xdcc1, and xdcc2
          dtResults<-domType(standYrDF, standAttr["TOTALCC"], standAttr["TPA"])

          #Dominance type
          yrOutput$DOM_TYPE<-dtResults[["DOMTYPE"]]

          #Dominance type 1
          yrOutput$DCC1<-dtResults[["DCC1"]]

          #Dominance type 1 CC percentage
          yrOutput$XDCC1<-round(dtResults[["XDCC1"]],2)

          #Dominance type 2
          yrOutput$DCC2<-dtResults[["DCC2"]]

          #Dominance type 2 CC percentage
          yrOutput$XDCC2<-round(dtResults[["XDCC2"]],2)

          #Canopy size class - midscale mapping
          yrOutput$CAN_SIZCL<-canSizeCl(dat = standYrDF[c("DBH", "TREECC")],
                                        totalCC = standAttr["TOTALCC"],
                                        tpa = standAttr["TPA"],
                                        type = 1)

          #Canopy size class - timberland
          yrOutput$CAN_SZTMB<-canSizeCl(dat = standYrDF[c("DBH", "TREECC")],
                                        totalCC = standAttr["TOTALCC"],
                                        tpa = standAttr["TPA"],
                                        type = 2)

          #Canopy size class - woodland
          yrOutput$CAN_SZWDL<-canSizeCl(dat = standYrDF[c("DBH", "TREECC")],
                                        totalCC = standAttr["TOTALCC"],
                                        tpa = standAttr["TPA"],
                                        type = 3)

          #BA storiedness
          yrOutput$BA_STORY<-baStory(stdYrFrame = standYrDF,
                                     totalCC = standAttr["TOTALCC"],
                                     tpa = standAttr["TPA"],
                                     ba = standAttr["BA"])

          #Now correct canopy cover (CAN_COV)
          yrOutput$CAN_COV<-round(correctCC(standAttr["TOTALCC"]),2)

          #Add TPA to yrOutput
          yrOutput$TPA<-standAttr["TPA"]

          #Add BA to yrOutput
          yrOutput$BA<-standAttr["BA"]

          #Order dataframe by column position
          yrOutput<-yrOutput[,c("RUN", "GROUP",	"PLOT_ID", "CY", "PROJ_YEAR",
                                "ST_AGE",	"TPA", "BA", "CAN_COV", "DOM_TYPE",
                                "DCC1", "XDCC1", "DCC2",	"XDCC2", "CAN_SIZCL",
                                "CAN_SZTMB", "CAN_SZWDL", "BA_STORY")]

          #Add yrOutput to standYrOutput
          standYrOutput[[k]]<-yrOutput

          ### END OF LOOP ACROSS YEARS
        }

        #If stand was marked as invalid, move to next iteration of loop and do
        #not send to output.
        if(invalidStand)
        {
          cat("Stand:",
              standSelect[j],
              "is invalid and information will not be sent to output.",
              "\n")
          next
        }

        #Combine all year-by-year information for standID into a single
        #dataframe.
        standOut<-do.call("rbind", standYrOutput)

        #============================================================
        #Write stand output to output to csv (even if xlsx file is
        #requested)
        #============================================================

        #Add tab to standIDs. This avoids the problems of stand IDs being
        #converted to scientific notation in csv.
        standOut$PLOT_ID<-paste0(standOut$PLOT_ID, "\t")

        #If file doesn't exist write to file
        if(!file.exists(output))
        {
          utils::write.table(standOut,
                             output,
                             sep = ",",
                             row.names = F)
        }

        #Else append standOut to output
        else
        {
          utils::write.table(standOut,
                             output,
                             sep = ",",
                             append = T,
                             row.names = F,
                             col.names = F)
        }

        ### END OF LOOP ACROSS SELECTED STANDS
      }

      #Update standSum and send to console
      standSum<-standSum + length(standSelect)
      cat(standSum, "stands processed out of", length(stands), "\n")

      #Remove dfDat
      rm(dfDat)

      ### END OF LOOP ACROSS ALL STANDS IN RUN
    }

    #Print number of stands that had no projectable tree records.
    cat(noLiveTrees,
        "stands contained no live tree records during simulation timeframe.",
        "\n")

    #Print number of stands that had no valid tree records (live or dead records)
    cat(noValidRecords,
        "stands contained no valid tree records.",
        "\n")

    #Print number of stands that were flagged as invalid during processing.
    cat(invalidStands,
        "stands were found to be invalid for processing.",
        "\n")

    #Print run that has finished being processed
    cat(paste0(rep("*", 75), collapse = ""), "\n")
    cat("*", "Finished processing run:", run, "\n")
    cat(paste0(rep("*", 75), collapse = ""), "\n")
  }

  ### END OF LOOP ACROSS RUNS

  #Print message indicating that all runs have been processed
  cat(paste0(rep("*", 75), collapse = ""), "\n")
  cat("*", "Finished processing all runs.", "\n")
  cat(paste0(rep("*", 75), collapse = ""), "\n")

  #Disconnect from con
  RSQLite::dbDisconnect(con)
  cat("Disconnected from input database:", input, "\n")

  #Message indicating that data is being sent to output.
  # cat("Writing data to output file:", output, "\n")

  ###########################################################################
  #Logic for writing out to xlsx is below. Taking this out for now.
  ###########################################################################

  # #Determine file extension for specified output file.
  # if(fileExtOut == 'xlsx')
  # {
  #
  #   #Read in data from temporary csv file
  #   tempData<-read.csv(tempOut)
  #
  #   #Determine if file exist. If file exists and overwrite is false, then
  #   #append data to existing file.
  #   if(file.exists(output) & overwriteOut == F)
  #   {
  #     #Read in existing data from output file. Here we assume that old output
  #     #data is in worksheet 1.
  #     oldData<-openxlsx::readWorkbook(output)
  #
  #     #Combine new results with old data
  #     allOut<-rbind(oldData, tempData)
  #
  #     #Send updated results back to workbook
  #     openxlsx::write.xlsx(allOut, output, overwrite = T)
  #   }
  #
  #   #Else overwrite existing file.
  #   else
  #   {
  #     openxlsx::write.xlsx(tempData, output, overwrite = T)
  #   }
  #
  #   #Remove tempOut
  #   unlink(tempOut)
  # }

  #PRINT: Message when output file has been written
  # cat("Data written to output.","\n")

  #Return from function main
  return(cat("End of program.", "\n"))
}
