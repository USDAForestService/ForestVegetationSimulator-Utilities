################################################################################
#main.R
#
#This script contains the main function which will be used to derive
#vegetation classifications.
################################################################################

################################################################################
#Function : main.R
#
#Arguments:
#
#input:        Directory path and file name to a SQLite database (.db). Path
#              name must be surrounded with double quotes "" and double
#              back slashes or single forward slashes need to be used for
#              specifying paths. Database defined in input must contain the
#              following tables: FVS_TreeList, and FVS_Cases.
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
#overwriteOut: Logical variable used to determine if output file should be
#              overwritten. If value is TRUE, any information existing in output
#              will be overwritten with new information. If value is FALSE and
#              the file in output argument exists, then main function will stop
#              with an error message. The default value of this argument is
#              FALSE.
#
#runTitles:    Vector of character strings corresponding to FVS runTitles that
#              will be processed. If runTitles is left as NULL, execution of
#              main function will terminate. Each run title in runTitles must be
#              surrounded by double quotes. The values specified for runTitles
#              argument need to be spelled correctly. If any of the values in
#              runTitles are spelled incorrectly, the execution of main function
#              will terminate.
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
#addCompute:   Logical variable used to indicate if information in FVS_Compute
#              table should be included in output. If the FVS_Compute table does
#              not exist in input, then no variables from this table will be
#              included in output. By default, this argument is set to TRUE.
#
#addPotFire:   Logical variable used to indicate if information in FVS_PotFire
#              table should be included in output. If the FVS_PotFire table does
#              not exist in input, then no variables from this table will be
#              included in output. By default, this argument is set to TRUE.
#
#addFuels:     Logical variable used to indicate if information in FVS_Fuels
#              table should be included in output. If the FVS_Fuels table does
#              not exist in input, then no variables from this table will be
#              included in output. By default, this argument is set to TRUE.
#
#addCarbon:    Logical variable used to indicate if information in FVS_Carbon
#              table should be included in output. If the FVS_Carbon table does
#              not exist in input, then no variables from this table will be
#              included in output. By default, this argument is set to TRUE.
#
#addVolume:    Logical variable used to indicate if 3 measures of volume should
#              be calculated and reported. If the value of this argument is
#              is set to TRUE, then the following measures of volume will be
#              calculated:
#
#              VOL1: Total cubic foot volume
#              VOL2: Merchantable cubic foot volume
#              VOL3: Board foot volume
#              DEADVOL1: Total cubic foot volume that died in that cycle
#              DEADVOL2: Merchantable cubic foot volume that died in that cycle
#              DEADVOL3: Board foot volume that died in that cycle
#
#vol1DBH:	     Minimum DBH of tree records included in calculation of VOL1 and
#              DEADVOL1 when addVolume is TRUE. By default, this argument is set
#              to 0.1.
#
#vol2DBH:      Minimum DBH of tree records included in calculation of VOL2 and
#              DEADVOL2 when addVolume is TRUE. By default, this argument is set
#              to 5.
#
#vol3DBH:	     Minimum DBH of tree records included in calculation of VOL3 and
#              DEADVOL3 when addVolume is TRUE. By default, this argument is set
#              to 9.
#
#startYear:    Integer value corresponding to the year that data should start
#              being reported in output argument. Data with years prior to this
#              value will not be included in the output argument. By default
#              this value is set to 0 (all data will be included in output).
################################################################################

#'@export
main<- function(input,
                output,
                runTitles = NULL,
                allRuns = F,
                overwriteOut = F,
                addCompute = T,
                addPotFire = T,
                addFuels = T,
                addCarbon = T,
                addVolume = T,
                vol1DBH = 0.1,
                vol2DBH = 5,
                vol3DBH = 9,
                startYear = 0)
{

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

  #If runTitles is NULL and allRuns not TRUE, then stop with error message.
  if(is.null(runTitles) & !allRuns)
  {
    stop("No runs specified in runTitles argument and allRuns is not TRUE.")
  }

  #Capitalize runTitles
  runTitles<-toupper(runTitles)

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

  #Stop with error message if output file exists and overwriteOut is not FALSE.
  if(file.exists(output) & !overwriteOut)
  {
    stop(paste(output,
               "file exists and overwriteOut is FALSE. Change name of",
               "output file or set overwriteOut to TRUE.", "\n"))
  }

  #If file exists and overwriteOut is TRUE, unlink the output file.
  if(file.exists(output) & overwriteOut) unlink(output)

  #If allRuns is TRUE, extract all unique runTitles from input (con).
  if(allRuns)
  {
    runTitles<-unique(RSQLite::dbGetQuery(con, "SELECT RunTitle FROM FVS_Cases")[,1])
    runTitles<-toupper(runTitles)
  }

  #===========================================================================
  #Begin loop across FVS run titles (runTitles)
  #===========================================================================

  for(r in 1:length(runTitles))
  {
    #Extract run from runTitles
    run<-runTitles[r]

    #Print run that is being processed
    cat(paste0(rep("*", 75), collapse = ""), "\n")
    cat("*", "Processing run:", run, "\n")
    cat(paste0(rep("*", 75), collapse = ""), "\n")

    #Pull StandIds, CasesIDs and Groups for the current run
    dbQuery<- caseQuery(run)
    cases<-RSQLite::dbGetQuery(con, dbQuery)

    cat("\n")
    cat("Columns read from cases table:", colnames(cases), "\n")

    cat("Total number of stands to process for run", paste0(run,":"),
        length(cases[["StandID"]]),"\n", "\n")

    #Initialize standSum. This will keep track of number of total stands
    #processed.
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
    #invalid when the inventory is comprised entirely of dead trees in first
    #cycle.
    invalidStands <- 0

    #===========================================================================
    #Begin loop across case IDs for run title (cases[["CaseID"]])
    #===========================================================================

    for(i in 1:length(cases[["CaseID"]]))
    {
      #Select stands to process
      caseID<-cases[["CaseID"]][i]

      #Find location of caseID in cases
      caseIndex <- match(caseID,
                         cases[["CaseID"]])

      #If caseIndex is NA (shouldn't ever be the case), move to next
      #iteration of the loop.
      if(is.na(caseIndex)) next

      #Define groups and standID for current case ID
      else
      {
        groups  <- cases$Groups[caseIndex]
        standID <- cases$StandID[caseIndex]
      }

      #Display which stand and case ID is being processed
      cat("Processing stand:", standID, "CaseID:", caseID, "\n")

      #Generate a query that will be used to read data from FVS tree list
      dbQuery<-treeQuery(caseID)

      cat("Querying tree list...", "\n")

      #Execute SQL query to obtain list of stand data
      standDF<-RSQLite::dbGetQuery(con,
                                 dbQuery)

      cat("Tree list query complete.", "\n")

      #Display column names and number of rows in standDF
      cat("Columns read from tree list:", colnames(standDF), "\n")
      cat("Number of rows read from tree list:", nrow(standDF), "\n")

      #If query yields no results, skip to next iteration of loop across
      #standList
      if(nrow(standDF) <= 0)
      {
        cat("No valid tree records found in tree query.", "\n")
        next
      }

      #Initialize invalidStand. This variable is used to determine if a stand
      #is invalid and should not be written to output argument.
      invalidStand = F

      #Skip to next stand if standDF has no nrows. This would occur if stand
      #has no live or dead records associated with it.
      if(nrow(standDF) <= 0)
      {
        noValidRecords <- noValidRecords + 1
        cat("Stand:", standID, "has no valid tree records.", "\n")
        standSum<-standSum + 1
        next
      }

      #If stand only contains dead tree records for the duration of the
      #simulation timeframe, increment noLiveTrees and move to next iteration
      #of loop.
      if(max(standDF$TPA) <= 0)
      {
        noLiveTrees <- noLiveTrees + 1
        cat("Stand:", standID, "has no live tree records.", "\n")
        standSum<-standSum + 1
        next
      }

      #Determine years that will be evaluated
      years<-unique(standDF$Year)

      #Sort years
      years<-sort(years)

      #Create list that will store output for stand j for all years
      standYrOutput<-vector(mode = "list",
                            length(years))

      #Print message indicating that vegClass attributes are being calculated
      #for stand
      cat("\n")
      cat("Vegetation classification attributes being calculated for stand:",
          standID, "\n")

      #=========================================================================
      #Begin loop across years in standDF
      #=========================================================================

      for(j in 1:length(years))
      {

        #If this is the last year to process and addCompute/addPotFire is T,
        #move to next iteration of loop. This is done, since FVS_Compute and
        #FVS_PotFire table report one less cycle than FVS_Treelist.
        if(j == length(years) & (addCompute | addPotFire | addCarbon |
                                 addFuels))
        {
          cat("Skipping last year:", years[j], "\n")
          next
        }

        #Extract rows for year j
        standYrDF<- standDF[standDF$Year == years[j],]
        cat("Year:", years[j],"\n")

        #If the initial inventory year (j == 1) has no live trees, set
        #invalidStand to T and break out of loop.
        if(j == 1 & max(standYrDF$TPA) <= 0)
        {
          invalidStands <- invalidStands + 1
          invalidStand = T
          break
        }

        #If year j is less than startYear, skip to next iteration of loop
        if(years[j] < startYear)
        {
          cat("Year:", years[j], "is before start year:", startYear,
              "and will not be processed.", "\n")
          rm(standYrDF)
          next
        }

        #Create dataframe that will store output for the stand in a given year.
        yrOutput<-data.frame(RUNTITLE = run,
                             CASEID = caseID,
                             STANDID = standID,
                             YEAR = years[j])

        #Bind data from vegOut to yrOutput
        yrOutput <- cbind(yrOutput,
                          vegOut(standYrDF))

        #If addVolume is TRUE, calculate volumes and add to yrOutput
        if(addVolume)
        {
          volume <- volumeCalc(standYrDF,
                               vol1DBH = vol1DBH,
                               vol2DBH = vol2DBH,
                               vol3DBH = vol3DBH)

          yrOutput$VOL1 <- volume["VOL1"]
          yrOutput$VOL2 <- volume["VOL2"]
          yrOutput$VOL3 <- volume["VOL3"]

          deadVolume <- volumeCalc(standYrDF,
                                   expf = "MortPA",
                                   vol1DBH = vol1DBH,
                                   vol2DBH = vol2DBH,
                                   vol3DBH = vol3DBH)

          yrOutput$DEADVOL1 <- deadVolume["VOL1"]
          yrOutput$DEADVOL2 <- deadVolume["VOL2"]
          yrOutput$DEADVOL3 <- deadVolume["VOL3"]
        }

        #Add yrOutput to standYrOutput
        standYrOutput[[j]]<-yrOutput

        #remove standYrDF and yrOutput
        rm(standYrDF, yrOutput)

        ### END OF LOOP ACROSS YEARS
      }

      #If stand was marked as invalid, move to next iteration of loop and do
      #not send to output.
      if(invalidStand)
      {
        cat("Stand:",
            standID,
            "is invalid and information will not be sent to output.",
            "\n")
        standSum<-standSum + 1
        next
      }

      #Combine all year-by-year information for standID into a single
      #dataframe.
      standOut<-do.call("rbind", standYrOutput)

      #Remove standYrOutput
      rm(standYrOutput)

      #Create cycle field now
      standOut$CY <- seq(from = 1, to = nrow(standOut), by = 1)

      #Add tab to standIDs. This avoids the problems of stand IDs being
      #converted to scientific notation in csv. There may be a better way to
      #deal with this but not quite sure what it is.
      standOut$STANDID<-paste0(standOut$STANDID, "\t")

      #Join compute variables to output if addCompute is TRUE and FVS_Compute
      #table exists in input
      if(addCompute)
      {
        #Test if FVS_Compute table exists. Read in information from compute table
        #along with caseIDs.
        if(RSQLite::dbExistsTable(con,
                                  "FVS_COMPUTE"))
        {
          #Generate compute query
          dbQuery <- computeQuery(caseID)

          #Print compute query message
          cat("\n")
          cat("Querying FVS_Compute...", "\n")

          #Get the data from FVS_Compute
          computeDF <- RSQLite::dbGetQuery(con,
                                           dbQuery)

          cat("FVS_Compute query complete.", "\n")
          cat("Number of rows read from compute query",
              nrow(computeDF),
              "\n")

          #If computeDF has data, merge it to standOut
          if(nrow(computeDF) > 0)
          {

            cat("Merging FVS_Compute variables to stand:",
                standID,
                "\n")

            #Capitalize column names
            colnames(computeDF) <- toupper(colnames(computeDF))

            #Remove STANDID from computeDF
            computeDF$STANDID <- NULL

            #Merge to standOut
            standOut <- merge(standOut,
                              computeDF,
                              by = c("CASEID", "YEAR"),
                              all.x = T)

            cat("Merging of FVS_Compute variables to stand:",
                standID,
                "is complete.",
                "\n")

            #Remove computeDF
            rm(computeDF)

          }

          else
          {
            cat("No data found in FVS_Compute query for stand:",
                standID,
                "\n")
          }

        }

        #Report that FVS_Compute table was not found in input.
        else
        {
          cat("\n")
          cat("FVS_Compute table not found in input. No information to join.",
              "\n")
        }
      }

      #Join FVS_PotFire variables to output if addPotFire is TRUE and
      #FVS_PotFire table exists in input.
      if(addPotFire)
      {
        #Test if FVS_PotFire table exists. Read in information from
        #FVS_PotFire table.
        if(RSQLite::dbExistsTable(con,
                                  "FVS_POTFIRE"))
        {
          #Generate PotFire query
          dbQuery <- potFireQuery(caseID)

          #Print FVS_PotFire query message
          cat("\n")
          cat("Querying FVS_PotFire...", "\n")

          #Get the data from FVS_PotFire
          potFireDF <- RSQLite::dbGetQuery(con,
                                           dbQuery)

          cat("FVS_PotFire query complete.", "\n")
          cat("Number of rows read from FVS_PotFire query",
              nrow(potFireDF),
              "\n")

          #If potFireDF has data, merge it to standOut
          if(nrow(potFireDF) > 0)
          {

            cat("Merging FVS_PotFire variables to stand:",
                standID,
                "\n")

            #Capitalize column names
            colnames(potFireDF) <- toupper(colnames(potFireDF))

            #Remove STANDID from computeDF
            potFireDF$STANDID <- NULL

            #Merge to standOut
            standOut <- merge(standOut,
                              potFireDF,
                              by = c("CASEID", "YEAR"),
                              all.x = T)

            cat("Merging of FVS_PotFire variables to stand:",
                standID,
                "is complete.",
                "\n")

            #Remove potFireDF
            rm(potFireDF)

          }

          else
          {
            cat("No data found in FVS_PotFire query for stand:",
                standID,
                "\n")
          }

        }

        #Report that FVS_PotFire table was not found in input.
        else
        {
          cat("\n")
          cat("FVS_PotFire table not found in input. No information to join.",
              "\n")
        }
      }

      #Join FVS_Fuels variables to output if addFuels is TRUE and
      #FVS_Fuels table exists in input.
      if(addFuels)
      {
        #Test if FVS_Fuels table exists. Read in information from
        #FVS_Fuels table.
        if(RSQLite::dbExistsTable(con,
                                  "FVS_FUELS"))
        {
          #Generate fuels query
          dbQuery <- fuelsQuery(caseID)

          #Print FVS_Fuels query message
          cat("\n")
          cat("Querying FVS_Fuels...", "\n")

          #Get the data from FVS_Fuels
          fuelsDF <- RSQLite::dbGetQuery(con,
                                           dbQuery)

          cat("FVS_Fuels query complete.", "\n")
          cat("Number of rows read from FVS_Fuels query",
              nrow(fuelsDF),
              "\n")

          #If fuelsDF has data, merge it to standOut
          if(nrow(fuelsDF) > 0)
          {

            cat("Merging FVS_Fuels variables to stand:",
                standID,
                "\n")

            #Capitalize column names
            colnames(fuelsDF) <- toupper(colnames(fuelsDF))

            #Remove STANDID from fuelsDF
            fuelsDF$STANDID <- NULL

            #Merge to standOut
            standOut <- merge(standOut,
                              fuelsDF,
                              by = c("CASEID", "YEAR"),
                              all.x = T)

            cat("Merging of FVS_Fuels variables to stand:",
                standID,
                "is complete.",
                "\n")

            #Remove fuelsDF
            rm(fuelsDF)

          }

          else
          {
            cat("No data found in FVS_Fuels query for stand:",
                standID,
                "\n")
          }

        }

        #Report that FVS_Fuels table was not found in input.
        else
        {
          cat("\n")
          cat("FVS_Fuels table not found in input. No information to join.",
              "\n")
        }
      }

      #Join FVS_Carbon variables to output if addCarbon is TRUE and FVS_Carbon
      #table exists in input.
      if(addCarbon)
      {
        #Test if FVS_Carbon table exists. Read in information from FVS_Carbon
        #table.
        if(RSQLite::dbExistsTable(con,
                                  "FVS_CARBON"))
        {
          #Generate carbon query
          dbQuery <- carbonQuery(caseID)

          #Print FVS_Carbon query message
          cat("\n")
          cat("Querying FVS_Carbon...", "\n")

          #Get the data from FVS_Carbon
          carbonDF <- RSQLite::dbGetQuery(con,
                                         dbQuery)

          cat("FVS_Carbon query complete.", "\n")
          cat("Number of rows read from FVS_Carbon query",
              nrow(carbonDF),
              "\n")

          #If carbonDF has data, merge it to standOut
          if(nrow(carbonDF) > 0)
          {

            cat("Merging FVS_Carbon variables to stand:",
                standID,
                "\n")

            #Capitalize column names
            colnames(carbonDF) <- toupper(colnames(carbonDF))

            #Remove STANDID from carbonDF
            carbonDF$STANDID <- NULL

            #Merge to standOut
            standOut <- merge(standOut,
                              carbonDF,
                              by = c("CASEID", "YEAR"),
                              all.x = T)

            cat("Merging of FVS_Carbon variables to stand:",
                standID,
                "is complete.",
                "\n")

            #Remove carbonDF
            rm(carbonDF)

          }

          else
          {
            cat("No data found in FVS_Carbon query for stand:",
                standID,
                "\n")
          }

        }

        #Report that FVS_Carbon table was not found in input.
        else
        {
          cat("\n")
          cat("FVS_Carbon table not found in input. No information to join.",
              "\n")
        }
      }

      #Rearrange column headers in standOut so RUNTITLE, GROUP, CASEID,
      #STANDID, YEAR, CY are reported in the correct order
      leadingCols <- c("RUNTITLE", "CASEID", "STANDID", "YEAR", "CY")

      #Get column names from standOut
      colNames <- colnames(standOut)

      #Remove leading columns from colNames
      colNames <- colNames[!colNames %in% leadingCols]

      #Append leading columns to cols
      colNames <- c(leadingCols, colNames)

      #Rearrange the column headers in standOut
      standOut <- standOut[, c(colNames)]

      cat("\n")
      cat("Columns in standOut:", "\n", colnames(standOut), "\n", "\n")

      #============================================================
      #Write stand output to CSV specified in output argument
      #============================================================

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

      #Update standSum and send to console
      standSum<-standSum + 1
      cat(standSum, "stands processed out of", nrow(cases), "\n", "\n")

      #Remove standDF and standOut
      rm(standDF, standOut)

      ### END OF LOOP ACROSS ALL CASEIDS IN RUN
    }

    #Print run that has finished being processed
    cat(paste0(rep("*", 75), collapse = ""), "\n")
    cat("*", "Finished processing run:", run, "\n")
    cat(paste0(rep("*", 75), collapse = ""), "\n", "\n")

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
        "\n", "\n")
  }

  ### END OF LOOP ACROSS RUNS

  #Print message indicating that all runs have been processed
  cat(paste0(rep("*", 75), collapse = ""), "\n")
  cat("*", "Finished processing all runs.", "\n")
  cat(paste0(rep("*", 75), collapse = ""), "\n")

  #Disconnect from con
  RSQLite::dbDisconnect(con)
  cat("Disconnected from input database:", input, "\n")

  #Return from function main
  return(cat("End of program.", "\n"))
}
