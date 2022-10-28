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
#
#addCompute:   Boolean variable used to indicate if information in FVS_Compute
#              table should be included in output. By default this argument
#              is set to TRUE. If the FVS_Compute table does not exist in input
#              then only the variables calculated by the vegClass package will
#              be returned in output.
#
#startYear:    Integer value corresponding to the year that data should start
#              being reported in output argment. Data with years prior to this
#              value will not be included in the output argument. By default
#              this value is set to 0 (all data will be included in output).
################################################################################

#'@export
main<- function(input,
                output,
                overwriteOut = F,
                groupTag = "---",
                runTitles = "Run 1",
                allRuns = F,
                numToQuery = 50,
                addCompute = T,
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

    #Pull StandIds, CasesIDs and Groups for the current run
    dbQuery<- caseQuery(run)
    cases<-RSQLite::dbGetQuery(con, dbQuery)

    cat("Columns read from cases table:", colnames(cases), "\n")

    cat("Total number of stands to process for run", paste0(run,":"),
        length(cases[["StandID"]]),"\n")

    #Split stand vector into list containing vectors of stand IDs
    caseList<-split(cases[["CaseID"]],
                    ceiling(seq_along(cases[["CaseID"]])/numToQuery))

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

    for(i in 1:length(caseList))
    {
      #Select stands to process
      caseSelect<-caseList[[i]]
      cat("Number of stands to query:", length(caseSelect), "\n")

      #Generate a query that will be used to read data from FVS tree list
      dbQuery<-treeQuery(caseSelect)

      cat("Querying tree list...", "\n")

      #Execute SQL query to obtain list of stand data
      treeData<-RSQLite::dbGetQuery(con,
                                 dbQuery)

      cat("Tree list query complete.", "\n")

      #Capitalize column headers of treeData
      cat("Columns read from tree list:", colnames(treeData), "\n")
      cat("Number of rows read from tree list:", nrow(treeData), "\n")

      #If query yields no results, skip to next iteration of loop across
      #standList
      if(nrow(treeData) <= 0)
      {
        cat("No valid tree records found in tree query.", "\n")
        next
      }

      #==============================
      #Begin loop across caseSelect
      #==============================

      for(j in 1:length(caseSelect))
      {
        #Locate case j in cases
        caseIndex <- match(caseSelect[j],
                            cases[["CaseID"]])

        #If caseIndex is NA (shouldn't ever be the case), move to next
        #iteration of the loop.
        if(is.na(caseIndex)) next

        #Define caseID, groups, and standID for current case
        else
        {
          caseID  <- cases$CaseID[caseIndex]
          groups  <- cases$Groups[caseIndex]
          standID <- cases$StandID[caseIndex]
        }

        #Extract stand from treeData
        standDF<-treeData[treeData$CaseID %in% caseSelect[j],]
        cat("Processing stand:", standID, "CaseID:", caseID, "\n")

        #Initialize invalidStand. This variable is used to determine if a stand
        #is invalid and should not be written to output argument.
        invalidStand = F

        #Skip to next stand if standDF has no nrows. This would occur if stand
        #has no live or dead records associated with it.
        if(nrow(standDF) <= 0)
        {
          noValidRecords <- noValidRecords + 1
          cat("Stand:", standID, "has no valid tree records.", "\n")
          next
        }

        #If stand only contains dead tree records for the duration of the'
        #simualtion timeframe, increment noLiveTrees and move to next iteration
        #of loop.
        if(max(standDF$TPA) <= 0)
        {
          noLiveTrees <- noLiveTrees + 1
          cat("Stand:", standID, "has no live tree records.", "\n")
          next
        }

        #Determine years that will be evaluated
        years<-unique(standDF$Year)

        #Sort years
        years<-sort(years)

        #Extract all years greater than startYear
        years <- years[years >= startYear]

        #Create list that will store output for stand j for all years
        standYrOutput<-vector(mode = "list",
                              length(years))

        #=====================================
        #Begin loop across years in standDF
        #=====================================

        for(k in 1:length(years))
        {
          #If this is the last year to process and addCompute is T, move to
          #next iteration of loop. This is done, since FVS_Compute table reports
          #one less cycle than FVS_Treelist.
          if(k == length(years) & addCompute)
          {
            cat("Skipping last year:", years[k], "\n")
            next
          }

          #Extract rows for year k
          standYrDF<- standDF[standDF$Year == years[k],]
          cat("Year:", years[k],"\n")

          #Determine group to report in yrOutput
          group<-getGroup(toupper(groups), groupTag)

          #Create dataframe that will store output for the stand in a given year.
          yrOutput<-data.frame(RUNTITLE = run,
                               GROUP = group,
                               CASEID = caseID,
                               STANDID = standID,
                               YEAR = years[k],
                               CY = k)

          #If the initial inventory year (k == 1) has no live trees, set
          #invalidStand to T. Stand will be processed but not sent to output.
          if(k == 1 & plotTPA(standYrDF) <= 0)
          {
            invalidStands <- invalidStands + 1
            invalidStand = T
          }

          #Calculate canopy cover uncorrected for overlap (CAN_COV)
          yrOutput$CAN_COV<-round(plotCC(standYrDF, type = 2),2)

          #Calculate BA, TPA, QMD, and SDI for seedlings + stems
          yrOutput$BA <- plotBA(standYrDF)
          yrOutput$TPA <- plotTPA(standYrDF)
          yrOutput$QMD <- plotQMD(standYrDF)
          yrOutput$ZSDI <- plotSDI(standYrDF, type = 1)
          yrOutput$RSDI <- plotSDI(standYrDF, type = 2)

          #Calculate BA, TPA, QMD, and SDI for stems only
          yrOutput$BA_STM <- plotBA(standYrDF,
                                    min = 1)

          yrOutput$TPA_STM <- plotTPA(standYrDF,
                                      min = 1)

          yrOutput$QMD_STM <- plotQMD(standYrDF,
                                      min = 1)

          yrOutput$ZSDI_STM <- plotSDI(standYrDF,
                                       type = 1,
                                       min = 1)

          yrOutput$RSDI_STM <- plotSDI(standYrDF,
                                       type = 2,
                                       min = 1)

          #Calculate QMD_TOP20
          yrOutput$QMD_TOP20 <- qmdTop20(standYrDF)

          #Calculate DomType, dcc1, dcc2, xdcc1, and xdcc2
          dtResults<-domType(data = standYrDF)

          #Dominance type
          yrOutput$DOM_TYPE<-dtResults[["DOMTYPE"]]

          #Dominance type 1
          yrOutput$DCC1<-dtResults[["DCC1"]]

          #Dominance type 1 CC
          yrOutput$XDCC1<-round(dtResults[["XDCC1"]],2)

          #Dominance type 2
          yrOutput$DCC2<-dtResults[["DCC2"]]

          #Dominance type 2 CC
          yrOutput$XDCC2<-round(dtResults[["XDCC2"]],2)

          #Canopy size class - midscale mapping
          yrOutput$CAN_SIZCL<-canSizeCl(data = standYrDF,
                                        type = 1)

          #Canopy size class - timberland
          yrOutput$CAN_SZTMB<-canSizeCl(data = standYrDF,
                                        type = 2)

          #Canopy size class - woodland
          yrOutput$CAN_SZWDL<-canSizeCl(data = standYrDF,
                                        type = 3)

          #BA storiedness
          yrOutput$BA_STORY<-baStory(standYrDF)

          #Add yrOutput to standYrOutput
          standYrOutput[[k]]<-yrOutput

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
          next
        }

        #Combine all year-by-year information for standID into a single
        #dataframe.
        standOut<-do.call("rbind", standYrOutput)

        #Add tab to standIDs. This avoids the problems of stand IDs being
        #converted to scientific notation in csv. There may be a better way to
        #deal with this but not quite sure what it is.
        standOut$STANDID<-paste0(standOut$STANDID, "\t")

        #Join compute variables to output if addCompute is TRUE and FVS_Compute
        #table exists in input
        if(addCompute)
        {
          #Test if FVS_Compute table exists. Read in information from compute table
          #along with caseIDs for FVS runs specified in runTitles.
          if(RSQLite::dbExistsTable(con,
                                    "FVS_COMPUTE"))
          {
            #Generate compute query
            dbQuery <- computeQuery(caseID)

            #Print compute query message
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

              cat("Merging compute variables to stand:",
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

              cat("Merging of compute variables to stand:",
                  standID,
                  "is complete.",
                  "\n")

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
            cat("FVS_Compute table not found in input. No information to join.",
                "\n")
          }
        }

        #Rearrange column headers in standOut so RUNTITLE, GROUP, CASEID,
        #STANDID, YEAR, CY are reported in the correct order
        leadingCols <- c("RUNTITLE", "GROUP", "CASEID", "STANDID", "YEAR", "CY")

        #ColNames
        colNames <- colnames(standOut)

        #Remove leading columns from colNames
        colNames <- colNames[!colNames %in% leadingCols]

        #Append leading columns to cols
        colNames <- c(leadingCols, colNames)

        #Rearrange the column headers in standOut
        standOut <- standOut[, c(colNames)]

        cat("Columns in standOut:", "\n", colnames(standOut), "\n", "\n")

        #============================================================
        #Write stand output to temporary CSV
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

        ### END OF LOOP ACROSS SELECTED STANDS
      }

      #Update standSum and send to console
      standSum<-standSum + length(caseSelect)
      cat(standSum, "stands processed out of", nrow(cases), "\n")

      #Remove treeData
      rm(treeData)

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

  #Return from function main
  return(cat("End of program.", "\n"))
}
