################################################################################
#main.R
#
#This script contains the main function which is used to derive vegetation
#classifications and other attributes.
################################################################################

################################################################################
#Function : main.R
#
#Arguments:
#
#input:        Directory path and file name to a SQLite database (.db). Path
#              name must be surrounded with double quotes "" and double
#              back slashes or single forward slashes need to be used for
#              specifying paths. By default, this argument is set to NULL.
#
#              Examples of valid input formats:
#              "C:/FVS/R3_Work/FVSOut.db"
#              "C:\\FVS\\R3_Work\\FVSOut.db"
#
#              Database defined in input should contain FVS_Treelist (western
#              variants) or FVS_Treelist_East (eastern variants) and FVS_Cases
#              table. If the FVS_Treelist or FVS_Treelist table does exist in
#              input argument but there is no information in either of these
#              tables for a given run, then no information for that run will be
#              sent to output argument.
#
#output:       Directory path and filename to a .csv file. Path name must be
#              surrounded with double quotes "" and double back slashes or
#              single forward slashes need to be used for specifying paths. By
#              default, this argument is set to NULL.
#
#              Examples of valid output formats:
#              "C:/FVS/R3_Work/FVSOut.csv"
#              "C:\\FVS\\R3_Work\\FVSOut.csv"
#
#overwriteOut: Logical variable used to determine if output file should be
#              overwritten. If value is TRUE, any information existing in output
#              will be overwritten with new information. If value is FALSE (F)
#              and the file in output argument exists, then main function will
#              stop with an error message. The default value of this argument is
#              FALSE (F).
#
#region:       Integer variable corresponding to USFS region number. Valid
#              values are 1, 2, 3, 4, 5, 6, 8, 9, or 10. This argument is
#              currently a dummy variable and has no effect on underlying logic.
#              In the future this variable will be likely used to determine rule
#              sets for calculating vegetation classifications and other
#              attributes. The value specified in this argument will be included
#              in the file specified in the output argument.
#
#              WARNING: the value specified in the region argument will apply
#              to all runs specified in the runTitles argument or all runs being
#              processed if the allRuns argument is set to TRUE (T). As such,
#              run(s) from only one region at a time when using the main
#              function.
#
#runTitles:    Vector of character strings corresponding to FVS runTitles that
#              will be processed. If runTitles is left as NULL and allRuns is
#              FALSE (F), execution of main function will stop with an error
#              message. Each run title in runTitles must be surrounded by double
#              quotes. The values specified for runTitles argument need to be
#              spelled correctly. If any of the values in runTitles are spelled
#              incorrectly, the execution of main function will stop with an
#              error message.
#
#              Example of how to specify single run title:
#              runTitles = "Run 1"
#
#              Example of how to specify multiple run titles:
#              runTitles = c("Run 1", "Run 2",...)
#              Note the use of the c(...) when processing multiple runs.
#
#allRuns:      Logical variable that is used to determine if all runs in
#              argument input should be processed. If value is TRUE (T), then
#              all runs will be processed and any runs specified in argument
#              runTitles will be ignored. By default, this argument is set
#              to FALSE (F).
#
#region:	     Integer variable corresponding to USFS region number. Valid
#              values are 1, 2, 3, 4, 5, 6, 8, 9, or 10. This argument is
#              currently a dummy variable and has no effect on output produced
#              from main function. In the future this variable will be likely
#              used to determine rule sets for calculating vegetation
#              classifications and other attributes. The value specified in this
#              argument will be included in the file specified in the output
#              argument.
#
#              WARNING: the value specified in the region argument will apply to
#              all runs specified in the runTitles argument or all runs being
#              processed if the allRuns argument is set to TRUE (T). As such,
#              run(s) from only one region at a time when using the main
#              function.
#
#addCompute:   Logical variable used to indicate if information in FVS_Compute
#              table should be included in output argument. If the FVS_Compute
#              table does not exist in input argument and addCompute is TRUE
#              (T), main function will stop with an error message. If the
#              FVS_Compute table does exist in input argument but there is no
#              information in the FVS_Compute table for a given run, then that
#              run will have NA values reported in output argument for all
#              variable found in the FVS_Compute table. By default, this
#              argument is set to FALSE (F).
#
#addPotFire:   Logical variable used to indicate if information in FVS_Potfire
#              or FVS_Potfire_East table should be included in output argument.
#              If the FVS_Potfire table does not exist in input argument and
#              addPotFire is TRUE (T), main function will  stop with an error
#              message. If the FVS_Potfire or FVS_Potfire_East table do exist in
#              input argument but there is no information in the FVS_Potfire or
#              FVS_Potfire_East table for a given run, then that run will have
#              NA values reported in output for all variables extracted from the
#              FVS_Potfire/FVS_Potfire_East table. By default, this argument is
#              set to FALSE (F).
#
#addFuels:     Logical variable used to indicate if information in FVS_Fuels
#              table should be included in output argument. If the FVS_Fuels
#              table does not exist in input argument and addFuels is TRUE (T),
#              main function will stop with an error message. If the FVS_Fuels
#              table does exist in input argument but there is no information in
#              the FVS_Fuels table for a given run, then that run will have NA
#              values reported in output argument for all variables found in the
#              FVS_Fuels table. By default, this argument is set to FALSE (F).
#
#addCarbon:    Logical variable used to indicate if information in FVS_Carbon
#              table should be included in output argument. If the FVS_Carbon
#              table does not exist in input argument and addCarbon is TRUE (T),
#              main function will stop with an error message. If the FVS_Carbon
#              table does exist in input argument but there is no information in
#              the FVS_Carbon table for a given run, then that run will have NA
#              values reported in output argument for all variables found in the
#              FVS_Carbon table. By default, this argument is set to FALSE (F).
#
#addVolume:    Logical variable used to indicate if 3 measures of volume should
#              be calculated and reported. If the value of this argument is
#              is set to TRUE (T), then the following measures of volume will be
#              calculated:
#
#              Eastern variants: CS, LS, NE and SN
#              VOL1: Merchantable cubic foot volume
#              VOL2: Sawlog cubic foot volume
#              VOL3: Sawlog Board foot volume
#              DEADVOL1: Merchantable cubic foot volume that died in that cycle
#              DEADVOL2: Sawlog cubic foot volume that died in that cycle
#              DEADVOL3: Sawlog Board foot volume that died in that cycle
#
#              Western variants
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
#              value will not be included in the output argument. By default,
#              this value is set to 0 (all data will be included in output).
#
#setIndices:   Logical variable, where if TRUE, CaseID indices will be created
#              in FVS database tables found in input argument. These indices
#              significantly increase the speed of SQL queries executed in the
#              main function. This argument DOES NOT need to be set to TRUE when
#              users are processing an output database produced by the FVS
#              graphical user interface (local or online configuration of FVS).
#              This function should only be invoked when a user produces an
#              output database through rFVS or FVS run through the command line.
#              By default, this argument is set to FALSE (F).
#
#modstandID:   Logical variable, where if TRUE, an underscore will be appended
#              before each Stand ID sent to output argument. The addition of
#              the underscore forces Microsoft Excel to recognize that the Stand
#              ID is a character and avoids the problem of long character
#              strings of numbers (i.e. Stand IDs in FIA data:
#              0004201904090101990050) being truncated and converted to numbers.
#              By default, this argument is set to TRUE (T).
#
#Value
#
#0 value invisibly returned.
################################################################################

#'@export
main<- function(input = NULL,
                output = NULL,
                runTitles = NULL,
                allRuns = F,
                overwriteOut = F,
                region = NA,
                addCompute = F,
                addPotFire = F,
                addFuels = F,
                addCarbon = F,
                addVolume = F,
                vol1DBH = 0,
                vol2DBH = 5,
                vol3DBH = 9,
                startYear = 0,
                setIndices = F,
                modStandID = T)
{
  #Set the start time of function execution
  startTime <- Sys.time()

  ###########################################################################
  #Check function arguments
  ###########################################################################

  #==========================================================================
  #Do checks on input argument
  #==========================================================================

  #Test if value in input argument is null.
  if (is.null(input)){
    stop(paste("No database specified in input argument."))
  }

  #Change \\ to / in input argument
  input <- gsub("\\\\", "/", input)

  #Test existence of input database.
  if (!(file.exists(input))){
    stop(paste("Input database not found. Make sure directory path and file",
               "name in input are spelled correctly."))
  }

  #Extract file extension for input argument.
  fileExtIn<-sub("(.*)\\.","",input)

  #Make sure input database is SQLite (.db).
  if (!fileExtIn %in% "db"){
    stop(paste("Input argument does not have a valid file extension. File",
               "extension must be .db."))
  }

  #==========================================================================
  #Do checks on output argument
  #==========================================================================

  #Test if value in output argument is null.
  if (is.null(output)){
    stop(paste("No file specified in output argument."))
  }

  #Change \\ to / in output argument
  output <- gsub("\\\\", "/", output)

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
    stop(paste("Output argument does not have a valid file extension. File",
               "extension must be .csv."))
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

  #==========================================================================
  #Do checks on runTitles argument
  #==========================================================================

  #If runTitles is NULL and allRuns not TRUE, then stop with error message.
  if(is.null(runTitles) & !allRuns)
  {
    stop("No runs specified in runTitles argument and allRuns is not TRUE.")
  }

  #Capitalize runTitles if not null
  if(!is.null(runTitles)) runTitles <- toupper(runTitles)

  #==========================================================================
  #Do checks on region argument
  #==========================================================================

  #If region is not a valid value, then stop with error message
  if(! region %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10))
  {
    stop(paste("Invalid region number was specified in region argument.",
               "Please enter a value of 1, 2, 3, 4, 5, 6, 8, 9, or 10"))
  }

  ###########################################################################
  #Perform checks on input database (con)
  ###########################################################################

  #Connect to input database
  con<-RSQLite::dbConnect(RSQLite::SQLite(), input)
  cat("Connected to input database:", input, "\n")

  #==========================================================================
  #Check if FVS_Cases table is in input (con). If it is not, disconnect from
  #con and stop with an error.
  #==========================================================================

  if(is.element(F, c("FVS_Cases") %in%
                RSQLite::dbListTables(con)))
  {
    RSQLite::dbDisconnect(con)
    stop(paste("FVS_Cases table not found in input database."))
  }

  #==========================================================================
  #If allRuns is not TRUE, check if individual runs specified in runTitles
  #exist. If any are missing, disconnect from con and stop with an error
  #message.
  #==========================================================================

  if(!allRuns)
  {
    #Check if runs from runTitles are found in FVS_Cases table
    runsFound<- runTitles %in% toupper(
      RSQLite::dbGetQuery(con,
                          "SELECT DISTINCT RunTitle FROM FVS_Cases")[,1])

    #If any runs are not found, then report them in error message
    if(F %in% runsFound)
    {
      #Determine missing runs
      missingRuns<-runTitles[runsFound == F]

      #Paste missing runs together separated by comma and space
      missingRuns<-paste(missingRuns, collapse = ", ")

      RSQLite::dbDisconnect(con)

      stop(paste("Run titles:",paste0("'",missingRuns,"'", collapse = ""),
                 "not found in input database. Please ensure all run",
                 "titles are spelled correctly."))
    }
  }

  #==========================================================================
  #Check if the following tables exist in input database:
  #FVS_TreeList, FVS_TreeList_East
  #FVS_Compute (if addCompute is TRUE)
  #FVS_PotFire, FVS_PotFire_East (if addPotFire is TRUE)
  #FVS_Fuels (if addFuels is TRUE)
  #FVS_Carbon (if addCarbon is TRUE)
  #If setIndices is TRUE, check if input database has indexes that are used
  #by main function.
  #==========================================================================

  #Grab all distinct runs and variants from FVS_Cases
  runs <- RSQLite::dbGetQuery(con,
                              paste("SELECT DISTINCT FVS_Cases.RunTitle,",
                                    "FVS_Cases.Variant",
                                    "FROM FVS_Cases"))

  #Capitalize column names
  colnames(runs) <- toupper(colnames(runs))

  #Capitalize run titles
  runs$RUNTITLE <- toupper(runs$RUNTITLE)

  #If allRuns is not TRUE, select runs specified in runTitles argument.
  if(!allRuns)
  {
    runs <- runs[runs$RUNTITLE %in% runTitles, ]
  }

  #Check if input tables exist using checkDBTables function
  message<-checkDBTables(con,
                         variants = runs$VARIANT,
                         addCompute = addCompute,
                         addPotFire = addPotFire,
                         addFuels = addFuels,
                         addCarbon = addCarbon,
                         setIndices = setIndices)

  #If checkDBTables function returns a message that is not 'PASS' then
  #disconnect from con and stop with error message.
  if(message != 'PASS')
  {
    #Disconnect from con
    RSQLite::dbDisconnect(con)

    #Print error message
    stop(message)
  }

  #Reset runTitles to what is in RUNTITLES column of runs data frame.
  runTitles <- runs$RUNTITLE

  #===========================================================================
  #If setIndices is true, then create CaseID indices for each FVS table in
  #input.
  #===========================================================================

  if(setIndices)
  {
    cat("Setting Case ID indices in:", input,
        "\n",
        "\n")

    setCaseIndices(con)

    cat("Case ID indices set in:", input,
        "\n",
        "\n")
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
    #stand/plot has a record in the STANDINIT/PLOTINIT table but no records in
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
        variant <- cases$Variant[caseIndex]

        #If modStandID is TRUE, append an underscore in front of STANDID.
        #This will force Excel to recognize that STANDID is text when opening a
        #CSV.
        if(modStandID)
        {
          standID <- paste0("_", standID)
        }
      }

      #Display which stand and case ID is being processed
      cat("Processing stand:", standID, "CaseID:", caseID, "\n")

      #Generate a query that will be used to read data from FVS tree list
      dbQuery<-treeQuery(caseID, variant)

      cat("Querying tree list...", "\n")

      #Execute SQL query to obtain list of stand data
      standDF<-RSQLite::dbGetQuery(con,
                                   dbQuery)

      #Display column names and number of rows in standDF
      cat("Columns read from tree list:", colnames(standDF), "\n")
      cat("Number of rows read from tree list:", nrow(standDF), "\n")

      #Initialize invalidStand. This variable is used to determine if a stand
      #is invalid and should not be written to output argument.
      invalidStand = F

      #Skip to next stand if standDF has no nrows. This would occur if stand
      #has no live or dead records associated with it.
      if(nrow(standDF) <= 0)
      {
        noValidRecords <- noValidRecords + 1
        cat("Stand:", standID, "has no valid tree records.", "\n", "\n")
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

      #Create list that will store output for stand i for all years
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

        #If this is the last year to process and addCompute, addPotFire,
        #addCarbon, addFuels is T, move to next iteration of loop. This is done,
        #since these tables report one less cycle than FVS_Treelist and
        #FVS_Treelist_East
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

        #Calculate basal and percent canopy cover for each tree record
        standYrDF$TREEBA <- standYrDF$DBH^2 * standYrDF$TPA * 0.005454
        standYrDF$TREECC <- pi * (standYrDF$CrWidth/2)^2 *
          (standYrDF$TPA/43560) * 100

        #Create dataframe that will store output for the stand in a given year.
        yrOutput<-data.frame(RUNTITLE = run,
                             CASEID = caseID,
                             STANDID = standID,
                             VARIANT = variant,
                             REGION = region,
                             YEAR = years[j])

        #Bind data from vegOut to yrOutput
        yrOutput <- cbind(yrOutput,
                          vegOut(standYrDF))

        #If addVolume is TRUE, calculate volumes and add to yrOutput
        if(addVolume)
        {
          #Logic for eastern variants
          if(variant %in% c("CS", "LS", "NE", "SN"))
          {
            volume <- volumeCalc(standYrDF,
                                 vol1 = "MCuFt",
                                 vol2 = "SCuFt",
                                 vol3 = "SBdFt",
                                 vol1DBH = vol1DBH,
                                 vol2DBH = vol2DBH,
                                 vol3DBH = vol3DBH)

            yrOutput$VOL1 <- volume["VOL1"]
            yrOutput$VOL2 <- volume["VOL2"]
            yrOutput$VOL3 <- volume["VOL3"]
            yrOutput$DEADVOL1 <- volume["VOL4"]
            yrOutput$DEADVOL2 <- volume["VOL5"]
            yrOutput$DEADVOL3 <- volume["VOL6"]
          }

          #Logic for all other variants
          else
          {
            volume <- volumeCalc(standYrDF,
                                 vol1DBH = vol1DBH,
                                 vol2DBH = vol2DBH,
                                 vol3DBH = vol3DBH)

            yrOutput$VOL1 <- volume["VOL1"]
            yrOutput$VOL2 <- volume["VOL2"]
            yrOutput$VOL3 <- volume["VOL3"]
            yrOutput$DEADVOL1 <- volume["VOL4"]
            yrOutput$DEADVOL2 <- volume["VOL5"]
            yrOutput$DEADVOL3 <- volume["VOL6"]
          }
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

      #=========================================================================
      #Join compute variables to output if addCompute is TRUE and FVS_Compute
      #table exists in input
      #=========================================================================

      if(addCompute)
      {
        #Test if FVS_Compute table exists. Read in information from compute
        #table along with caseIDs.
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

          cat("Number of rows read from compute query",
              nrow(computeDF),
              "\n")

          #If computeDF has data, merge it to standOut
          if(nrow(computeDF) > 0)
          {

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

            #Add a blank NA row to computeDF
            computeDF[nrow(computeDF) + 1 ,] <- NA

            #Capitalize column names
            colnames(computeDF) <- toupper(colnames(computeDF))

            #Remove STANDID and Year from computeDF
            computeDF[c('STANDID', 'YEAR')] <- list(NULL)

            #Assign caseID to CASEID column
            computeDF$CASEID <- caseID

            #Merge computeDF to standOut by CASEID
            standOut <- merge(standOut,
                              computeDF,
                              by = c("CASEID"),
                              all.x = T)

            cat("Merging of FVS_Compute variables to stand:",
                standID,
                "is complete.",
                "\n")

            #Remove computeDF
            rm(computeDF)
          }
        }

        #Report that FVS_Compute table was not found in input.
        else
        {
          cat("\n")
          stop(paste("FVS_COMPUTE table not found in input database and",
                    "addCompute is true. Either set the addCompute argument to",
                    "FALSE or include the FVS_COMPUTE table in input database.",
                    "\n"))
        }
      }

      #=========================================================================
      #Join FVS_PotFire variables to output if addPotFire is TRUE and
      #FVS_PotFire table exists in input.
      #=========================================================================

      if(addPotFire)
      {

        #Set table name (potFire) for eastern variants: CS and SN
        if(variant %in% c("CS", "SN"))
        {
          potFire <- "FVS_POTFIRE_EAST"
        }

        #Set table name (potFire) for western variants
        else
        {
          potFire <- "FVS_POTFIRE"
        }

        #Test if FVS_PotFire table exists. Read in information from FVS_PotFire
        #table.
        if(RSQLite::dbExistsTable(con,
                                  potFire))
        {
          #Generate PotFire query
          dbQuery <- potFireQuery(caseID, variant)

          #Print FVS_PotFire query message
          cat("\n")
          cat("Querying FVS_PotFire...", "\n")

          #Get the data from FVS_PotFire
          potFireDF <- RSQLite::dbGetQuery(con,
                                           dbQuery)

          cat("Number of rows read from FVS_PotFire query",
              nrow(potFireDF),
              "\n")

          #If potFireDF has data, merge it to standOut
          if(nrow(potFireDF) > 0)
          {

            #Capitalize column names
            colnames(potFireDF) <- toupper(colnames(potFireDF))

            #If FVS_POTFIRE_EAST table is being processed additional columns
            #from FVS_POTFIRE will be added. This has to be done in case user
            #has set addPotFire to TRUE and a run with SN or CS variant is
            #being processed with a run from another variant which uses the
            #FVS_POTFIRE table.
            if(potFire == 'FVS_POTFIRE_EAST')
            {
              #Add the following columns to potFireDF: TOT_FLAME_SEV,
              #TOT_FLAME_MOD, FIRE_TYPE_SEV, FIRE_TYPE_MOD, PTORCH_SEV,
              #PTORCH_MOD, TORCH_INDEX, CROWN_INDEX
              potFireDF$TOT_FLAME_SEV <- NA
              potFireDF$TOT_FLAME_MOD <- NA
              potFireDF$FIRE_TYPE_SEV <- NA
              potFireDF$FIRE_TYPE_MOD <- NA
              potFireDF$PTORCH_SEV <- NA
              potFireDF$PTORCH_MOD <- NA
              potFireDF$TORCH_INDEX <- NA
              potFireDF$CROWN_INDEX <- NA

              #Change name of FLAME_LEN_SEV and FLAME_LEN_MOD to SURF_FLAME_SEV
              #SURF_FLAME_MOD respectively.
              names(potFireDF)[names(potFireDF) == "FLAME_LEN_SEV"] <-
                "SURF_FLAME_SEV"

              names(potFireDF)[names(potFireDF) == "FLAME_LEN_MOD"] <-
                "SURF_FLAME_MOD"

              #Now reorder potFireDF
              potFireVars <- getPotFireVars()
              potFireDF <- potFireDF[,potFireVars]
            }

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

            #Add a blank NA row to potFireDF
            potFireDF[nrow(potFireDF) + 1 ,] <- NA

            #Capitalize column names
            colnames(potFireDF) <- toupper(colnames(potFireDF))

            #If FVS_POTFIRE_EAST table is being processed additional columns
            #from FVS_POTFIRE will be added. This has to be done in case user
            #has set addPotFire to TRUE and a run with SN or CS variant is
            #being processed with a run from another variant which uses the
            #regular FVS_POTFIRE table.
            if(potFire == 'FVS_POTFIRE_EAST')
            {
              #Add the following columns to potFireDF: TOT_FLAME_SEV,
              #TOT_FLAME_MOD, FIRE_TYPE_SEV, FIRE_TYPE_MOD, PTORCH_SEV,
              #PTORCH_MOD, TORCH_INDEX, CROWN_INDEX
              potFireDF$TOT_FLAME_SEV <- NA
              potFireDF$TOT_FLAME_MOD <- NA
              potFireDF$FIRE_TYPE_SEV <- NA
              potFireDF$FIRE_TYPE_MOD <- NA
              potFireDF$PTORCH_SEV <- NA
              potFireDF$PTORCH_MOD <- NA
              potFireDF$TORCH_INDEX <- NA
              potFireDF$CROWN_INDEX <- NA

              #Change name of FLAME_LEN_SEV and FLAME_LEN_MOD to SURF_FLAME_SEV
              #SURF_FLAME_MOD respectively.
              names(potFireDF)[names(potFireDF) == "FLAME_LEN_SEV"] <-
                "SURF_FLAME_SEV"

              names(potFireDF)[names(potFireDF) == "FLAME_LEN_MOD"] <-
                "SURF_FLAME_MOD"

              #Now reorder potFireDF
              potFireVars <- getPotFireVars()
              potFireDF <- potFireDF[, potFireVars]
            }

            #Remove Year from potFireDF
            potFireDF['YEAR'] <- NULL

            #Assign caseID to CASEID column
            potFireDF$CASEID <- caseID

            #Merge FVS_Potfire to standOut by CASEID
            standOut <- merge(standOut,
                              potFireDF,
                              by = c("CASEID"),
                              all.x = T)

            cat("Merging of FVS_Potfire variables to stand:",
                standID,
                "is complete.",
                "\n")

            #Remove computeDF
            rm(potFireDF)
          }
        }

        #Report that FVS_PotFire/FVS_PotFire east table was not found in input.
        else
        {
          cat("\n")
          stop(paste(potFire, "table not found in input database and",
                    "addPotFire is true. Either set the addPotFire argument to",
                    "FALSE or include the", potFire, "table in input database.",
                    "\n"))
        }
      }

      #=========================================================================
      #Join FVS_Fuels variables to output if addFuels is TRUE and FVS_Fuels
      #table exists in input.
      #=========================================================================

      if(addFuels)
      {
        #Test if FVS_Fuels table exists. Read in information from FVS_Fuels
        #table.
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

          cat("Number of rows read from FVS_Fuels query",
              nrow(fuelsDF),
              "\n")

          #If fuelsDF has data, merge it to standOut
          if(nrow(fuelsDF) > 0)
          {

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

            #Add a blank NA row to fuelsDF
            fuelsDF[nrow(fuelsDF) + 1 ,] <- NA

            #Capitalize column names
            colnames(fuelsDF) <- toupper(colnames(fuelsDF))

            #Remove STANDID and Year from fuelsDF
            fuelsDF[ ,c('STANDID', 'YEAR')] <- list(NULL)

            #Assign caseID to CASEID column
            fuelsDF$CASEID <- caseID

            #Merge FVS_Fuels to standOut by CASEID
            standOut <- merge(standOut,
                              fuelsDF,
                              by = c("CASEID"),
                              all.x = T)

            cat("Merging of FVS_Fuels variables to stand:",
                standID,
                "is complete.",
                "\n")

            #Remove computeDF
            rm(fuelsDF)
          }
        }

        #Report that FVS_Fuels table was not found in input.
        else
        {
          cat("\n")
          stop(paste("FVS_FUELS table not found in input database and addFuels",
                     "is true. Either set the addFuels argument to FALSE or",
                     "include the FVS_FUELS table in input database.",
                    "\n"))
        }
      }

      #=========================================================================
      #Join FVS_Carbon variables to output if addCarbon is TRUE and FVS_Carbon
      #table exists in input.
      #=========================================================================

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

          cat("Number of rows read from FVS_Carbon query",
              nrow(carbonDF),
              "\n")

          #If carbonDF has data, merge it to standOut
          if(nrow(carbonDF) > 0)
          {

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

            #Add a blank NA row to carbonDF
            carbonDF[nrow(carbonDF) + 1 ,] <- NA

            #Capitalize column names
            colnames(carbonDF) <- toupper(colnames(carbonDF))

            #Remove STANDID and Year from carbonDF
            carbonDF[ ,c('STANDID', 'YEAR')] <- list(NULL)

            #Assign caseID to CASEID column
            carbonDF$CASEID <- caseID

            cat("Merging NA FVS_Carbon variables to stand:",
                standID,
                "\n")

            #Merge FVS_Carbon to standOut by CASEID
            standOut <- merge(standOut,
                              carbonDF,
                              by = c("CASEID"),
                              all.x = T)

            cat("Merging of FVS_Carbon variables to stand:",
                standID,
                "is complete.",
                "\n")

            #Remove carbonDF
            rm(carbonDF)
          }
        }

        #Report that FVS_Carbon table was not found in input.
        else
        {
          cat("\n")
          stop("FVS_CARBON table not found in input database and addCarbon",
               "is true. Either set the addCarbon argument to FALSE or",
               "include the FVS_CARBON table in input database.",
               "\n")
        }
      }

      #Rearrange column headers in standOut so RUNTITLE, CASEID, STANDID,
      #VARIANT, REGION, YEAR, and CY are reported in the correct order
      leadingCols <- c("RUNTITLE", "CASEID", "STANDID", "VARIANT", "REGION",
                       "YEAR", "CY")

      #Get column names from standOut
      colNames <- colnames(standOut)

      #Remove leading columns from colNames
      colNames <- colNames[!colNames %in% leadingCols]

      #Append leading columns to cols
      colNames <- c(leadingCols, colNames)

      #Rearrange the column headers in standOut
      standOut <- standOut[, c(colNames)]

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
      cat("\n")
      cat(paste0(rep("*", 75), collapse = ""), "\n")
      cat(standSum, "stands processed out of", nrow(cases), "\n")
      cat(paste0(rep("*", 75), collapse = ""), "\n", "\n")

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

    #Print number of stands that had no valid tree records (live or dead
    #records)
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

  #===========================================================================
  #If setIndices is true, then remove CaseID indices for each FVS table in
  #input that were previously created by setCaseIndices.
  #===========================================================================

  if(setIndices)
  {
    cat("Removing Case ID indices from:", input,
        "\n",
        "\n")

    removeCaseIndices(con)

    cat("Case ID indices removed from:", input,
        "\n",
        "\n")
  }

  #Disconnect from con
  RSQLite::dbDisconnect(con)
  cat("Disconnected from input database:", input, "\n", "\n")

  #=============================================================================
  #Determine how long main function took to process (approximate)
  #=============================================================================

  #Set the end time of function execution
  endTime <- Sys.time()

  #Determine duration in seconds for function execution
  duration <- as.numeric(difftime(endTime,
                                  startTime,
                                  units = "secs"))
  duration <- round(duration, 0)

  #Determine hours
  hours <- floor(duration/3600)

  #Determine minutes
  mins <- floor(duration/60) %% 60

  #Determine seconds
  secs <- duration %% 60

  #Print startTime, endTime, total processing time and end of program
  cat(paste("Start time:", startTime, "\n"))
  cat(paste("End time:", endTime, "\n"))
  cat("Total processing time:", hours, "hours", mins, "minutes", secs,
      "seconds", "\n")
  cat("End of program.")

  #Return from function main
  return(invisible(0))
}
