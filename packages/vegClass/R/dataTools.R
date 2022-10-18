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

  #Create dataframe containing FVS_GroupAddfilesAndKeywords table
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
#Function: eru
#
#This function returns a vector of ERU codes (USFS R3 ERU Codes)
#
#Argument
#
#None
#
#Return value
#
#Vector containing USFS R3 ERU Codes
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

################################################################################
#Function pvConvert
#
#This function converts input PV Code to USFS Region 3 ERU codes. If the input
#PV Code is not recognized, then a NA value is returned.
#
#Arguments
#
#pv: input PV code.
#
#Return value
#
#ERU code.
################################################################################

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

################################################################################
#Function: dbCombine
#
#Function dbCombine is used to read in database tables from input FVS-ready
#data sets and write the database tables from each of these to a single output
#SQLite database.
#
#Arguments:
#
#dbIn:     Character vector of directory paths and file names of FVS-ready SQLite
#          databases to process.
#
#dbOut:    Character string corresponding to SQLite database to write out to.
#
#dbTables: Character vector of database tables to process from argument dbIn.
#          By default this argument contains the following values:
#          FVS_STANDINIT
#          FVS_TREEINIT
#          FVS_STANDINIT_PLOT
#          FVS_STANDINIT_COND
#          FVS_PLOTINIT_PLOT
#          FVS_TREEINIT_PLOT
#          FVS_TREEINIT_COND
#
#buildGaak: Boolean variable used to determine if FVS_GROUPADDFILESANDKEYWORDS
#           will be written to dbOut. If TRUE, this table will be written to
#           dbOut. By default this argument is set to TRUE.
#
#gaakType:  Integer value from 1 - 3 used to determine what kind of GAAK table
#           will be written to dbOut if buildGaak is TRUE.
#           1: GAAK table with All_Stands and All_Plots grouping codes.
#           2: GAAK table with All_FIA_Conditions, All_FIA_Plots,
#              All_FIA_Subplots grouping codes.
#           3: GAAK table with All_Stands, All_Plots, All_FIA_Conditions,
#              All_FIA_Plots, All_FIA_Subplots grouping codes.
#           For more information refer to fvsGaak function in dataTools.R
#
#addEru:    Boolean variable used to determine if ERU should be added as value
#           to groups field of FVS_STANDINIT, FVS_PLOTINIT, FVS_STANDINIT_PLOT,
#           FVS_STANDINIT_COND, and FVS_PLOTINIT_PLOT. If TRUE, ERU as defined
#           by USFS Region 3 will be added to groups column of above tables. By
#           default this argument is set to TRUE.
#
#Value
#
#Message indicating that database has been created.
################################################################################


#'@export
dbCombine <- function(dbIn = NULL,
                      dbOut = NULL,
                      dbTables = c("FVS_STANDINIT",
                                   "FVS_TREEINIT",
                                   "FVS_STANDINIT_PLOT",
                                   "FVS_STANDINIT_COND",
                                   "FVS_PLOTINIT_PLOT",
                                   "FVS_TREEINIT_PLOT",
                                   "FVS_TREEINIT_COND"),
                      buildGaak = T,
                      gaakType = 2,
                      addERU = T)
{

  #Vector containing valid table names that can be processed. These values
  #match the default values in dbTables argument.
  validTables <- c("FVS_STANDINIT",
                   "FVS_TREEINIT",
                   "FVS_STANDINIT_PLOT",
                   "FVS_STANDINIT_COND",
                   "FVS_PLOTINIT_PLOT",
                   "FVS_TREEINIT_PLOT",
                   "FVS_TREEINIT_COND")

  #Test if no values have been specified for dbIn
  if(is.null(dbIn))
  {
    stop(paste("No files were specified for dbIn."))
  }

  #Test if no values have been specified for dbOut
  if(is.null(dbOut))
  {
    stop(paste("No file was specified for dbOut."))
  }

  #Test if dbTables is null and return if null. Otherwise capitalize the
  #table names in dbTables.
  if(is.null(dbTables))
  {
    stop(paste("No table names were provided for dbTables."))
  }
  else
  {
    dbTables <- toupper(dbTables)
  }

  #Select only tables found in validTables
  dbTables <- dbTables[dbTables %in% validTables]
  cat("Database table names to consider:",
      dbTables,
      "\n")

  #Catch erroneous gaakType values
  if(gaakType < 1 | gaakType > 3)
  {
    gaakType = 2
  }

  #Loop through dbIn and test if any of the files don't exist
  for(i in 1:length(dbIn))
  {
    if(!file.exists(dbIn[i]))
    {
      stop(paste("File:",
                 dbIn[i],
                 "does not exist."))
    }

    else
    {
      cat("Database", i, dbIn[i], "\n")
    }
  }

  #Test if output file is a SQLite database
  fileExtOut<-sub("(.*)\\.","",dbOut)
  if(!fileExtOut %in% "db")
  {
    stop("Output database:",
         dbOut,
         "is not a SQLite database.",
         "\n")
  }

  #If dbOut already exists, delete it
  if(file.exists(dbOut))
  {
    unlink(dbOut)
  }

  cat("Output database:", dbOut, "\n","\n")

  #Call collectFieldNames to identify all possible field names from each
  #database table in dbTables argument. fieldNamesList is a named list where
  #each entry in the list contains a character vector of field names for each
  #database table specified in dbTables.
  fieldNamesList <- collectFieldNames(dbIn,
                                      dbTables)

  #Begin processing dbIn
  for(i in 1:length(dbIn))
  {

    #Extract database to process
    db <- dbIn[i]

    cat("Processing db:", db, "\n", "\n")

    #If the file extension of db is not .db then skip to next iteration of loop.
    fileExtIn<-sub("(.*)\\.","",db)
    if(!fileExtIn %in% "db")
    {
      cat("File: ",
          db,
          "is not a SQLite database and will be skipped.",
          "\n")
      next
    }

    #Begin processing dbTables
    for(j in 1:length(dbTables))
    {
      #Extract table name
      tableName <- dbTables[j]
      cat("Processing table:",
          tableName,
          "\n")

      #Connect to db
      conIn <- RSQLite::dbConnect(RSQLite::SQLite(), db)

      #Test if table does not exist in db. if this is the case move to next
      #iteration of loop.
      if(!tableName %in% toupper(RSQLite::dbListTables(conIn)))
      {
        cat("Table:",
            tableName,
            "was not found in database.",
            "\n", "\n")
        next
      }

      #Read in the dbTable table.
      dbTable <- RSQLite::dbReadTable(conIn,
                                      name = tableName)

      #Capitalize column headers
      colnames(dbTable) <- toupper(colnames(dbTable))

      #Pull fieldNames from current table being processed
      fieldNames <- fieldNamesList[[tableName]]

      #Determine if ERU needs to be added to dbTable
      if(addERU & tableName %in% c("FVS_STANDINIT",
                                   "FVS_PLOTINIT",
                                   "FVS_STANDINIT_PLOT",
                                   "FVS_STANDINIT_COND",
                                   "FVS_PLOTINIT_PLOT"))
      {
        #Determine if PV_CODE and GROUPS fields exist in dbTable. If they don't,
        #then ERU will not be cross walked and not be included in output database.
        if(! "PV_CODE" %in% colnames(dbTable) | ! "GROUPS" %in% colnames(dbTable))
        {
          cat("PV_CODE and/or GROUPS column not found in",
          tableName,
          ".ERU cross walk will not occur.",
          "\n")
        }

        #Cross walk PV_CODE to ERU
        else
        {
          cat("Cross walking PV_CODE to ERU in", tableName, "\n")

          #Determine if we are processing the FVS_STANDINIT_PLOT or
          #FVS_STANDINIT_COND tables. If so, move values from PV_FIA_HABTYPCD1
          #to PV_CODE
          if(tableName %in% c("FVS_STANDINIT_PLOT",
                              "FVS_STANDINIT_COND") &
             "PV_FIA_HABTYPCD1" %in% colnames(dbTable))
          {
            dbTable["PV_CODE"] <- dbTable["PV_FIA_HABTYPCD1"]
          }

          #Cross walk PV_CODE to ERU
          dbTable$ERU<-mapply(pvConvert, dbTable$PV_CODE)

          #Add ERU to groups column
          dbTable$GROUPS<-paste(dbTable$GROUPS,
                                paste0("ERU=",
                                        dbTable$ERU))

          #Add ERU to fieldNames
          fieldNames <- c(fieldNames, "ERU")
          cat("ERU added to fieldNames", fieldNames, "\n")
          cat("ERU added to GROUPS column of", tableName, "\n")
        }
      }

      #If there are no rows (i.e. no data) in dbTable, skip to next iteration
      #of loop.
      if(nrow(dbTable) <= 0)
      {
        cat("No data found in",
            tableName,
            "\n")

        #Disconnect from conIn
        RSQLite::dbDisconnect(conIn)
        next
      }

      #Disconnect from dbIn
      RSQLite::dbDisconnect(conIn)

      #Connect to dbOut
      conOut <- RSQLite::dbConnect(RSQLite::SQLite(),
                                   dbOut)

      #Test if tableName exists in conOut. If it does, this dbTable will be
      #appended to the existing table in output (conOut).
      if(tableName %in% toupper(RSQLite::dbListTables(conOut)))
      {
        #Set datatypes of dbTable
        dbTable <- setDataTypes(dbTable,
                                ignoreCols = F)

        cat("Appending", tableName, "to", dbOut, "\n", "\n")

        #Append data to conOut
        RSQLite::dbWriteTable(conn = conOut,
                              name = tableName,
                              value = dbTable,
                              append = T)
      }

      #Table will be created in conOut and data will then be written to the
      #table.
      else
      {
        #Print number of fields in fieldNames and dbTable
        cat("Number of fields in fieldNames:", length(fieldNames), "\n")
        cat("Number of fields in dbTable:", length(names(dbTable)), "\n")

        #Identify any columns from field names that are not in dbTable
        notInDb <- fieldNames[! fieldNames %in% names(dbTable)]
        if(length(notInDb) > 0)cat("Fields not included in dbTable", notInDB, "\n")

        #For any fields that dbTable is missing, a column with a NA value will
        #be added.
        if(length(notInDb) > 0)
        {
          for(n in 1:length(notInDb))
          {
            field <- notInDb[n]
            cat("Adding", field, "to database.", "\n")
            dbTable[field] <- NA
          }
        }

        #Set datatypes of dbTable
        dbTable <- setDataTypes(dbTable,
                                ignoreCols = F)

        cat("Writing", tableName, "to", dbOut, "\n", "\n")

        #Create the dbTable in conOut and write information from dbTable to it.
        RSQLite::dbWriteTable(conn = conOut,
                              name = tableName,
                              value = dbTable,
                              overwrite = T)
      }

      #Delete dbTable
      rm(dbTable)

      #Disconnect from conOut
      RSQLite::dbDisconnect(conOut)
    }

    #Print message indicating which db has been processed.
    cat("Finished processing db:",
        db,
        "\n",
        "\n")
  }

  #Determine if GAAK table should be written to dbOut.
  if(buildGaak)
  {
    conOut <- RSQLite::dbConnect(RSQLite::SQLite(),
                                 dbOut)

    cat("Writing fvsGAAK table to",
        dbOut,
        "\n",
        "\n")

    RSQLite::dbWriteTable(conn = conOut,
                          name = "FVS_GROUPADDFILESANDKEYWORDS",
                          value = fvsGaak(type = gaakType),
                          overwrite = T)

    #Disconnect from conOut
    RSQLite::dbDisconnect(conOut)
  }

  return("Data created!")
}

################################################################################
#Function: collectFieldNames
#
#Arguments:
#
#dbIn:     Character vector of directory paths and file names of SQLite
#          databases to process.
#
#dbTables: Character vector of database tables to process from argument dbIn.
#          By default this argument contains the following values:
#          FVS_STANDINIT
#          FVS_TREEINIT
#          FVS_STANDINIT_PLOT
#          FVS_STANDINIT_COND
#          FVS_PLOTINIT_PLOT
#          FVS_TREEINIT_PLOT
#          FVS_TREEINIT_COND
#
#Return value
#
#Named list where each index of list contains a character vector of field names
#corresponing to database tables in dbTables argument.
################################################################################

collectFieldNames <- function(dbIn,
                              dbTables)
{
  cat("In collectFieldNames", "\n")

  #Create list that matches length of dbTables
  fieldNames <- vector(mode = "list",
                       length = length(dbTables))

  #Assign names to fieldNames
  for(i in 1:length(fieldNames))
  {
    names(fieldNames)[i] <- dbTables[i]
  }

  #Loop across dbIn
  for(i in 1:length(dbIn))
  {
    #Extract db
    db <- dbIn[i]

    cat("Processing db:", db, "\n")

    #Connect to input database
    conIn <- RSQLite::dbConnect(RSQLite::SQLite(),
                                db)

    #Loop across dbTables
    for(j in 1:length(dbTables))
    {
      #Extract table name
      tableName <- dbTables[j]
      cat("Processing table:", tableName, "\n")

      #If database table is not in conIn, skip to next iteration of loop
      if(! tableName %in% toupper(RSQLite::dbListTables(conIn)))
      {
        cat("Table", tableName, " not found in database.", "\n")
        next
      }

      else
      {
        #Store field names for tableName in fNames
        fNames <- fieldNames[[tableName]]

        #If fNames is null assign values from dbListTables to fNames
        if(is.null(fNames)){
          fNames <- RSQLite::dbListFields(conIn,
                                          name = tableName)
        }

        #Append new columns to fNames
        else{
          fNames <- unique(c(fNames, RSQLite::dbListFields(conIn,
                                                           name = tableName)))
        }

        #Add fNames to tableName in fieldNames
        fieldNames[[tableName]] <- fNames
      }
    }

    RSQLite::dbDisconnect(conIn)
  }

  cat("Field names collected.", "\n")
  cat("Leaving collectFieldNames", "\n", "\n")
  return(fieldNames)
}

################################################################################
#Function: fvsGetCols
#
#This function returns a vector of field names corresponding to all columns
#which can be included in a FVS input database.
#
#Arguments
#
#none
#
#Return value
#Character vector of FVS column names
################################################################################

fvsGetCols <- function()
{
  #FVS variables from blank database templates
  fvsVars =c(
    "STAND_ID",      "VARIANT",      "INV_YEAR",     "GROUPS",            "ADDFILES",
    "FVSKEYWORDS",   "GIS_LINK",     "PROJECT_NAME", "LATITUDE",          "LONGITUDE",
    "REGION",        "FOREST",       "DISTRICT",     "COMPARTMENT",       "LOCATION",
    "ECOREGION",     "PV_CODE",      "PV_REF_CODE",  "AGE",               "ASPECT",
    "SLOPE",         "ELEVATION",    "ELEVFT",       "BASAL_AREA_FACTOR", "INV_PLOT_SIZE",
    "BRK_DBH",       "NUM_PLOTS",    "NONSTK_PLOTS", "SAM_WT",            "STK_PCNT",
    "DG_TRANS",      "DG_MEASURE",   "HTG_TRANS",    "HTG_MEASURE",       "MORT_MEASURE",
    "MAX_BA",        "MAX_SDI",      "SITE_SPECIES", "SITE_INDEX",        "MODEL_TYPE",
    "PHYSIO_REGION", "FOREST_TYPE",  "STATE",        "COUNTY",            "FUEL_MODEL",
    "FUEL_0_25_H",   "FUEL_25_1_H",  "FUEL_1_3_H",   "FUEL_3_6_H",        "FUEL_6_12_H",
    "FUEL_12_20_H",  "FUEL_20_35_H", "FUEL_35_50_H", "FUEL_GT_50_H",      "FUEL_0_25_S",
    "FUEL_25_1_S",   "FUEL_1_3_S",   "FUEL_3_6_S",   "FUEL_6_12_S",       "FUEL_12_20_S",
    "FUEL_20_35_S",  "FUEL_35_50_S", "FUEL_GT_50_S", "FUEL_LITTER",       "FUEL_DUFF",
    "PHOTO_REF",     "PHOTO_CODE",   "PLOT_ID",      "STANDPLOT_ID",      "TREE_ID",
    "TREE_COUNT",    "HISTORY",      "SPECIES",      "DIAMETER",          "DG",
    "HT",            "HTG",          "HTTOPK",       "CRRATIO",           "DAMAGE1",
    "SEVERITY1",     "DAMAGE2",      "SEVERITY2",    "DAMAGE3",           "SEVERITY3",
    "TREEVALUE",     "PRESCRIPTION", "TOPOCODE",     "SITEPREP",          "DBH",
    "STAND_CN",      "STANDPLOT_CN")

  return(fvsVars)
}

################################################################################
#Function: fvsGetTypes
#
#This function returns a character vector containing data types associated with
#values returned from fvsGetCols function.
#
#Arguments
#
#none
#
#Return value
#Character vector of FVS field data types.
################################################################################

fvsGetTypes <- function()
{
  #Datatypes for FVS variables
  fvsTypes = c(
    "character", "character", "integer",   "character", "character",
    "character", "character", "character", "double",    "double",
    "integer",   "integer",   "integer",   "integer",   "integer",
    "character", "character", "integer",   "integer",   "double",
    "double",    "double",    "double",    "double",    "double",
    "double",    "integer",   "integer",   "double",    "double",
    "integer",   "integer",   "integer",   "integer",   "integer",
    "double",    "double",    "character", "double",    "integer",
    "integer",   "integer",   "integer",   "integer",   "integer",
    "double",    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",    "double",
    "integer",   "character", "double",    "character", "double",
    "double",    "double",    "character", "double",    "double",
    "double",    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",    "double",
    "character", "character")

  return(fvsTypes)
}

################################################################################
#Function: setDataTypes
#
#This function accepts a dataframe and checks if all columns in the data frame
#match a specified datatype. If a column does not match a specified data type,
#the column in the dataframe is cast to the correct data type. Only double,
#integer, and character values are considered in this function. If a column is
#not recognized, then it is either ignored or cast to character type depending
#on the value specified in argument ignoreCols.
#
#Arguments
#
#data:       Input dataframe
#
#cols:       Character vector of variable names.By default this argument
#            is set  to NULL. When this value is NULL, variables will be
#            set to values produced by fvsGetCols function. Length of variables
#            argument must match length of types argument.
#
#colTypes:   Character vector of data type types that correspond to variables in
#            argument variables. By default this argument is set to NULL. When
#            this value is NULL, variables will be set to values produced by
#            fvsGetTypes function. Length of types argument must match length of
#            types argument.
#
#ignoreCols: Boolean variable that determines how to handle columns that are in
#            argument data but not in argument variables. If this argument is
#            FALSE, then these columns will be cast to character type. If this
#            argument is TRUE, then these columns are ignored. By default this
#            argument is set to FALSE.
#
#verbose:    Boolean variable that determines if debug information should be
#            printed to console. By default this argument set to FALSE.
#
#Return value
#
#Input dataframe
################################################################################

setDataTypes<-function(data,
                       cols = NULL,
                       colTypes = NULL,
                       ignoreCols = F,
                       verbose = F)
{

  #If data is not dataframe stop with error message
  if(!is.data.frame(data))
  {
    stop("Argument data must be a dataframe.")
  }

  #If data has no rows return
  if(nrow(data) <= 0)
  {
    return("No data in input dataframe.")
  }

  #If cols is NULL, call fvsGetVars
  if(is.null(cols))
  {
    cols = fvsGetCols()
  }

  #If colTypes is NULL, call fvsGetTypes
  if(is.null(colTypes))
  {
    colTypes = fvsGetTypes()
  }

  #If length of variables is not equal to types, return with error.
  if(length(cols) != length(colTypes))
  {
    if(verbose) cat("Length cols:", length(cols), "\n")
    if(verbose) cat("Length types:", length(colTypes), "\n")
    stop("Variables and types arguments must have the same length.")
  }

  #Iterate across columns of input dataframe
  for(i in 1:length(names(data)))
  {
    #Extract column name
    colname<-toupper(names(data)[i])
    if(verbose) cat("Column:", colname, "being processed.", "\n")

    #attempt to match column name with variable in fvsvars
    varIndex<-match(colname, cols)

    #If varIndex is not NA, extract the data type for the column from colTypes.
    if(!is.na(varIndex))
    {
      #Extract datatype from colTypes.
      datatype<-colTypes[varIndex]

      #If data type of column matches with designated data type, move to next
      #loop iteration.
      if(typeof(data[,i]) == datatype){
        if(verbose) cat("Data type of", colname, "is a match.", "\n")
        next
      }

      #Variable is a character
      if(datatype == "character")
      {
        #Print message that column will be converted datatype.
        if(verbose) cat(colname, "being converted to", datatype, "\n")
        data[,i]<-as.character(data[,i])
      }

      #Variable is a integer
      if(datatype == "integer")
      {
        #Print message that column will be converted datatype.
        if(verbose) cat(colname, "being converted to", datatype, "\n")
        data[,i]<-as.integer(data[,i])
      }

      #Variable is a double
      if(datatype == "double")
      {
        #Print message that column will be converted datatype.
        if(verbose) cat(colname, "being converted to", datatype, "\n")
        data[,i]<-as.double(data[,i])
      }
    }

    #If varIndex is NA, then determine what will happen with column. The column
    #will either be ignored or converted to character type depending on input
    #argument ignoreCols.
    else
    {
      if(!ignoreCols)
      {
        if(verbose) cat(colname,
                        "not recognized and being converted to character",
                        "\n")
        data[,i]<-as.character(data[,i])
      }

      else{
        if(verbose) cat(colname, "not recognized and ignored.", "\n")
      }
    }
  }

  return(data)
}

