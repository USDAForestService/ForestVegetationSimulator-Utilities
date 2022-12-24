################################################################################
#Function: fvsGaak
#
#This function creates a FVS_GroupAddFilesAndKeyword (GAAK) table with the
#appropriate SQL statements for a variety of grouping codes.
#
#Arguments
#
#dbName: Character string corresponding to name of database used in GAAK table.
#        By default this value is set to FVS_Data. A .db extension will be
#        appended to the value specified in dbName.
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
################################################################################

fvsGaak<-function(dbName="FVS_Data", type = 2)
{
  #Capture invalid type argument values
  if(type < 1 | type > 3) type = 2

  #Create dataframe containing FVS_GroupAddfilesAndKeywords table
  gaak<-data.frame(GROUPS = c("All_Stands","All_Plots","All_FIA_Conditions",
                              "All_FIA_Plots", "All_FIA_Subplots"),
                   ADDFILES = c("","","","",""),
                   FVSKEYWORDS = c(paste("Database", "DSNin",
                                         paste0(dbName, ".db"), "StandSQL",
                                         "SELECT *", "FROM  FVS_StandInit",
                                         "WHERE Stand_ID = '%StandID%'",
                                         "EndSQL","TreeSQL", "SELECT *",
                                         "FROM FVS_TreeInit",
                                         "WHERE Stand_ID ='%StandID%'",
                                         "EndSQL", "END", sep = "\n"),
                                   paste("Database", "DSNin",
                                         paste0(dbName, ".db"), "StandSQL",
                                         "SELECT *", "FROM  FVS_PlotInit",
                                         "WHERE StandPlot_ID = '%StandID%'",
                                         "EndSQL","TreeSQL", "SELECT *",
                                         "FROM FVS_TreeInit",
                                         "WHERE StandPlot_ID ='%StandID%'",
                                         "EndSQL", "END", sep = "\n"),
                                   paste("Database", "DSNin",
                                         paste0(dbName, ".db"), "StandSQL",
                                         "SELECT *", "FROM  FVS_StandInit_Cond",
                                         "WHERE Stand_CN = '%Stand_CN%'",
                                         "EndSQL","TreeSQL", "SELECT *",
                                         "FROM FVS_TreeInit_Cond",
                                         "WHERE Stand_CN ='%Stand_CN%'",
                                         "EndSQL", "END", sep = "\n"),
                                   paste("Database", "DSNin",
                                         paste0(dbName, ".db"),
                                         "StandSQL", "SELECT *",
                                         "FROM  FVS_StandInit_Plot",
                                         "WHERE Stand_CN = '%Stand_CN%'",
                                         "EndSQL","TreeSQL", "SELECT *",
                                         "FROM FVS_TreeInit_Plot",
                                         "WHERE Stand_CN ='%Stand_CN%'",
                                         "EndSQL", "END", sep = "\n"),
                                   paste("Database", "DSNin",
                                         paste0(dbName, ".db"), "StandSQL",
                                         "SELECT *", "FROM  FVS_PlotInit_Plot",
                                         "WHERE StandPlot_CN = '%Stand_CN%'",
                                         "EndSQL","TreeSQL", "SELECT *",
                                         "FROM FVS_TreeInit_Plot",
                                         "WHERE StandPlot_CN ='%Stand_CN%'",
                                         "EndSQL", "END", sep = "\n")))

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

################################################################################
#Function: pvCodes
#
#This function returns a vector of USFS Region 3 Plant Association codes (PV
#code). This function is called in pvConvert to crosswalk PV Code to ERU.
#
#Argument
#
#None
#
#Return value
#
#Vector containing PV Codes
################################################################################

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

################################################################################
#Function: eru
#
#This function returns a vector of USFS Region 3 ERU codes. This function is
#called in pvConvert to crosswalk PV Codes to ERU.
#
#Argument
#
#None
#
#Return value
#
#Vector containing USFS R3 ERU Codes
################################################################################

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
#This function converts input Plant Association code (PV code) to USFS Region
#3 ERU code. If the input PV Code is not recognized, then a NA value is
#returned.
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
#Function addERUGroup
#
#This function adds an ERU group label to GROUPS column of FVS_STANDINIT,
#FVS_PLOTINIT, FVS_STANDINIT_PLOT, FVS_STANDINIT_COND, FVS_PLOTININT_PLOT for
#PPF. This function is called from dbCombine when addERU is TRUE.
#
#Arguments
#
#group: character string of FVS group labels
#
#eru:   character string representing ERU code.
#
#Return value
#
#ERU code.
################################################################################

addERUGroup <- function(group, eru)
{
  #If eru is NA return group
  if(is.na(eru))
  {
    return(group)
  }

  #Add ERU grouping code to group
  group <- paste(group, paste0("ERU=", eru))

  #If ERU is PPG or PPO, then add an additional grouping code for PPF
  if(eru == 'PPG' | eru == 'PPO')
  {
    group <- paste(group, "ERU=PPF")
  }

  #Return group
  return(group)
}

################################################################################
#Function: dbCombine
#
#This function is used to read in database tables from input FVS-ready data sets
#and write the database tables from each of these to a single output SQLite
#database. SQLite databases (.db) are the only compatible input database type
#that can be processed in this function. The primary purpose of this function is
#to combine input FVS databases into a single database or extract FVS database
#tables from a larger database such as those on the FIA datamart.
#
#Arguments:
#
#dbIn:         Character vector of directory paths and file names for SQLite
#              databases to process. Files can either be a SQLite database (.db)
#              or zipped folder (.zip) which contains a SQLite database(s).

#              NOTE: .zip files will be unzipped to a temporary folder called
#              xxxvegClassdbCombineUnzipxxx in current working directory.
#              Temporary folder will be deleted after dbCombine has finished
#              writing data to output database.
#
#              Examples of valid dbIn formats:
#             "C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FIADB_AZ.db",
#             "C:\\FIA2FVS_Databases\\SQLite_FIADB_AZ\\FIADB_AZ.db"
#
#             "C:/FIA2FVS_Databases/SQLite_FIADB_AZ/ FIADB_AZ.zip",
#             "C:\\FIA2FVS_Databases\\SQLite_FIADB_AZ\\ FIADB_AZ.zip "
#
#dbOut:       Character string corresponding to SQLite database to write out to.
#
#             Examples of valid dbOut formats:
#             "C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FVS_Data.db",
#             "C:\\FIA2FVS_Databases\\SQLite_FIADB_NM\\FVS_Data.db"
#
#dbTables:    Character vector of database tables to process from argument dbIn.
#             By default this argument contains the following values:
#             "FVS_STANDINIT"
#             "FVS_TREEINIT"
#             "FVS_STANDINIT_PLOT"
#             "FVS_STANDINIT_COND"
#             "FVS_PLOTINIT_PLOT"
#             "FVS_TREEINIT_PLOT"
#             "FVS_TREEINIT_COND"
#
#buildGaak:   Logical variable used to determine if FVS_GROUPADDFILESANDKEYWORDS
#             will be written to dbOut. If TRUE, this table will be written to
#             dbOut. By default this argument is set to TRUE.
#
#gaakType:    Integer value from 1 - 3 used to determine what kind of GAAK table
#             will be written to dbOut if buildGaak is TRUE.
#             1: GAAK table with All_Stands and All_Plots grouping codes.
#             2: GAAK table with All_FIA_Conditions, All_FIA_Plots,
#                All_FIA_Subplots grouping codes.
#             3: GAAK table with All_Stands, All_Plots, All_FIA_Conditions,
#                All_FIA_Plots, All_FIA_Subplots grouping codes.
#             For more information refer to fvsGaak function.
#
#addEru:      Logical variable used to determine if ERU should be added as a
#             field in FVS_STANDINIT, FVS_PLOTINIT, FVS_STANDINIT_PLOT,
#             FVS_STANDINIT_COND, and FVS_PLOTINIT_PLOT tables. In addition,
#             ERU code will be added as a grouping code in the GROUPS field of
#             these tables if addERU is TRUE. By default this argument is set to
#             TRUE.
#
#deleteInput: Logical variable used to determine if values in dbIn should be
#             deleted after dbCombine has been called. By default this argument
#             is set to FALSE. Be careful with this argument. The primary
#             purpose of this argument is to conserve hard disk space for users
#             who do not want the input databases specified in dbIn.
#
#readChunks:  Logical variable used to determine if data from database table
#             should be read in chunks. In general, processing time of dbCombine
#             increases but less RAM is used in R session if this argument is
#             TRUE. By default this argument is set to FALSE.
#
#rowsToRead:  Integer value corresponding to number of rows to read from a
#             database table if readChunks is TRUE. By default this argument is
#             set to 5000.
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
                      addERU = T,
                      deleteInput = F,
                      readChunks = F,
                      rowsToRead = 5000)
{

  #Create directory where file will be unzipped to.
  #If this file exists for any reason, delete it.
  unzipDir <- paste(getwd(),
                    "xxxvegClassdbCombineUnzipxxx",
                    sep = "/")

  if(file.exists(unzipDir))
  {
    unlink(unzipDir,
           recursive = T)
  }

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

  #Test if dbTables is null and return with error message. Otherwise capitalize
  #the table names in dbTables.
  if(is.null(dbTables))
  {
    stop(paste("No table names were provided for dbTables."))
  }
  else
  {
    dbTables <- toupper(dbTables)

    #Select only tables found in validTables
    dbTables <- dbTables[dbTables %in% validTables]
    cat("Database table names to consider:",
        dbTables,
        "\n")

    #If dbTables is empty after selection from validTables, stop with error
    #message.
    if(length(dbTables) <= 0)
    {
      stop(paste("No valid databases tables to process."))
    }
  }

  #Catch erroneous gaakType values
  if(gaakType < 1 | gaakType > 3)
  {
    gaakType = 2
  }

  #Report error message if rowsToRead is less than or equal to 0
  if(readChunks)
  {
    rowsToRead <- as.integer(rowsToRead)
    if(rowsToRead <= 0)
    {
      stop(paste("Value for rowsToRead needs to be integer value greater than",
                 "zero."))
    }
  }

  #Replace \\ with / in dbIn and dbOut
  dbIn <- gsub("\\\\", "/", dbIn)
  dbOut <- gsub("\\\\", "/", dbOut)

  #Loop through dbIn and test if any of the files don't exist. If a file does
  #not exist then error message is reported.
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

  #If there is more than one value specified in dbOut, stop with error message.
  if(length(dbOut) > 1)
  {
    stop(paste("Only one output file can be specified for dbOut."))
  }

  #Test if dbOut file path is valid.
  #Extract path to dbOut by extracting all characters before the last / in
  #output argument.
  outPath <- gsub("/[^/]+$", "", dbOut)

  #Test existence of output path and if it does not exist report error.
  if (!(file.exists(outPath))){
    stop(paste("Path to output:", outPath, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }

  #Test if output file is a SQLite database. If the file is not a SQLite
  #database then error message is reported.
  fileExtOut<-sub("(.*)\\.","",dbOut)
  if(!fileExtOut %in% "db")
  {
    stop(paste("Output database:",
         dbOut,
         "is not a SQLite database.",
         "\n"))
  }

  #If dbOut already exists, delete it
  if(file.exists(dbOut))
  {
    cat(paste0("\n","Deleting preexisting dbOut"), "\n")
    unlink(dbOut,
           force = T)
  }

  cat("Output database:", dbOut, "\n","\n")

  #Initialize dbInUpdate. This is a vector that will be used to store input
  #directory paths.
  dbInUpdate <- vector(mode = "character")

  #Loop through dbIn and check if files are not .db or .zip. If a file is a .zip
  #then unzip it to unzipDir. All db files will be added to dbInUpdate.
  for(i in 1:length(dbIn))
  {
    db <- dbIn[i]

    cat("Processing db:", db, "\n")

    #Grab file extension for db
    fileExtIn<-sub("(.*)\\.","",db)

    #Display file extension in console
    cat("File extension:",
        fileExtIn,
        "\n",
        "\n")

    #If the file extension of db is not .db or .zip then stop with error message.
    if(!fileExtIn %in% c("db", "zip"))
    {
      stop(paste("File:",
          db,
          "is not a SQLite database or zip file.",
          "\n"))
    }

    #If the file is a zip file, then it will be unzipped into xxxdbCombinexxx
    if(fileExtIn == "zip")
    {
      #Create directory where file will be unzipped to
      unzipDir <- paste(getwd(),
                        "xxxvegClassdbCombineUnzipxxx",
                        sep = "/")

      cat("Unzipping:", db, "to", unzipDir, "\n", "\n")

      #Unzip the file
      unzip(zipfile = db,
            exdir = unzipDir)

      #Now list all the files that contain .db in the name.
      #Recursive argument is set to true so any sub directories are checked for
      #db files as well.
      dbList <- list.files(unzipDir,
                           pattern = ".db",
                           full.names = T,
                           recursive = T)

      #If dbList is empty move to next iteration of loop
      if(length(dbList) <= 0)
      {
        cat("No .db file found in", db, "\n")
        next
      }

      #If dbList has at least one value then append the values in dbList to
      #dbInUpdate.
      else
      {
        dbInUpdate <- c(dbInUpdate, dbList)
      }
    }

    #Dealing with .db file. This file will be appended to dbInUpdate.
    else
    {
      dbInUpdate <- c(dbInUpdate, db)
    }
  }

  #If dbInUpdate does not have any databases, then stop with error message and
  #delete unzip directory if it exists.
  if(length(dbInUpdate) <= 0)
  {
    #Check if unzipDir exists. If it does, delete it.
    if(file.exists(unzipDir))
    {
      retCode <- unlink(unzipDir,
                        recursive = T,
                        force = T)

      if(retCode != 0)
      {
        cat("Failed to delete temporary unzip directory:", unzipDir, "\n")
      }
    }

    stop("No valid database files (.db) are available for processing.")
  }

  #Remove duplicate values in dbInUpdate
  dbInUpdate <- unique(dbInUpdate)
  cat("List of db files to process:", "\n")

  #List values in dbInUpdate
  for(i in 1:length(dbInUpdate))
  {
    cat("Database", i, dbInUpdate[i], "\n")
  }

  #Begin processing dbInUpdate
  for(i in 1:length(dbInUpdate))
  {

    #Extract database to process
    db <- dbInUpdate[i]

    cat("\n")
    cat("Processing db:", db, "\n", "\n")

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
        #Disconnect from conIn
        RSQLite::dbDisconnect(conIn)
        next
      }

      #Determine number of rows in tableName
      query <- paste("SELECT COUNT(1) FROM", tableName)
      numRows <- RSQLite::dbGetQuery(conIn,
                                     query)[[1]]

      #If there are no rows (i.e. no data) in dbTable, skip to next iteration
      #of loop.
      if(numRows <= 0)
      {
        cat("No data found in",
            tableName,
            "\n")

        #Disconnect from conIn
        RSQLite::dbDisconnect(conIn)
        next
      }

      #Disconnect from conIn
      RSQLite::dbDisconnect(conIn)

      #If readChunks is FALSE, call addDbTable, otherwise call addDbRows.
      if(!readChunks)
      {
        addDbTable(db,
                   dbOut,
                   tableName,
                   ignoreCol,
                   addERU)
      }
      else
      {
        addDbRows(db,
                   dbOut,
                   tableName,
                   ignoreCol,
                   addERU,
                   rowsToRead,
                   numRows)
      }
    }

    #Print message indicating which db has been processed.
    cat("Finished processing db:",
        db,
        "\n")

    #Check if db should be deleted
    #Any files that are unzipped by dbCombine function will be deleted.
    #All other dbs will be deleted if deleteInput is T
    if(db %in% list.files(unzipDir,
                          pattern = ".db",
                          full.names = T,
                          recursive = T))
    {
      unlink(db,
             force = T)
    }
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

  #If deleteInput is TRUE, loop through dbIn and delete files.
  if(deleteInput)
  {
    for(i in 1:length(dbIn))
    {
      cat(paste("deleteInput is TRUE.",
                "Deleting database:", dbIn[i],
                "\n", "\n"))

      retCode <- unlink(dbIn[i])

      if(retCode != 0)
      {
        cat("Failed to delete:", dbIn[i], "\n", "\n")
      }
    }
  }

  #Before returning, delete unzipDir if it exists.
  if(file.exists(unzipDir))
  {
    retCode <- unlink(unzipDir,
                      recursive = T,
                      force = T)

    if(retCode != 0)
    {
      cat("Failed to delete temporary unzip directory:", unzipDir, "\n")
    }
  }

  return(cat("Data created!"))
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
    "STAND_ID",      "VARIANT",      "INV_YEAR",     "GROUPS",
    "ADDFILES",      "FVSKEYWORDS",  "GIS_LINK",     "PROJECT_NAME",
    "LATITUDE",      "LONGITUDE",    "REGION",       "FOREST",
    "DISTRICT",      "COMPARTMENT",  "LOCATION",     "ECOREGION",
    "PV_CODE",       "PV_REF_CODE",  "AGE",          "ASPECT",
    "SLOPE",         "ELEVATION",    "ELEVFT",       "BASAL_AREA_FACTOR",
    "INV_PLOT_SIZE", "BRK_DBH",      "NUM_PLOTS",    "NONSTK_PLOTS",
    "SAM_WT",        "STK_PCNT",     "DG_TRANS",     "DG_MEASURE",
    "HTG_TRANS",     "HTG_MEASURE",  "MORT_MEASURE", "MAX_BA",
    "MAX_SDI",       "SITE_SPECIES", "SITE_INDEX",   "MODEL_TYPE",
    "PHYSIO_REGION", "FOREST_TYPE",  "STATE",        "COUNTY",
    "FUEL_MODEL",    "FUEL_0_25_H",  "FUEL_25_1_H",  "FUEL_1_3_H",
    "FUEL_3_6_H",    "FUEL_6_12_H",  "FUEL_12_20_H", "FUEL_20_35_H",
    "FUEL_35_50_H",  "FUEL_GT_50_H",  "FUEL_0_25_S", "FUEL_25_1_S",
    "FUEL_1_3_S",    "FUEL_3_6_S",   "FUEL_6_12_S",  "FUEL_12_20_S",
    "FUEL_20_35_S",  "FUEL_35_50_S", "FUEL_GT_50_S", "FUEL_LITTER",
    "FUEL_DUFF",     "PHOTO_REF",    "PHOTO_CODE",   "PLOT_ID",
    "STANDPLOT_ID",  "TREE_ID",      "TREE_COUNT",   "HISTORY",
    "SPECIES",       "DIAMETER",     "DG",           "HT",
    "HTG",           "HTTOPK",       "CRRATIO",      "DAMAGE1",
    "SEVERITY1",     "DAMAGE2",      "SEVERITY2",    "DAMAGE3",
    "SEVERITY3",     "TREEVALUE",    "PRESCRIPTION", "TOPOCODE",
    "SITEPREP",      "DBH",          "STAND_CN",     "STANDPLOT_CN")

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
    "character", "character", "integer",   "character",
    "character", "character", "character", "character",
    "double",    "double",    "integer",   "integer",
    "integer",   "integer",   "integer",  "character",
    "character", "integer",   "integer",   "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "integer",   "integer",
    "double",    "double",    "integer",   "integer",
    "integer",   "integer",   "integer",   "double",
    "double",    "character", "double",    "integer",
    "integer",   "integer",   "integer",   "integer",
    "integer",   "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "integer",   "character", "double",
    "character", "double",    "double",    "double",
    "character", "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "character", "character")

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

    #Attempt to match column name with variable in fvsvars
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
        #Print message that column will be converted to character.
        if(verbose) cat(colname, "being converted to", datatype, "\n")
        data[,i]<-as.character(data[,i])
      }

      #Variable is a integer
      if(datatype == "integer")
      {
        #Print message that column will be converted to integer.
        if(verbose) cat(colname, "being converted to", datatype, "\n")
        data[,i]<-as.integer(data[,i])
      }

      #Variable is a double
      if(datatype == "double")
      {
        #Print message that column will be converted to double.
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

################################################################################
#Function: addDbTable
#
#This function sends an entire database table (expressed as dataframe) to
#output SQLite database.
#
#Arguments
#
#db:         Directory path and file name to input database.
#
#dbOut:      Directory path and file name to output SQLite database.
#
#tableName:  Name of database table in db being sent to dbOut.
#
#ignoreCols: Logical variable indicating if unrecognized fields in setDataTypes
#            function should be ignored. See setDataTypes function for more
#            details.
#
#addERU:     Logical variable indicating if ERU should be added to GROUPS column
#            of FVS_STANDINIT tables (FIA or regular FVS versions). See
#            dbCombine function for more details.
#
#Return value
#
#None
################################################################################

addDbTable<-function(db,
                     dbOut,
                     tableName,
                     ignoreCol,
                     addERU)
{

  #Connect to input database (db)
  conIn <- RSQLite::dbConnect(RSQLite::SQLite(),
                              db)

  #Read in the dbTable table (tableName)
  dbTable <- RSQLite::dbReadTable(conIn,
                                  name = tableName)

  #Capitalize column headers
  colnames(dbTable) <- toupper(colnames(dbTable))

  #Determine if ERU needs to be added to dbTable
  if(addERU & tableName %in% c("FVS_STANDINIT",
                               "FVS_PLOTINIT",
                               "FVS_STANDINIT_PLOT",
                               "FVS_STANDINIT_COND",
                               "FVS_PLOTINIT_PLOT"))
  {
    #Determine if PV_CODE and GROUPS fields exist in dbTable. If they don't,
    #then ERU will not be cross walked or included in output database.
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

      #Determine if the FVS_STANDINIT_PLOT  or FVS_STANDINIT_COND table is being
      #processed. If so, move values from PV_FIA_HABTYPCD1 to PV_CODE. The
      #FVS_PLOTINIT_PLOT table seems to have the correct values in PV_CODE
      #field in Region 3 FIA data.
      if(tableName %in% c("FVS_STANDINIT_PLOT",
                          "FVS_STANDINIT_COND") &
         "PV_FIA_HABTYPCD1" %in% colnames(dbTable))
      {
        dbTable["PV_CODE"] <- dbTable["PV_FIA_HABTYPCD1"]
      }

      #Cross walk PV_CODE to ERU
      dbTable$ERU<-mapply(pvConvert, dbTable$PV_CODE)

      #Add ERU grouping code
      dbTable$GROUPS<-mapply(addERUGroup,
                             dbTable$GROUPS,
                             dbTable$ERU)
    }
  }

  #Disconnect from conIn
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

    #Identify any fields in dbTable that are missing from the same data table
    #in conOut.
    dbFields <- RSQLite::dbListFields(conOut,
                                      name = tableName)

    #Missing fields
    missingFields <- names(dbTable)[! names(dbTable) %in% dbFields]

    #Loop through missingFields and add to database table in conOut
    if(length(missingFields) > 0)
    {
      cat("Fields missing from",
          tableName,
          "in",
          dbOut,
          "\n",
          missingFields, "\n")

      for(i in 1:length(missingFields))
      {
        #Extract field
        field <- missingFields[i]

        #Extract datatype of field
        colType <- typeof(dbTable[[field]])

        cat("colType:", colType, "\n")

        #Change colType to REAL, TEXT, or INTEGER depending on dataType.
        #REAL, CHARACTER, and TEXT are used since these datatypes are assumed
        #by default when sending data to database with dbWriteTable function.
        if(colType == "double")
        {
          dataType <- "REAL"
        }

        if(colType == 'character')
        {
          dataType <- "TEXT"
        }

        if(colType == 'integer')
        {
          dataType <- "INTEGER"
        }

        cat("Adding field:",
            field,
            paste0("(", dataType, ")"),
            "to table:",
            tableName,
            "\n")

        #Create query to alter table and add field in conout
        addField <-paste("ALTER TABLE",
                         tableName,
                         "ADD COLUMN",
                         field,
                         dataType)

        #Add field to conOut
        RSQLite::dbExecute(conOut, addField)

        cat("Field:",
            field,
            "added to table:",
            tableName,
            "\n")
      }
    }

    cat("Appending",
        tableName,
        "to",
        dbOut,
        "\n")

    #Append data to conOut
    RSQLite::dbWriteTable(conn = conOut,
                          name = tableName,
                          value = dbTable,
                          append = T)

    cat(tableName,
        "appended to",
        dbOut,
        "\n",
        "\n")
  }

  #Table will be created in conOut and data will then be written to the table.
  else
  {
    #Set datatypes of dbTable
    dbTable <- setDataTypes(dbTable,
                            ignoreCols = F)

    cat("Writing",
        tableName,
        "to",
        dbOut,
        "\n")

    #Create the dbTable in conOut and write information from dbTable to it.
    RSQLite::dbWriteTable(conn = conOut,
                          name = tableName,
                          value = dbTable,
                          overwrite = T)

    cat(tableName,
        "written to",
        dbOut,
        "\n",
        "\n")
  }

  #Delete dbTable
  rm(dbTable)

  #Disconnect from conOut
  RSQLite::dbDisconnect(conOut)

  return()
}

################################################################################
#Function: addDbRows
#
#This function incrementally sends portions of a database table (expressed as
#dataframe) to output SQLite database.
#
#Arguments
#
#db:         Directory path to input SQLite database.
#
#dbOut:      Directory path to output SQLite database.
#
#tableName:  Name of database table being sent from db to dbOut.
#
#ignoreCols: Logical variable indicating if unrecognized fields in setDataTypes
#            function should be ignored. See setDataTypes function for more
#            details.
#
#addERU:     Logical variable indicating if ERU should be added to GROUPS column
#            of FVS_STANDINIT tables (FIA or regular FVS versions). See
#            dbCombine function for more details.
#
#numToRead:  Number of rows to read in from database table at a time.
#
#numRows:    Number of rows in argument tableName.
#
#Return value
#
#None
################################################################################

addDbRows<-function(db,
                     dbOut,
                     tableName,
                     ignoreCol,
                     addERU,
                     numToRead,
                     numRows)
{

  #Variable to signify when read of data from tableName in db is complete
  doneReading <- F

  #Lower value of rows to read from
  lower <- 0

  #Upper value of rows to read from. Upper value is only used in messages sent
  #to console.
  upper <- 0

  #Variable used to keep track of number of rows that have been processed
  rowsDone <- 0

  #Variable to indicate whether first pass is complete.
  firstPass <- T

  while(!doneReading)
  {
    # If this is the first pass, set lower to 1 and upper to numToRead. Then
    #set firstPass to F.
    if(firstPass)
    {
      upper <- numToRead
      firstPass <- F
    }

    #If this is not the first pass then set lower to lower + numToRead and
    #upper to upper + NumToRead
    else
    {
      lower <- lower + numToRead
      upper <- upper + numToRead
    }

    #If upper is greater than or equal to numRows, set numToRead to
    #numRows - rowsDone and set doneReading to T. This will signify that
    #function is about to make the last read from db.
    if(upper >= numRows)
    {
      numToRead <- numRows - rowsDone
      upper <- numRows
      doneReading <- T
    }

    #Setup query for reading data
    query <- paste("SELECT * FROM",
                   tableName,
                   "LIMIT",
                   paste0(lower,",", numToRead))

    cat("Row query:",
        query,
        "\n")

    #Display what rows are being read from database table.
    cat("Reading rows:",
        lower + 1,
        "through",
        upper,
        "from",
        tableName, "\n")

    #Connect to db
    conIn <- RSQLite::dbConnect(RSQLite::SQLite(),
                                db)

    #Read the data
    dbTable <- RSQLite::dbGetQuery(conIn,
                                   query)

    #Disconnect from db
    RSQLite::dbDisconnect(conIn)

    #Determine number of rows read in current pass
    rowsRead <- nrow(dbTable)

    #Print number of rows in dbTable
    cat("Number of rows in read from database:",
        rowsRead,
        "\n")

    #Capitalize column headers
    colnames(dbTable) <- toupper(colnames(dbTable))

    #Determine if ERU needs to be added to dbTable
    if(addERU & tableName %in% c("FVS_STANDINIT",
                                 "FVS_PLOTINIT",
                                 "FVS_STANDINIT_PLOT",
                                 "FVS_STANDINIT_COND",
                                 "FVS_PLOTINIT_PLOT"))
    {
      #Determine if PV_CODE and GROUPS fields exist in dbTable. If they don't,
      #then ERU will not be cross walked or be included in output database.
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

        #Determine if the FVS_STANDINIT_PLOT  or FVS_STANDINIT_COND table is
        #being processed. If so, move values from PV_FIA_HABTYPCD1 to PV_CODE.
        #The FVS_PLOTINIT_PLOT table seem to have the correct values in PV_CODE
        #field in Region 3 FIA data.
        if(tableName %in% c("FVS_STANDINIT_PLOT",
                            "FVS_STANDINIT_COND") &
           "PV_FIA_HABTYPCD1" %in% colnames(dbTable))
        {
          dbTable["PV_CODE"] <- dbTable["PV_FIA_HABTYPCD1"]
        }

        #Cross walk PV_CODE to ERU
        dbTable$ERU<-mapply(pvConvert, dbTable$PV_CODE)

        #Add ERU grouping code
        dbTable$GROUPS<-mapply(addERUGroup,
                               dbTable$GROUPS,
                               dbTable$ERU)
      }
    }

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

      #Identify any fields in dbTable that are missing from the same data table
      #in conOut.
      dbFields <- RSQLite::dbListFields(conOut,
                                        name = tableName)

      #Missing fields
      missingFields <- names(dbTable)[! names(dbTable) %in% dbFields]

      #Loop through missingFields and add to database table in conOut
      if(length(missingFields) > 0)
      {
        cat("Fields missing from",
            tableName,
            "in",
            dbOut,
            "\n",
            missingFields, "\n")

        for(i in 1:length(missingFields))
        {
          #Extract field
          field <- missingFields[i]

          #Extract datatype of field
          colType <- typeof(dbTable[[field]])

          cat("colType:", colType, "\n")

          #Change colType to REAL, TEXT, or INTEGER depending on dataType.
          #REAL, CHARACTER, and TEXT are used since these datatypes are assumed
          #by default when sending data to database with dbWriteTable function.
          if(colType == "double")
          {
            dataType <- "REAL"
          }

          if(colType == 'character')
          {
            dataType <- "TEXT"
          }

          if(colType == 'integer')
          {
            dataType <- "INTEGER"
          }

          cat("Adding field:",
              field,
              paste0("(", dataType, ")"),
              "to table:",
              tableName,
              "\n")

          #Create query to add field to table in conout
          addField <-paste("ALTER TABLE",
                           tableName,
                           "ADD COLUMN",
                           field,
                           dataType)

          #Add field to conOut
          RSQLite::dbExecute(conOut, addField)

          cat("Field:",
              field,
              "added to table:",
              tableName,
              "\n")
        }
      }

      cat("Appending rows", lower + 1, "through", upper, "from",
          tableName,
          "to",
          dbOut,
          "\n")

      #Append data to conOut
      RSQLite::dbWriteTable(conn = conOut,
                            name = tableName,
                            value = dbTable,
                            append = T)

      cat("Rows", lower + 1, "through", upper, "from", tableName,
          "appended to",
          dbOut,
          "\n")
    }

    #Table will be created in conOut and data will then be written to the
    #table.
    else
    {
      #Set datatypes of dbTable
      dbTable <- setDataTypes(dbTable,
                              ignoreCols = F)

      cat("Writing rows", lower + 1, "through", upper, "from",
          tableName,
          "to",
          dbOut,
          "\n")

      #Create the dbTable in conOut and write information from dbTable to it.
      RSQLite::dbWriteTable(conn = conOut,
                            name = tableName,
                            value = dbTable,
                            overwrite = T)

      cat("Rows", lower + 1, "through", upper, "from", tableName,
          "written to",
          dbOut,
          "\n")
    }

    #Update rowsDone
    rowsDone <- rowsDone + rowsRead

    #Print number of rows processed
    cat("Number of rows processed:",
        rowsDone,
        "\n",
        "\n")

    #Delete dbTable
    rm(dbTable)

    #Disconnect from dbOut
    RSQLite::dbDisconnect(conOut)
  }

  return()
}
