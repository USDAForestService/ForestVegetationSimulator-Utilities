#############################################################################
#Function: validateDBInputs
#
#This function performs a series of checks to see if incoming FVS output
#database is ready for processing. A message is returned from the function
#which will determine if processing of input database in main (main.R) function
#is terminated or continued.
#
#Arguments
#
#con:       Connection to input .db
#
#runTitle:  Name of FVS run title(s) that will be searched for in FVS_Cases
#           table.
#
#Value
#
#Character string containing message.
#############################################################################

validateDBInputs<-function(con, runTitle)
{
  #Flag variable used to determine if further checks should be made on incoming
  #database con.
  validDB<-T

  #Assign value of PASS to message
  message<-"PASS"

  #==========================================================================
  #If FVS_TreeList, FVS_Cases, or FVS_Summary 2 is missing then message is
  #changed and validDB is set to F.
  #==========================================================================

  if(validDB)
  {
    if(is.element(F, c("FVS_TreeList", "FVS_Cases", "FVS_Summary2") %in%
                  RSQLite::dbListTables(con)))
    {
      message<-paste("One of the following tables was not found in incoming database:",
    "FVS_TreeList, FVS_Cases, and FVS_Summary2.")

      #Assign value of F to validDB. Database (con) is not ready for prcoessing.
      validDB = F
    }
  }

  #==========================================================================
  #If runTitle is not NA then make sure the input run title is found in
  #FVS_Cases table. If it is not found, message is changed and validDB is set
  #to F.
  #==========================================================================

  if(validDB)
  {

    #Check if runs from runTitle are found in FVS_Cases table
    runsFound<- runTitle %in% toupper(unique(RSQLite::dbGetQuery(con, "SELECT RunTitle FROM
                                                FVS_Cases")[,1]))

    #If any runs are not found, then report them in error message
    if(F %in% runsFound)
    {
      #Determine missing runs
      missingRuns<-runTitle[runsFound == F]

      #Paste missing runs together separated by comma and space
      missingRuns<-paste(missingRuns, collapse = ", ")

      message<-(paste("Run titles:",paste0("'",missingRuns,"'", collapse = ""),"not found in",
      "input database. Please ensure all run titles are spelled correctly."))

      #Assign value of F to validDB. Database (con) is not ready for prcoessing.
      validDB = F
    }
  }

  #==========================================================================
  #Database has passed checks. Return value of 'PASS' as defined previously.
  #==========================================================================

  return(message)
}

#############################################################################
#Function: buildQuery
#
#This function builds and returns a string of SQL statements that can be used
#to read in data from the FVS_TreeList, FVS_Summary2, and FVS_Cases tables of
#an FVS output database in main function (main.R). The buildQuery function
#can be invoked outside of main.R for testing purposes and can be used in the
#dbGetQuery function of the RSQLite R package.
#
#Arguments
#
#stands:	 Character vector of stand IDs.
#
#runTitle: Character string pertaining to FVS run title.
#
#Value
#
#Character string of SQL statements.
#############################################################################

#'@export
buildQuery<-function(stands, runTitle)
{
  #Define portion of query that will always be used.
  query<-
    paste("SELECT TL.CaseID, TL.StandID, TL.Year,
          TL.SpeciesPLANTS, TL.TPA, TL.DBH, TL.CrWidth,
          FVS_Cases.RunTitle, FVS_Cases.Groups,
          FVS_Summary2.Age
          FROM FVS_TreeList TL
          INNER JOIN FVS_Cases
          ON TL.CaseID = FVS_Cases.CaseID
          INNER JOIN FVS_Summary2
          ON TL.CaseID = FVS_Summary2.CaseID AND
          TL.Year = FVS_Summary2.Year")

  #Add stand query as long as length of stands is at least 1.
  if(length(stands) >= 1)
  {
    #Add quotes to stand and commas to stands
    stands<-paste0("'",stands,"'", ",")

    #Collapse stands into a single string
    stands<-paste(stands, collapse = "")

    #Remove last comma from stands
    stands<-substr(stands,1, nchar(stands)-1)

    #Add parantheses around stands
    stands<-paste0("(", stands, ")")

    #Create WHERE clause with stands
    standQuery<-paste0("WHERE TL.StandID IN", stands)

    #Add standQuery to query
    query<-paste(query, standQuery)
  }

  #If runtitle is not NA, then a WHERE clause with runTitle will be added to
  #query
  if(!is.na(runTitle))
  {
    runTitleString<-paste0("'%",runTitle,"%'")
    query<-paste(query, paste("AND RunTitle LIKE", runTitleString))
  }

  return(query)
}

#############################################################################
#Function: getGroup
#
#This function takes in a string of FVS group labels and returns the group
#proceeding the input string identifier as specified in label argument.
#
#Arguments
#
#groups: String containing group labels as formatted in the FVS_Cases table.
#
#label:  Label that is used to extract the group of interest. For instance,
#        the label "ERU=" would be used to extract "MCW" from the group
#        code, ERU=MCW. If label is not found in group, then first group in
#        groups argument is returned.
#############################################################################

getGroup<-function(groups, label)
{
  #Paste comma to end of groups
  groups<-paste0(groups,",")
  # cat(paste(groups, "\n"))

  #Remove all characters through label
  groups<-sub(paste0("(.*)",label,""),"",groups)
  # cat(paste(groups, "\n"))

  #Remove all characters after first comma
  groups<-sub(",(.*)","",groups)
  # cat(paste(groups, "\n"))

  #Assign groups to targetGroup
  targetGroup<-groups

  #Return targetGroup
  return(targetGroup)
}

