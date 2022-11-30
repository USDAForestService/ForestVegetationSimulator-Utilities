################################################################################
#Function: validateDBInputs
#
#This function performs a series of checks to see if incoming FVS output
#database is ready for processing. A message is returned from the function
#which will determine if processing of input database in main (main.R) function
#is terminated or continued.
#
#Arguments
#
#con:      Connection to input .db
#
#runTitle: Name of FVS run title(s) that will be searched for in FVS_Cases
#          table.
#
#allRuns:  Logical variable used to signify that all runs from FVS output
#          database are being processed. See main function in main.R for
#          further details.
#
#Value
#
#Character string containing message.
################################################################################

validateDBInputs<-function(con,
                           runTitle,
                           allRuns)
{
  #Flag variable used to determine if further checks should be made on incoming
  #database con.
  validDB<-T

  #Assign value of PASS to message
  message<-"PASS"

  #=============================================================================
  #If FVS_Cases is missing then message is changed and validDB is set to F.
  #=============================================================================

  if(validDB)
  {
      if(is.element(F, c("FVS_Cases") %in%
                    RSQLite::dbListTables(con)))
      {
        message<-paste("FVS_Cases table not found in input database.")

        #Assign value of F to validDB. Database (con) is not ready for
        #processing.
        validDB = F
      }
  }

  #=============================================================================
  #Test if run titles are in FVS_Cases table. If any are not found, message is
  #changed and validDB is set to F.
  #=============================================================================

  if(validDB)
  {
    if(!allRuns)
    {
      #Check if runs from runTitle are found in FVS_Cases table
      runsFound<- runTitle %in% toupper(unique(RSQLite::dbGetQuery(con,
      "SELECT RunTitle FROM FVS_Cases")[,1]))

      #If any runs are not found, then report them in error message
      if(F %in% runsFound)
      {
        #Determine missing runs
        missingRuns<-runTitle[runsFound == F]

        #Paste missing runs together separated by comma and space
        missingRuns<-paste(missingRuns, collapse = ", ")

        message<-(paste("Run titles:",paste0("'",missingRuns,"'", collapse = ""),
                        "not found in input database. Please ensure all run",
                         "titles are spelled correctly."))

        #Assign value of F to validDB. Database (con) is not ready for
        #processing.
        validDB = F
      }
    }
  }

  #==========================================================================
  #Database has passed checks. Return value of 'PASS' as defined previously.
  #==========================================================================

  return(message)
}

################################################################################
#Function: treeQuery
#
#This function builds and returns a string of SQL statements that can be used to
#read in data from the FVS_TreeList or FVS_TreeList_East table. The query will
#read in CaseID, StandID, Year, SpeciesPLANTS, MortPA, TPA, DBH, Ht, CrWidth,
#and volume variables from the FVS_TreeList for the tree records associated with
#Case ID values specified in the treeQuery function arguments. The treeQuery
#function can be invoked outside of main function for testing purposes with the
#dbGetQuery function of the RSQLite R package.
#
#Arguments
#
#cases:	  Character vector of case IDs.
#
#variant: Two character variant code. This code is used to determine what tree
#         list databaes table data should be read from (FVS_TreeList or
#         FVS_TreeList_East).Valid variant codes: "AK", "BM", "CA", "CI", "CR",
#         "CS", "EC", "EM",  "IE", "KT", "LS", "NC", "NE", "PN", "SN", "SO",
#         "TT", "UT", "WC", "WS", "OC", "OP"
#
#Value
#
#Character string of SQL statements.
################################################################################

#'@export
treeQuery<-function(cases, variant)
{
  variant <- toupper(variant)

  #Define SELECT portion of tree list query

  #Eastern variants
  if(variant %in% c("CS", "NE", "LS", "SN"))
  {
    query<-
      paste("SELECT TL.CaseID, TL.StandID, TL.Year, TL.SpeciesPLANTS, TL.TPA,",
            "TL.MortPA, TL.DBH, TL.Ht, TL.CrWidth, TL.MCuFt, TL.SCuFt,
             TL.SBdFt",
            "FROM FVS_TreeList_East TL")
  }

  #All other variants
  else
  {
    query<-
      paste("SELECT TL.CaseID, TL.StandID, TL.Year, TL.SpeciesPLANTS, TL.TPA,",
            "TL.MortPA, TL.DBH, TL.Ht, TL.CrWidth, TL.TCuFt, TL.MCuFt, TL.BdFt",
            "FROM FVS_TreeList TL")
  }

  #Add stand query as long as length of cases is at least 1.
  if(length(cases) >= 1)
  {
    #Add quotes to stand and commas to cases
    cases<-paste0("'",cases,"'", ",")

    #Collapse cases into a single string
    cases<-paste(cases, collapse = "")

    #Remove last comma from cases
    cases<-substr(cases,1, nchar(cases)-1)

    #Add parentheses around cases
    cases<-paste0("(", cases, ")")

    #Create WHERE clause with cases
    standQuery<-paste0("WHERE TL.CaseID IN", cases)

    #Add standQuery to query
    query<-paste(query, standQuery)
  }

  return(query)
}

################################################################################
#Function: caseQuery
#
#This function builds and returns a string of SQL statements that is used to
#read CaseID, StandID, and Groups from the FVS_Cases table for a FVS run title
#specified in the caseQuery function arguments. The caseQuery function can be
#invoked outside of main.R with the dbGetQuery function of the RSQLite R package.
#
#Arguments
#
#runTitle: Character string pertaining to FVS run title.
#
#Value
#
#Character string of SQL statements.
################################################################################

#'@export
caseQuery <- function(runTitle)
{
  query<- paste("SELECT FVS_Cases.StandID, FVS_Cases.CaseID, FVS_Cases.Groups,",
                "FVS_Cases.Variant",
                "FROM FVS_Cases",
                "WHERE RunTitle LIKE", paste0("'%",runTitle,"%'"))
  return(query)
}

################################################################################
#Function: computeQuery
#
#This function builds and returns a string of SQL statements that can be used
#to read in data from the FVS_Compute table. The query will read in all fields
#from the FVS_Compute table. The computeQuery function can be invoked outside of
#main.R for testing purposes and can be used in the dbGetQuery function of the
#RSQLite R package.
#
#Arguments
#
#cases:	 Character vector of cases IDs.
#
#Value
#
#Character string of SQL statements.
################################################################################

#'@export
computeQuery<-function(cases)
{
  #Define SELECT portion of tree list query
  query<-
    paste("SELECT *
          FROM FVS_Compute")

  #Add stand query as long as length of cases is at least 1.
  if(length(cases) >= 1)
  {
    #Add quotes to stand and commas to cases
    cases<-paste0("'",cases,"'", ",")

    #Collapse cases into a single string
    cases<-paste(cases, collapse = "")

    #Remove last comma from cases
    cases<-substr(cases,1, nchar(cases)-1)

    #Add parentheses around cases
    cases<-paste0("(", cases, ")")

    #Create WHERE clause with cases
    caseQuery<-paste0("WHERE FVS_Compute.CaseID IN", cases)

    #Add standQuery to query
    query<-paste(query, caseQuery)
  }

  return(query)
}

################################################################################
#Function: potFireQuery
#
#This function builds and returns a string of SQL statements that can be used
#to read in data from the FVS_Potfire or FVS_PotFire_East table. This query will
#read in all fields. from the FVS_PotFire or FVS_PotFire_East table. The
#potFireQuery function can be invoked outside of main.R for testing purposes and
#can be used in the dbGetQuery function of the RSQLite R package.
#
#Arguments
#
#cases:	  Character vector of cases IDs.
#
#variant: Two character variant code. This code is used to determine what
#         potential fire database table data should be read from (FVS_Potfire
#         or FVS_Potfire_East). Valid variant codes: "AK", "BM", "CA", "CI",
#         "CR", "CS", "EC", "EM",  "IE", "KT", "LS", "NC", "NE", "PN", "SN",
#         "SO", "TT", "UT", "WC", "WS", "OC", "OP"
#
#Value
#
#Character string of SQL statements.
################################################################################

#'@export
potFireQuery<-function(cases, variant)
{
  variant <- toupper(variant)

  #Define SELECT portion of potfire query

  #CS and SN variants
  if(variant == "CS" | variant == "SN")
  {
    query<-
      paste("SELECT *
          FROM FVS_Potfire_East PF")
  }

  #All other variants
  else
  {
    query<-
      paste("SELECT *
          FROM FVS_Potfire PF")
  }

  #Add stand query as long as length of cases is at least 1.
  if(length(cases) >= 1)
  {
    #Add quotes to stand and commas to cases
    cases<-paste0("'",cases,"'", ",")

    #Collapse cases into a single string
    cases<-paste(cases, collapse = "")

    #Remove last comma from cases
    cases<-substr(cases,1, nchar(cases)-1)

    #Add parentheses around cases
    cases<-paste0("(", cases, ")")

    #Create WHERE clause with cases
    caseQuery<-paste0("WHERE PF.CaseID IN", cases)

    #Add standQuery to query
    query<-paste(query, caseQuery)
  }

  return(query)
}

################################################################################
#Function: fuelsQuery
#
#This function builds and returns a string of SQL statements that can be used
#to read in data from the FVS_Fuels table. This query will read in all fields.
#from the FVS_Fuels table. The fuelsQuery function can be invoked outside
#of main.R for testing purposes and can be used in the dbGetQuery function of
#the RSQLite R package.
#
#Arguments
#
#cases:	 Character vector of cases IDs.
#
#Value
#
#Character string of SQL statements.
################################################################################

#'@export
fuelsQuery<-function(cases)
{
  #Define SELECT portion of fuels query
  query<-
    paste("SELECT *
          FROM FVS_Fuels")

  #Add stand query as long as length of cases is at least 1.
  if(length(cases) >= 1)
  {
    #Add quotes to stand and commas to cases
    cases<-paste0("'",cases,"'", ",")

    #Collapse cases into a single string
    cases<-paste(cases, collapse = "")

    #Remove last comma from cases
    cases<-substr(cases,1, nchar(cases)-1)

    #Add parentheses around cases
    cases<-paste0("(", cases, ")")

    #Create WHERE clause with cases
    caseQuery<-paste0("WHERE FVS_Fuels.CaseID IN", cases)

    #Add standQuery to query
    query<-paste(query, caseQuery)
  }

  return(query)
}

################################################################################
#Function: carbonQuery
#
#This function builds and returns a string of SQL statements that can be used
#to read in data from the FVS_Carbon table. This query will read in all fields.
#from the FVS_Carbon table. The carbonQuery function can be invoked outside
#of main.R for testing purposes and can be used in the dbGetQuery function of
#the RSQLite R package.
#
#Arguments
#
#cases:	 Character vector of cases IDs.
#
#Value
#
#Character string of SQL statements.
################################################################################

#'@export
carbonQuery<-function(cases)
{
  #Define SELECT portion of carbon query
  query<-
    paste("SELECT *
          FROM FVS_Carbon")

  #Add stand query as long as length of cases is at least 1.
  if(length(cases) >= 1)
  {
    #Add quotes to stand and commas to cases
    cases<-paste0("'",cases,"'", ",")

    #Collapse cases into a single string
    cases<-paste(cases, collapse = "")

    #Remove last comma from cases
    cases<-substr(cases,1, nchar(cases)-1)

    #Add parentheses around cases
    cases<-paste0("(", cases, ")")

    #Create WHERE clause with cases
    caseQuery<-paste0("WHERE FVS_Carbon.CaseID IN", cases)

    #Add standQuery to query
    query<-paste(query, caseQuery)
  }

  return(query)
}

################################################################################
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
################################################################################

getGroup<-function(groups, label)
{
  #Extract targetGroup if present in groups
  if(grepl(label, groups))
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
  }

  #Else assign All to targetGroup
  else
  {
    targetGroup<-"ALL"
  }

  #Return targetGroup
  return(targetGroup)
}

