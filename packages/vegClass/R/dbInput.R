################################################################################
#Function: checkDBTables
#
#This function performs a series of checks to see if incoming FVS output
#database is ready for processing. A message is returned from the function
#which will determine if processing of input database in main (main.R) function
#is terminated or continued.
#
#Arguments
#
#con:        Connection to input .db
#
#variants:   Character vector of FVS variant codes associated with runs being
#            processed in main function (main.R).
#
#addCompute: Logical variable used to indicate if information in FVS_Compute
#            table should be included in output. Refer to main function in
#            main.R for further details.
#
#addPotFire: Logical variable used to indicate if information in FVS_Potfire
#            table should be included in output. Refer to main function in
#            main.R for further details.
#
#addFuels:   Logical variable used to indicate if information in FVS_Fuels
#            table should be included in output. Refer to main function in
#            main.R for further details.
#
#addCarbon:  Logical variable used to indicate if information in FVS_Carbon
#            table should be included in output. Refer to main function in
#            main.R for further details.
#
#Value
#
#Character string containing message.
################################################################################

checkDBTables<-function(con,
                        variants,
                        addCompute,
                        addPotFire,
                        addFuels,
                        addCarbon)
{
  #Flag variable used to determine if further checks should be made on incoming
  #database con.
  validDB<-T

  #Assign value of PASS to message
  message<-"PASS"

  #=============================================================================
  #If check if FVS_TreeList or FVS_TreeList_East is in con. If it is not, change
  #message and set validDB to FALSE.
  #=============================================================================

  if(validDB)
  {
    #Check for FVS_TreeList_East for any runs that assume Eastern variants
    if(is.element(T, variants %in% c("CS", "LS", "NE", "SN")))
    {
      #Check for FVS_TREELIST_EAST in con
      if(!RSQLite::dbExistsTable(con,
                                 "FVS_TREELIST_EAST"))
      {
        message<-paste("One or more runs use Eastern FVS variants (CS, LS, NE,",
                       "or SN) and FVS_TREELIST_EAST table was not found in",
                       "input database.",
                       "\n")

        #Assign value of F to validDB. Database (con) is not ready for
        #processing.
        validDB = F
      }
    }

    else
    {
      #Check for FVS_TREELIST in con
      if(!RSQLite::dbExistsTable(con,
                                 "FVS_TREELIST"))
      {
        message<-paste("One or more runs use Western FVS variants and",
                       "FVS_TREELIST table was not found in input database.",
                       "\n")

        #Assign value of F to validDB. Database (con) is not ready for
        #processing.
        validDB = F
      }
    }
  }

  #=============================================================================
  #If addCompute is TRUE, check for FVS_Compute table in con. If FVS_Compute
  #does not exist, change message and set validDB to FALSE.
  #=============================================================================

  if(validDB & addCompute)
  {
    #Check for FVS_TREELIST in con
    if(!RSQLite::dbExistsTable(con,
                               "FVS_COMPUTE"))
    {
      message<-paste("FVS_COMPUTE table not found in input database and",
                     "addCompute is TRUE. Either set the addCompute argument",
                     "to FALSE or include the FVS_COMPUTE table in input",
                     "database.",
                     "\n")

      #Assign value of F to validDB. Database (con) is not ready for
      #processing.
      validDB = F
    }
  }

  #=============================================================================
  #If addPotFIre is TRUE, check for FVS_Potfire or FVS_Potfire_East table in
  #con. If FVS_Potfire/FVS_Potfire_East does not exist, change message and set
  #validDB to FALSE.
  #=============================================================================

  if(validDB & addPotFire)
  {
    #Check for FVS_PotFire_East for any runs that assume CS or SN variant
    if(is.element(T, variants %in% c("CS", "SN")))
       {
         #Check for FVS_POTFIRE_EAST in con
         if(!RSQLite::dbExistsTable(con,
                                    "FVS_POTFIRE_EAST"))
         {
           message<-paste("FVS_POTFIRE_EAST table was not found in input",
                          "database and addPotFire is TRUE. Either set the",
                          "addPotFire argument to FALSE or include the",
                          "FVS_POTFIRE_EAST table in the input database.",
                          "\n")

           #Assign value of F to validDB. Database (con) is not ready for
           #processing.
           validDB = F
         }
    }

    #Check for FVS_PotFire for any runs that assume variants other than CS or
    #SN.
    else
    {
      #Check for FVS_POTFIRE in con
      if(!RSQLite::dbExistsTable(con,
                                 "FVS_POTFIRE"))
      {
        message<-paste("FVS_POTFIRE table was not found in input",
                       "database and addPotFire is TRUE. Either set the",
                       "addPotFire argument to FALSE or include the",
                       "FVS_POTFIRE table in the input database.",
                       "\n")

        #Assign value of F to validDB. Database (con) is not ready for
        #processing.
        validDB = F
      }
    }
  }

  #=============================================================================
  #If addFuels is TRUE, check for FVS_Fuels table in con. If FVS_Fuels does not
  #exist, change message and set validDB to FALSE.
  #=============================================================================

  if(validDB & addFuels)
  {
    #Check for FVS_TREELIST in con
    if(!RSQLite::dbExistsTable(con,
                               "FVS_FUELS"))
    {
      message<-paste("FVS_FUELS table not found in input database and",
                     "addFuels is TRUE. Either set the addFuels argument",
                     "to FALSE or include the FVS_FUELS table in input",
                     "database.",
                     "\n")

      #Assign value of F to validDB. Database (con) is not ready for
      #processing.
      validDB = F
    }
  }

  #=============================================================================
  #If addCarbon is TRUE, check for FVS_Carbon table in con. If FVS_Carbon does
  #not exist, change message and set validDB to FALSE.
  #=============================================================================

  if(validDB & addCarbon)
  {
    #Check for FVS_TREELIST in con
    if(!RSQLite::dbExistsTable(con,
                               "FVS_CARBON"))
    {
      message<-paste("FVS_CARBON table not found in input database and",
                     "addCarbon is TRUE. Either set the addCarbon argument",
                     "to FALSE or include the FVS_CARBON table in input",
                     "database.",
                     "\n")

      #Assign value of F to validDB. Database (con) is not ready for
      #processing.
      validDB = F
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
#read in data from the FVS_TreeList or FVS_TreeList_East table depending on the
#variant specified in the function call. The treeQuery function can be invoked
#outside of main function for testing purposes with the dbGetQuery function of
#the RSQLite R package.
#
#The query will read in the following fields from the FVS_Treelist table when
#variant argument is not CS, LS, NE, or SN:
#
#"CaseID", "StandID", "Year", "SpeciesPLANTS", "MortPA", "TPA", "DBH", "Ht",
#"CrWidth", "TCuFt", "MCuFt", "BdFt"
#
#The query will read in the following fields from the FVS_Treelist_East table
#when variant argument is CS, LS, NE, or SN:
#
#"CaseID", "StandID", "Year", "SpeciesPLANTS", "MortPA", "TPA", "DBH", "Ht",
#"CrWidth", "MCuFt", "SCuFt", "SBdFt"
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
            "TL.MortPA, TL.DBH, TL.Ht, TL.CrWidth, TL.MCuFt, TL.SCuFt,",
            "TL.SBdFt",
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
#read CaseID, StandID, Groups, and Viarnt from the FVS_Cases table for a FVS run
#title specified in the caseQuery function arguments. The caseQuery function can
#be invoked outside of main.R with the dbGetQuery function of the RSQLite R
#package.
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
    paste("SELECT *",
          "FROM FVS_Compute")

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
#to read in data from the FVS_Potfire or FVS_PotFire_East table depending on the
#variant specified in the function call. potFireQuery function can be invoked
#outside of main.R for testing purposes and can be used in the dbGetQuery
#function of the RSQLite R package.

#This query will read in the following fields from the FVS_PotFire table:

#"CaseID, "Year", "Surf_Flame_Sev", "Surf_Flame_Mod", "Tot_Flame_Sev",
#"Tot_Flame_Mod", "Fire_Type_Sev", "Fire_Type_Mod", "PTorch_Sev", "PTorch_Mod",
#"Torch_Index", "Crown_Index", "Canopy_Ht", "Canopy_Density", "Mortality_BA_Sev",
#"Mortality_BA_Mod", "Mortality_VOL_Sev", "Mortality_VOL_Mod", "Pot_Smoke_Sev",
#"Pot_Smoke_Mod"

#This query will read in the following fields from the FVS_PotFire_East table:

#"CaseID, "Year", "Flame_Len_Sev", "Flame_Len_Mod", "Canopy_Ht",
#"Canopy_Density", "Mortality_BA_Sev", "Mortality_BA_Mod", "Mortality_VOL_Sev",
#"Mortality_VOL_Mod", "Pot_Smoke_Sev", "Pot_Smoke_Mod"
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
      paste("SELECT PF.CaseID, PF.Year, PF.Flame_Len_Sev, PF.Flame_Len_Mod,",
            "PF.Canopy_Ht, PF.Canopy_Density, PF.Mortality_BA_Sev,",
            "PF.Mortality_BA_Mod, PF.Mortality_VOL_Sev, PF.Mortality_VOL_Mod,",
            "PF.Pot_Smoke_Sev, PF.Pot_Smoke_Mod",
            "FROM FVS_Potfire_East PF")
  }

  #All other variants
  else
  {
    query<-
      paste("SELECT PF.CaseID, PF.Year, PF.Surf_Flame_Sev, PF.Surf_Flame_Mod,",
            "PF.Tot_Flame_Sev, PF.Tot_Flame_Mod, PF.Fire_Type_Sev,",
            "PF.Fire_Type_Mod, PF.PTorch_Sev, PF.PTorch_Mod, PF.Torch_Index,",
            "PF.Crown_Index, PF.Canopy_Ht, PF.Canopy_Density,",
            "PF.Mortality_BA_Sev, PF.Mortality_BA_Mod, PF.Mortality_VOL_Sev,",
            "PF.Mortality_VOL_Mod, PF.Pot_Smoke_Sev, PF.Pot_Smoke_Mod",
            "FROM FVS_Potfire PF")
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
#Function: getPotFireVars
#
#This function returns the variable names that are included in the FVS_POTFIRE
#database table except for StandID. This function is used to order the columns
#of the FVS_POTFIRE_EAST table so it can be combined with output from the
#standard FVS_POTFIRE_TABLE.
#
#Arguments
#
#None
#
#Value
#
#Character vector containing FVS_POTFIRE variable names
################################################################################

getPotFireVars <- function()
{
  potfireVars<-c("CASEID",
                 "YEAR",
                 "SURF_FLAME_SEV",
                 "SURF_FLAME_MOD",
                 "TOT_FLAME_SEV",
                 "TOT_FLAME_MOD",
                 "FIRE_TYPE_SEV",
                 "FIRE_TYPE_MOD",
                 "PTORCH_SEV",
                 "PTORCH_MOD",
                 "TORCH_INDEX",
                 "CROWN_INDEX",
                 "CANOPY_HT",
                 "CANOPY_DENSITY",
                 "MORTALITY_BA_SEV",
                 "MORTALITY_BA_MOD",
                 "MORTALITY_VOL_SEV",
                 "MORTALITY_VOL_MOD",
                 "POT_SMOKE_SEV",
                 "POT_SMOKE_MOD")

  return(potfireVars)
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
    paste("SELECT *",
          "FROM FVS_Fuels")

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
    paste("SELECT *",
          "FROM FVS_Carbon")

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

