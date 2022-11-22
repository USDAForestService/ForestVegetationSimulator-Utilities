################################################################################
#Function: canSizeCl
#
#This function calculates canopy size class for stand/plot in accordance with
#Region 3 rulesets from Vandendriesche, D., 2013. A Compendium of NFS
#Regional Vegetation Classification Algorithms. USDA Forest Service. Fort
#Collins, CO. (2013 pg.R3-3 - R3-4).
#
#Argument
#
#data:    Data frame containing tree records from a single stand or plot. Data
#         frame must contain a column corresponding to stand/plot ID, DBH,
#         expansion factor, and crown width for each tree record.
#
#stand:   Character string corresponding to name of column pertaining to stand
#         or plot ID associated with tree records in data argument. By default,
#         this value is set to "StandID".
#
#dbh:     Character string corresponding to name of column pertaining to DBH of
#         tree records in data argument. By default, this argument is set to
#         "DBH".
#
#expf:    Name of column in data argument corresponding to expansion factor of
#         tree records By default this argument is set to "TPA".
#
#crwidth: Character string corresponding to name of column pertaining to crown
#         width values of tree records in data argument. By default, this
#         argument is set to "CrWidth".
#
#TPA:     Trees per acre of stand/plot.
#
#CC:      Percent canopy cover corrected for overlap of stand/plot.
#
#type:    Indicator variable used to determine which type of diameter class to
#         return
#         1 - Midscale mapping (default and will be used if any value other than
#             2 or 3 is entered for type argument.)
#         2 - Timberland dominance type
#         3 - Woodland dominance type
#
#Possible return values when type = 1
#
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium and large tree canopy cover
#4 = very large tree canopy cover
#5 - giant tree canopy cover
#
#Possible return values when type = 2
#
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium tree canopy cover
#4 = large - giant tree cover
#
#Possible return values when type = 3
#
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium - giant tree cover
################################################################################

#'@export
canSizeCl<-function(data,
                    stand = "StandID",
                    dbh = "DBH",
                    expf = "TPA",
                    crwidth = "CrWidth",
                    TPA,
                    CC,
                    type=1,
                    debug = F)
{

  if(debug)
  {
    cat("In function canSizCls", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "crwidth:", crwidth, "\n",
        "expf:", expf, "\n", "\n")
  }

  #Check for missing columns in data
  missing <- c(stand, dbh, expf, crwidth) %in% colnames(data)

  #If name of columns provided in stand, dbh, expf, and crwidth are not found in
  #data warning message is issued and NA value is returned.
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Initialize named vector for storing CC by diameter class (1 - 5)
  ccVec<-c("1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0)

  #Statement used to avoid NOTE when stateTrans package is built.
  DC<-NULL

  #Calculate percent canopy cover for each tree record
  data$TREECC <- pi * (data[[crwidth]]/2)^2 *(data[[expf]]/43560) * 100

  #If plot CC is less than 10% and TPA less than 100, then cansizcl is 0
  if(CC < 10 & TPA < 100)
  {
    cansizcl = 0
    if(debug) cat("Total CC:", CC, " LT 10 and TPA:", TPA,
                  "LT 100.", "\n")
  }

  #If plot CC is less than 10% and TPA GE 100, then cansizcl is 1
  else if(CC < 10 & TPA >= 100)
  {
    cansizcl = 1
    if(debug) cat("Total CC:", CC, " LT 10 and TPA:", TPA,
                  "GE 100.", "\n")
  }

  #Else calculate cansizcl
  else
  {

    #Loop across data and sum canopy cover values for each class in ccVec
    for(i in 1:nrow(data))
    {
      #obtain DC for tree i
      dcIndex<-getCanSizeDC(data[[dbh]][i], type, debug)

      #Add percent canopy cover of tree i to appropriate diameter class index
      #in ccVec.
      ccVec[dcIndex]<- ccVec[dcIndex] + data$TREECC[i]
    }

    #Extract cansizcl associated with maximum CC
    cansizcl<-names(ccVec)[which.max(ccVec)]

    if(debug) cat("In canSizeCl function", "\n",
                  "Initial cansizcl", "\n",
                  "DC:", names(ccVec), "\n",
                  "CC:", ccVec, "\n",
                  "cansizcl:", cansizcl, "\n")

    #Timberland canopy size class adjustments
    if(type == 2)
    {
      #If canopy size class is 2 and the amount of canopy cover in classes 3 - 5
      #is greater than the canopy cover in class 2, then a canopy size class
      #selection is made from classes 3 - 5 (whichever has the most canopy
      #cover).
      if(cansizcl == "2" & sum(ccVec[3:5]) >= ccVec[2])
      {
        ccVec[1:2]<-0
        cansizcl<-names(ccVec)[which.max(ccVec)]
      }

      #If canopy size class is 1 and the amount of canopy cover in classes 2 - 5
      #is greater than the canopy cover in class 1, then a canopy size class
      #selection is made from classes 2 - 5 (whichever has the most canopy
      #cover).
      if(cansizcl == "1" & sum(ccVec[2:5]) >= ccVec[1])
      {
        ccVec[1]<-0
        cansizcl<-names(ccVec)[which.max(ccVec)]
      }
    }

    #Woodland canopy size class adjustments
    if(type == 3)
    {
      #If canopy size class is 1 and the amount of canopy cover in classes 2 - 5
      #is greater than the canopy cover in class 1, then a canopy size class
      #selection is made from classes 2 - 5 (whichever has the most canopy
      #cover).
      if(cansizcl == "1" & sum(ccVec[2:5]) >= ccVec[1])
      {
        ccVec[1]<-0
        cansizcl<-names(ccVec)[which.max(ccVec)]
      }
    }

    #Convert cansizcl to integer
    cansizcl<-as.integer(cansizcl)

    if(debug) cat("Final cansizcl", "\n",
                  "DC:", names(ccVec), "\n",
                  "CC:", ccVec, "\n",
                  "cansizcl:", cansizcl, "\n")
  }

  return(cansizcl)
}

################################################################################
#Function: getCanSizDC
#
#This function takes in a DBH value and a type argument and returns a diameter
#class that is used to calculate canopy size class in the canSizeCls function.
#
#Arguments
#
#DBH:   DBH value
#
#type:  Indicator variable used to determine which type of diameter class to
#       return
#       1 - Midscale mapping (default and will be used if any value other than
#           2 or 3 is entered for type argument.)
#       2 - Timberland dominance type
#       3 - Woodland dominance type
#
#debug:	Boolean variable used to specify if debug output should be printed to R
#       console. If value is TRUE, then debug output will printed to R console.
#
#Possible return values when type = 1
#
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium and large tree canopy cover
#4 = very large tree canopy cover
#5 - giant tree canopy cover
#
#Possible return values when type = 2
#
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium tree canopy cover
#4 = large - giant tree cover
#
#Possible return values when type = 3
#
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium - giant tree cover
################################################################################

getCanSizeDC<-function(DBH, type = 1, debug = F)
{
  #Diameter class criteria for type = 1 midscale mapping
  #(0, 5, 10, 20, 30)
  if(type == 1)
  {
    #DC1: 0 - 5"
    if(DBH >= 0 & DBH < 5) DC = 1

    #DC2: 5 - 10"
    else if(DBH >= 5 & DBH < 10) DC = 2

    #DC3: 10 - 20"
    else if(DBH >= 10 & DBH < 20) DC = 3

    #DC4: 20 - 30"
    else if(DBH >= 20 & DBH < 30) DC = 4

    #DC3: 30"+
    else DC = 5
  }

  #Diameter class criteria for type = 2 timberland
  #(0, 5, 10, 20)
  if(type == 2)
  {
    #DC1: 0 - 5"
    if(DBH >= 0 & DBH < 5) DC = 1

    #DC2: 5 - 10"
    else if(DBH >= 5 & DBH < 10) DC = 2

    #DC3: 10 - 20"
    else if(DBH >= 10 & DBH < 20) DC = 3

    #DC4: 20"+
    else DC = 4
  }

  #Diameter class criteria for type = 3 woodland
  #(0, 5, 10)
  if(type == 3)
  {
    #DC1: 0 - 5"
    if(DBH >= 0 & DBH < 5) DC = 1

    #DC2: 5 - 10"
    else if(DBH >= 5 & DBH < 10) DC = 2

    #DC3: 10" +
    else DC = 3
  }

  if(debug)
  {
    cat("In getCanSizeDC function", "\n")
    cat("DBH:", DBH,
                "DC:", DC,
                "TYPE:", type, "\n")
  }

  #Return DC
  return(DC)
}
