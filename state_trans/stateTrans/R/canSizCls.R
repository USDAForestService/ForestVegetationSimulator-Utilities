#############################################################################
#Function: getDiaValues
#
#This function takes in a type argument and returns a list containing a vector
#of diameters and a vector of diameter class values based on R3 midscale
#mapping, timber dominance type, or woodland  dominance types criteria. These
#different diameter classes are described in NFS_Reg_Veg_Class.pdf.
#
#Arguments
#
#type: Indicator variable used to determine which diameters and diameter class
#      values to return.
#      1 - Midscale mapping (default and will be used if any value other than
#          2 or 3 is entered for type argument.)
#      2 - Timberland dominance type
#      3 - Woodland dominance type
#
#debug:	     Boolean variable used to specify if debug output should be
#            printed to R console. If value is TRUE, then debug output will
#            printed to R console.
#
#Return values when type = 1
#
#diameters = c(0, 5, 10, 20, 30)
#diameter class values:
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium and large tree canopy cover
#4 = very large tree canopy cover
#5 - giant tree canopy cover
#
#Return values when type = 2
#
#diameters = c(0, 5, 10, 20)
#diameter class values:
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium tree canopy cover
#4 = large - giant tree cover
#
#Return values when type = 3
#
#diameters = c(0, 5, 10)
#diameter class values:
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium - giant tree cover
#############################################################################

getDiaValues<-function(type = 1, debug = F)
{
  #Define lower and upper DBH limits for midscale mapping
  #Lower diameter limit
  diameters<-c(0, 5, 10, 20, 30)

  #Diameter class values
  dcls<-seq(from = 1, to = 5, by = 1)

  #Define lower and upper DBH limits for timber dominance types
  if(type == 2)
  {
    #Lower diameter limit
    diameters<-c(0, 5, 10, 20)

    #Diameter class values
    dcls<-seq(from = 1, to = 4, by = 1)
  }

  #Define lower and upper DBH limits for woodland dominance types
  if(type == 3)
  {
    #Lower diameter limit
    diameters<-c(0, 5, 10)

    #Diameter class values
    dcls<-seq(from = 1, to = 3, by = 1)
  }

  if(debug) cat("In getDiaValues function", "\n",
                "Type:", type, "\n",
                "diameters:", diameters, "\n",
                "dcls:", dcls, "\n")

  #Collect diameters and dcls in a list and return
  results<-list(diameters, dcls)

  return(results)
}

#############################################################################
#Function: findCategory
#
#This function returns a category or classification from input argument
#validOutputs based on the value of input argument x. The following logic
#is used to determine a value from validOutputs using x and/or inputValues
#arguments:
#
#1) If inputValues has only one value, then first value in outputValues is
#   returned.
#
#2) If x is less than first item in inputValues, then value specified in
#   invalidReturn arugment is returned. If useLowerBound is true, then
#   value in the first item of outputValues is returned.
#
#3) If x is greater than last value in inputValues, the last item in
#   validOutputs is returned.
#
#4) When 1 - 3 are not true, then x is compared against all i-th and i-th+1
#   values in inputValues. If x is a value GE to ith value and LT i-th + 1
#   value in inputValues, then ith value in outputValues is returned.
#
#Arguments
#
#x:             Incoming value to evaluate. Must be numeric and not NA.
#
#inputValues:   Values to compare x against. This argument has to be the same
#               length as outputValues.
#
#outputValues:  Classification values to return containing depending on value
#               x. This argument has to be the same length as inputValues.
#
#invalidReturn: Default value to return when:
#               1) x is not a valid value
#               2) InputValues is empty, a character vector, or contains any
#                  NA values.
#               3) Length of inputValues does not equal outputValues.
#
#Return value
#
#Numeric (or integer) category or classification
#############################################################################

findCategory<-function(x, inputValues = 0, outputValues = 0, invalidReturn = 0,
                       useLowerBound = F)
{

  #Initialize return category (cat) to invalidReturn
  cat = invalidReturn

  #If x is character or NA, return.
  if(is.character(x) | is.na(x))
  {
    return(cat)
  }

  #If InputValues is less than length 1, a character vector, or contains any NA
  #values, then return.
  if(length(inputValues) < 1 | is.character(inputValues) | NA %in% inputValues)
  {
    return(cat)
  }

  #If inputValues and outputValues are not the same length, then return.
  if(length(inputValues) != length(outputValues))
  {
    return(cat)
  }

  #===========================
  #Look for category
  #===========================

  #If x is less than first item in inputValues then cat is assigned invalidReturn
  #If useLowerBound == T then cat is assigned the first item in outputValues.
  if(x < inputValues[1])
  {
    cat = invalidReturn
    if(useLowerBound == T) cat = outputValues[1]
  }

  #Test if x is greater than last value in outputValues
  else if (x >= inputValues[length(inputValues)])
  {
    cat = outputValues[length(outputValues)]
  }

  #Else, loop across length of inputValues vector and determine diameter class
  #for x value.
  else
  {
    done = F
    i = 1
    while(done == F)
    {

      #If at the end of inputValues vector, then done becomes true. This is
      #a precautionary condition if a valid output is not found for x during
      #search. This condition should rarely, if ever, evaluate as true...
      if(i == length(inputValues))
      {
        done = T
      }

      #If x is between the ith and ith+ 1 items in inputValues, then cat is
      #assigned the ith value in outputValues vector and done becomes true.
      else if(x >= inputValues[i] & x < inputValues[i + 1])
      {
        cat = outputValues[i]
        done = T
      }

      #Else keep searching
      else
      {
        i = i + 1
      }
    }
  }

  return(cat)
}

#############################################################################
#Function: getCanSizDC
#
#This function takes in a vector of DBH values and a type argument and returns
#a vector of diameter classes that are used to calculate canopy size class
#in the canSizeCls function.
#
#Arguments
#
#DBH:   Vector containing DBH values.
#
#type:  Indicator variable used to determine which type of diameter class to
#       return
#       1 - Midscale mapping (default and will be used if any value other than
#          2 or 3 is entered for type argument.)
#       2 - Timberland dominance type
#       3 - Woodland dominance type
#
#debug:	Boolean variable used to specify if debug output should be
#       printed to R console. If value is TRUE, then debug output will
#       printed to R console.
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
#############################################################################

getCanSizeDC<-function(DBH, type = 1, debug = F)
{
  #Get diameters and DC values
  values<-getDiaValues(type, debug)

  #Initialize vector the same size as DBH for storing DC values
  DC<-vector(length = length(DBH))

  if(debug) cat("In getCanSizeDC function", "\n")

  #Loop across DBH vector and determine
  for(i in 1:length(DBH))
  {
    dcls<-findCategory(DBH[i], values[[1]], values[[2]])
    DC[i]<-dcls
    if(debug) cat("DBH:", DBH[i], "DC:", DC[i],"\n")
  }

  #Return DC
  return(DC)
}

#############################################################################
#Function: calcCanSizCl
#
#This function takes in a tree-level dataframe and returns a canopy size
#class for the inventory plot that the tree records reside on.
#
#Argument
#
#dat:     Tree level dataframe that contains DBH and canopy percent cover of each
#         tree record (TREECC).
#
#totalCC: Percent canopy cover of stand.
#
#type:    Indicator variable used to determine which type of diameter class to
#         return
#         1 - Midscale mapping (default and will be used if any value other than
#         2 or 3 is entered for type argument.)
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
#############################################################################

#'@export
canSizeCl<-function(dat, totalCC, type=1, debug = F)
{
  #Print stand
  if(debug) cat("Stand:", unique(dat$StandID), "\n")

  #Initialize named vector for storing CC by diameter class
  ccVec<-c("1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0)

  #Statement used to avoid NOTE when stateTrans package is built.
  DC<-TREECC<-NULL

  #If plot CC is less than 10% then cansizcl is 0
  if(correctCC(totalCC) < 10)
  {
    cansizcl = 0
    if(debug) cat("Total CC:", correctCC(totalCC), " less than 10.", "\n")
  }

  #Else calculate cansizcl
  else
  {
    #Determine diameter class for each tree record
    dat$DC<-getCanSizeDC(dat$DBH, type, debug)

    #Loop across dat and sum canopy cover values for each class in ccVec
    for(i in 1:nrow(dat))
    {
      #obtain DC for tree i
      dcIndex<-dat$DC[i]

      #Sum CC for DC in ccVec
      ccVec[dcIndex]<- ccVec[dcIndex] + dat$TREECC[i]
    }

    #Extract cansizcl associated with maximum CC
    cansizcl<-names(ccVec)[which.max(ccVec)]

    if(debug) cat("In canSizeCl function", "\n",
                  "Initial cansizcl", "\n",
                  "DC:", names(ccVec), "\n",
                  "CC:", ccVec, "\n",
                  "cansizcl:", cansizcl, "\n")

    #Timber canopy size class adjustments
    if(type == 2)
    {
      if(cansizcl == "2" & sum(ccVec[3:5]) >= ccVec[2])
      {
        ccVec[1:2]<-0
        cansizcl<-names(ccVec)[which.max(ccVec)]
      }

      if(cansizcl == "1" & sum(ccVec[2:5]) >= ccVec[1])
      {
        ccVec[1]<-0
        cansizcl<-names(ccVec)[which.max(ccVec)]
      }
    }

    #Woodland size class adjustments
    if(type == 3)
    {
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

#############################################################################
#Function: correctCC
#
#This function takes in an uncorrected percent canopy cover value and returns
#a corrected value using the relationship described on page 2 of Crookston,
#Nicholas L.; Stage, Albert R. 1999. Percent canopy cover and stand structure
#statistics from the Forest Vegetation Simulator. Gen. Tech. Rep. RMRS-GTR-24.
#Ogden, UT: U. S. Department of Agriculture, Forest Service, Rocky Mountain
#Research Station. 11 p.
#
#Argument
#
#CC: Uncorrected CC value
#
#Return value
#
#Corrected CC value.
#############################################################################

correctCC<-function(CC)
{
  corCC = 100 * (1 - exp ( - 0.01* CC))
  return(corCC)
}

#############################################################################
#Function: roundCC
#
#This function takes in an uncorrected percent canopy cover value and returns
#a rounded value using the criteria described on page R3-3 of NFS Regional
#Vegetation Classification Algorithms.
#
#Argument
#
#CC:          Uncorrected CC value
#
#ccThreshold: Percent canopy cover value that determines how argument CC is
#             rounded. By Default this argument is set to 10.
#
#Return value
#
#Rounded CC value.
#############################################################################

roundCC<-function(CC, ccThreshold = 10)
{
  #Round to nearest 5
  if(CC > 10)
  {
    ccRound = round(CC / 5) * 5
  }

  #Round to nearest 1
  else
  {
    ccRound = round(CC / 1) * 1
  }

  return(ccRound)
}

