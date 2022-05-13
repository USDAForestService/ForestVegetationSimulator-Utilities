#############################################################################
#Function: getDiaValues
#CURRENT STATUS: This function could use a little more testing.
#
#This function takes in a type argument and returns a list containing a vector
#of diameters and a vector of diameter class values based on R3 midscale
#mapping, timber dominance type, or woodland  dominance types criteria. These
#different diameter classes are described in NFS_Reg_Veg_Class.pdf.
#
#Arguments
#type: Indicator variable used to determine which diameters and diameter class
#      values to return.
#      1 - Midscale mapping (default and will be used if any value other than
#          2 or 3 is entered for type argument.)
#      2 - Timberland dominance type
#      3 - Woodland dominance type
#
#Return values when type = 1
#diameters = c(0, 5, 10, 20, 30)
#diameter class values:
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium and large tree canopy cover
#4 = very large tree canopy cover
#5 - giant tree canopy cover
#
#Return values when type = 2
#diameters = c(0, 5, 10, 20)
#diameter class values:
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium tree canopy cover
#4 = large - giant tree cover
#
#Return values when type = 3
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
#CURRENT STATUS: I think this function is mostly working but could use a
#little more testing.
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
#4) When 1 - 3 are not true, then x is compared against all i-th and ith+1
#   values in inputValues. If x is a value GE to ith value and LT ith + 1
#   value in inputValues, then ith value in outputValues is returned.
#
#Arguments
#x:             Incoming value to evaluate. Must be numeric and not NA.
#inputValues:   Values to compare x against. This argument has to be the same
#               length as outputValues.
#outputValues:  Classification values to return containing depending on value
#               x. This arugment has to be the same length as inputValues.
#invalidReturn: Default value to return when:
#               1) x is not a valid value
#               2) InputValues is empty, a character vector, or contains any
#                  NA values.
#               3) Length of inputValues does not equal outputValues.
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
#CURRENT STATUS: This function could use a little more testing.
#
#This function takes in a vector of DBH values and a type argument and returns
#a vector of diameter classes that are used to calculate canopy size class
#in the canSizeCls function.
#
#Arguments
#DBH:  Vector containing DBH values.
#type: Indicator variable used to determine which type of diameter class to
#      return
#      1 - Midscale mapping (default and will be used if any value other than
#          2 or 3 is entered for type argument.)
#      2 - Timberland dominance type
#      3 - Woodland dominance type
#
#Possible return values when type = 1
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium and large tree canopy cover
#4 = very large tree canopy cover
#5 - giant tree canopy cover
#
#Possible return values when type = 2
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium tree canopy cover
#4 = large - giant tree cover
#
#Possible return values when type = 3
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
#CURRENT STATUS: This function may need a little more refinement. Need to
#find out if CanSizCl should be zero if stand CC is below a certain threshold
#(i.e. 10% canopy cover).
#
#This function takes in a tree-level dataframe and returns a canopy size
#class for the inventory plot that the tree records reside on.
#
#Argument
#dat:     Tree level dataframe that contains DBH and canopy percent cover of each
#         tree record (TREECC).
#totalCC: Percent canopy cover of stand.
#type:    Indicator variable used to determine which type of diameter class to
#         return
#         1 - Midscale mapping (default and will be used if any value other than
#         2 or 3 is entered for type argument.)
#         2 - Timberland dominance type
#         3 - Woodland dominance type
#
#Possible return values when type = 1
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium and large tree canopy cover
#4 = very large tree canopy cover
#5 - giant tree canopy cover
#
#Possible return values when type = 2
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium tree canopy cover
#4 = large - giant tree cover
#
#Possible return values when type = 3
#1 = seedling sapling canopy cover
#2 = small tree canopy cover
#3 = medium - giant tree cover
#############################################################################

#'@export
canSizeCl<-function(dat, totalCC, type=1, debug = F)
{

  #Statement used to avoid NOTE when stateTrans package is built.
  DC<-TREECC<-NULL

  #If plot CC is less than 10% then cansizcl is 0
  if(totalCC < 10)
  {
    cansizcl = 0
    if(debug) cat("Total CC:", totalCC, " less than 10.", "\n")
  }

  #Else calculate cansizcl
  else
  {
    #Determine diameter class for each tree record
    dat$DC<-getCanSizeDC(dat$DBH, type, debug)

    #Summarize CC by midscale diameter class
    can.sum<- dat %>%
      dplyr::group_by(DC) %>%
      dplyr::summarize(CC = sum(TREECC))

    #Extract cansizcl associated with maximum CC
    cansizcl<-can.sum$DC[which.max(can.sum$CC)]

    if(debug) cat("In canSizeCl function", "\n",
                  "DC:", can.sum$DC, "\n",
                  "CC:", can.sum$CC, "\n",
                  "cansizcl:", cansizcl, "\n")
  }

  return(cansizcl)
}
