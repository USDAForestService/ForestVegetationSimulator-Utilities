#############################################################################
#Function: calcCanClassDC
#CURRENT STATUS: This function could use a little more testing. Will remove
#the commented out upper vectors at some point.
#
#This function takes in a DBH value and returns a diameter class based on R3
#midscale mapping, timber dominance type, or woodland  dominance types criteria.
#These different diameter classes are described in NFS_Reg_Veg_Class.pdf.
#
#Arguments
#type: Indicator variable used to determine which type of diameter class to
#      return
#      1 - Midscale mapping (default and will be used if any value other than 
#          2 or 3 is entered for type argument.)
#      2 - Timberland dominance type
#      3 - Woodland dominance type
#
#DBH:  diameter of tree record
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

canSizeDC<-function(DBH, type = 1)
{
  #Define lower and upper DBH limits for midscale mapping
  #Lower diameter limit
  diameters<-c(0, 5, 10, 20, 30)
  
  #Upper diameter limit
  # upper<-c(5, 10, 20, 30, 30)
  
  #Diameter class values
  dcls<-seq(from = 1, to = 5, by = 1)
  
  #Define lower and upper DBH limits for timber dominance types
  if(type == 2)
  {
    #Lower diameter limit
    diameters<-c(0, 5, 10, 20)
    
    #Upper diameter limit
    # upper<-c(5, 10, 20, 20)
    
    #Diameter class values
    dcls<-seq(from = 1, to = 4, by = 1)
  }
  
  #Define lower and upper DBH limits for woodland dominance types
  if(type == 3)
  {
    #Lower diameter limit
    diameters<-c(0, 5, 10)
    
    #Upper diameter limit
    # upper<-c(5, 10, 10)
    
    #Diameter class values
    dcls<-seq(from = 1, to = 3, by = 1)
  }
  
  #Find diameter class
  dc<-findCategory(DBH, diameters, dcls)
  
  return(dc)
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
#Function: calcCanSizCl
#CURRENT STATUS: This function could use some more work. Not sure what
#happens yet if dataframe with all dead trees is passed in or if there
#is a value to return if total stand percent canopy cover is below a certain
#value.
#
#This function takes in a dataframe containing diameter class and treecc and
#returns a canopy size class value ranging from 1-5. Canopy size class is 
#determined by calculating percent canopy cover for each mid scale mapping
#diameter class. The diameter class with the largest amount of percent 
#canopy cover is returned.
#
#Argument
#dat:  tree level dataframe containing DBH and canopy percent cover of each
#      tree record (TREECC).
#type: type determines what kind of diameter class will be calculated. See
#      use of type in calcCanSizDC
#
#Possible return values from function
#1 - 5
#############################################################################

canSizeCl<-function(dat, type=1)
{
  #Create column with type variable
  dat$TYPE<-type
  
  #Calculate diameter class for incoming records
  dat$DC<-mapply(canSizeDC, dat$DBH, dat$TYPE)
  
  #Summarize CC by midscale diameter class
  can.sum<- dat %>%
    group_by(DC) %>%
    summarize(CC = sum(TREECC))
  
  #Extract cansizcl associated with maximum CC
  cansizcl<-can.sum$DC[which.max(can.sum$CC)]
  
  return(cansizcl)
}
