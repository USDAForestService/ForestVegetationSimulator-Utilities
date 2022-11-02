################################################################################
#Function: baStory
#
#Calculates storiedness for a stand/plot in accordance with NFS Regional
#Vegetation Classification Algorithms Vandendriesche (2013 pg. R3-4 - R3-5).
#
#Arguments
#
#data:    Tree level dataframe corresponding to trees from a single stand.
#
#stand:   Name of column corresponding to stand associated with tree records
#         in data. By default this value is set to "StandID".
#
#dbh:     Name of column in data argument corresponding to DBH of tree records.
#         By default this argument is set to "DBH".
#
#expf:    Name of column in data argument corresponding to TPA of tree records.
#         By default this argument is set to "TPA".
#
#BA:      Basal area per acre of plot/stand.
#
#TPA:     Trees per acre of plot/stand.
#
#CC:      Percent canopy cover corrected for overlap of plot/stand.
#
#debug:	  Boolean variable used to specify if debug output should be printed to
#         R console. If value is TRUE, then debug output will printed to R
#         console.
#
#Return value
#
#Storiedness value (integer value from 0 - 3)
################################################################################

#'@export
baStory<-function(data,
                  stand = "StandID",
                  dbh = "DBH",
                  expf = "TPA",
                  BA,
                  TPA,
                  CC,
                  debug = F)
{
  if(debug)
  {
    cat("In function baStory", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "expf:", expf, "\n", "\n")
  }

  #Check of missing columns in data
  missing <- c(stand, dbh, expf) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Calculate TREECC and TREEBA
  data$TREEBA <- data[[dbh]]^2 * data[[expf]] * 0.005454

  #Print BA
  if(debug) cat("BA of plot is", BA, "\n")
  if(debug) cat("CC of plot is", CC, "\n")
  if(debug) cat("TPA of plot is", TPA, "\n")

  #Test if plot is below minimum CC and TPA. If so, then set story to 0.
  if(CC < 10 & TPA < 100)
  {
    story<-0
    if(debug) cat("Total CC:",CC, " LT 10 and TPA:", TPA,
                                "LT 100.", "\n")
  }

  #Test if plot is below minimum CC and above minimum TPA. If so, then set story
  #to 1.
  else if(CC < 10 & TPA >= 100)
  {
    story<-1
    if(debug) cat("Total CC:",CC, " LT 10 and TPA:", TPA,
    "GE 100.", "\n")
  }

  #Else calculate story using storiedness algorithm
  else
  {
    #Set story to 3 (multistory). This value will be returned if plot is not
    #determined to be single or two story in logic below.
    story<- 3

    #Test if sum of BA for trees greater than 24 inches is greater than 70% of
    #total stand basal area. If true then stand is single story.
    if((sum(data$TREEBA[data[[dbh]] >= 24]) / BA )>= 0.7)
    {
      if(debug) cat("BA for trees >= 24 inches:",
                    sum(data$TREEBA[data[[dbh]] >= 24]),
                    "GE", "total BA:", BA, "\n")
      story = 1
    }

    else
    {
      #Initialize bottom variable (moving lower limit of diameter range being
      #evaluated)
      bottom = 0

      #Initialize boolean variable used to determine if story was found in
      #while loop below
      storyFound = F

      while(bottom < 24 & !storyFound)
      {
        #Determine top variable (moving upper limit of diameter range being
        #evaluated)
        top = bottom + 8

        #Calculate BA in current 8" DBH range
        baSlide<-sum(data$TREEBA[data[[dbh]] >= bottom & data[[dbh]] < top])
        if(debug) cat("baSlide", "for trees between", bottom, "and",
                      top, "DBH:", baSlide, "\n")

        #Test if plot is two story based on value of baSlide
        if((baSlide/BA) >= 0.6 & (baSlide/BA) < 0.7)
        {
          if(debug) cat("baSlide for trees between", bottom, "and", top, "DBH:",
                         baSlide, "GE 60% of total plot BA:", BA, "\n")
          story = 2
          storyFound = T
        }

        #Test if plot is single story based on value of baSlide
        if((baSlide/BA) >= 0.7)
        {
          if(debug) cat("baSlide for trees between", bottom, "and", top, "DBH:",
                        baSlide, "GE 70% of total plot BA:", BA, "\n")
          story = 1
          storyFound = T
        }

        #increment bottom
        bottom = bottom + 1
      }
    }
  }

  return(story)
}
