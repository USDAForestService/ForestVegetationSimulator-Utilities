################################################################################
#Function: baStory
#
#Calculates storiedness in accordance with NFS Regional Vegetation
#Classification Algorithms Vandendriesche (2013 pg. R3-4 - R3-5).
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
#crwidth: Name of column corresponding crown width values of tree records in
#         data. By default this argument is set to "CrWidth".

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
                  crwidth = "CrWidth",
                  debug = F)
{
  if(debug)
  {
    cat("In function baStory", "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "crwidth:", crwidth, "\n",
        "expf:", expf, "\n", "\n")
  }

  #Check of missing columns in data
  missing <- c(stand, dbh, expf, crwidth) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Print stand
  if(debug) cat("Stand:", unique(data[[stand]]), "\n")

  #Calculate TREECC and TREEBA
  data$TREECC <- pi * (data[[crwidth]]/2)^2 *(data[[expf]]/43560) * 100
  data$TREEBA <- data[[dbh]]^2 * data[[expf]] * 0.005454

  if(debug) cat("Columns:", "\n",
                "stand:", stand, "\n",
                "dbh:", dbh, "\n",
                "expf:", expf, "\n",
                "crwidth:", crwidth, "\n", "\n")

  #Calculate CC corrected for overlap
  totalCC <- plotCC(data,
                    stand = stand,
                    dbh = dbh,
                    crwidth = crwidth,
                    expf = expf,
                    type = 2)

  #Calculate BA
  ba <- plotBA(data,
               stand = stand,
               dbh = dbh,
               expf = expf)

  #Calculate TPA
  tpa <- plotTPA(data,
                 stand = stand,
                 dbh = dbh,
                 expf = expf)

  #Print BA
  if(debug) cat("BA of plot is", ba, "\n")
  if(debug) cat("CC of plot is", totalCC, "\n")
  if(debug) cat("TPA of plot is", tpa, "\n")

  #Test if plot is below minimum CC and TPA. If so, then set story to 0.
  if(totalCC < 10 & tpa < 100)
  {
    story<-0
    if(debug) cat("Total CC:", totalCC, " LT 10 and TPA:", tpa,
                                "LT 100.", "\n")
  }

  #Test if plot is below minimum CC and above minimum TPA. If so, then set story
  #to 1.
  else if(totalCC < 10 & tpa >= 100)
  {
    story<-1
    if(debug) cat("Total CC:", totalCC, " LT 10 and TPA:", tpa,
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
    if((sum(data$TREEBA[data[[dbh]] >= 24]) / ba )>= 0.7)
    {
      if(debug) cat("BA for trees >= 24 inches:",
                    sum(data$TREEBA[data[[dbh]] >= 24]),
                    "GE", "total BA:", ba, "\n")
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
        if((baSlide/ba) >= 0.6 & (baSlide/ba) < 0.7)
        {
          if(debug) cat("baSlide for trees between", bottom, "and", top, "DBH:",
                         baSlide, "GE 60% of total plot BA:", ba, "\n")
          story = 2
          storyFound = T
        }

        #Test if plot is single story based on value of baSlide
        if((baSlide/ba) >= 0.7)
        {
          if(debug) cat("baSlide for trees between", bottom, "and", top, "DBH:",
                        baSlide, "GE 70% of total plot BA:", ba, "\n")
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
