#############################################################################
#Function: baStory
#
#CURRENT STATUS: 
#
#Calculates storiedness in accordance with NFS Regional Vegetation 
#Classification Algorithms Vandendriesche (2013) pg. R3-1.
#
#Arguments
#stdYrFrame: Tree level dataframe that contains DBH and basal area values
#            for each tree record.
#totalCC:    Percent canopy cover of stand.
#############################################################################

baStory<-function(stdYrFrame, totalCC, debug = F)
{
  #Calculate totalBA
  totalBA<-sum(stdYrFrame$TREEBA)
  if(debug) cat("BA of plot is", totalBA, "\n")
  
  #Test if plot is below minimum CC. If so, then set story to 0.
  if(totalCC < 10)
  {
    story<-0
    if(debug) cat("Total CC:", totalCC, " less than 10.", "\n")
  }
  
  #Else calculate story using storiedness algorithm
  else
  {
    #Set story to 3 (multistory). This value will be returned if plot is not
    #determined to be single or two story in logic below.
    story<- 3
    
    #Test if sum of BA for trees greater than 24 inches is greater than 70% of
    #total stand basal area. If true then stand is single story.
    if((sum(stdYrFrame$TREEBA[stdYrFrame$DBH >= 24]) / totalBA )>= 0.7)
    {
      if(debug) cat("BA for trees >= 24 inches:", sum(stdYrFrame$TREEBA[stdYrFrame$DBH >= 24]),
                    "GE", "total BA:", totalBA, "\n")
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
        baSlide<-sum(stdYrFrame$TREEBA[stdYrFrame$DBH >= bottom & stdYrFrame$DBH <
                                         top])
        if(debug) cat("baSlide", "for trees between", bottom, "and",
                      top, "DBH:", baSlide, "\n")
        
        #Test if plot is two story based on value of baSlide
        if((baSlide/totalBA) >= 0.6 & (baSlide/totalBA) < 0.7)
        {
          if(debug) cat("baSlide for trees between", bottom, "and", top, "DBH:",
                         baSlide, "GE 60% of total plot BA:", totalBA, "\n")
          story = 2
          storyFound = T
        }
        
        #Test if plot is single story based on value of baSlide
        if((baSlide/totalBA) >= 0.7)
        {
          if(debug) cat("baSlide for trees between", bottom, "and", top, "DBH:",
                        baSlide, "GE 70% of total plot BA:", totalBA, "\n")
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
