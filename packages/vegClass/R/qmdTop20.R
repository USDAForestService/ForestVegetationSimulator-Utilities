################################################################################
#Function qmdTop20
#
#This function calculates QMD of top 20% of tree records based on DBH in a
#plot/stand assuming at least 20 TPA. This algorithm first determines if 20% of
#plot/stand TPA is larger than 20 TPA. The algorithm will then iterate across
#a tree list sorted from largest to smallest DBH and sum (DBH^2 * expansion
#factor) and expansion factors of the tree records until 20 TPA or 20% of
#plot/stand TPA (whichever is larger) is exceeded. QMD will then be calculated
#from the squared diameter and TPA values. If the input tree list has less than
#20 TPA, then QMD is calculated from the entirety of the tree list. If percent
#canopy cover of the stand is less than 10, then seedlings  (DBH == 0.1) are
#included, otherwise they are ignored.
#
#Arguments:
#
#data:    Tree level dataframe corresponding to trees from a single stand.
#
#stand:   Name of column corresponding to stand ID associated with tree records
#         in data. By default this value is set to "StandID".
#
#dbh:     Name of column in data argument corresponding to DBH of tree records.
#         By default this argument is set to "DBH".
#
#expf:    Name of column in data argument corresponding to TPA of tree records.
#         By default this argument is set to "TPA".
#
#TPA:     Trees per acre of plot/stand.
#
#CC:      Percent canopy cover corrected for overlap for plot/stand.
#
#debug:   logical variable indicating if debug statements should be printed. By
#         default this value is set to FALSE.
#
#Value
#
#QMD top 20 of inventory plot/stand
################################################################################

#'@export
qmdTop20 <- function(data,
                     stand = "StandID",
                     dbh = "DBH",
                     expf = "TPA",
                     TPA,
                     CC,
                     debug = F)
{
  if(debug)
  {
    cat("In function qmdTop20", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "expf:", expf, "\n", "\n")
  }

  #Check of missing columns in data
  missing <- c(dbh, expf, stand) %in% colnames(data)

  #If there is a FALSE value in missing report message and return NA value
  if(F %in% missing)
  {
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(NA)
  }

  #Sort data from largest to smallest diameter
  data <- data[order(-data[[dbh]]),]

  #If CC is greater than or equal to 10, set min to 0.2
  if(CC >= 10) minDBH = 0.2
  else minDBH = 0.1

  #Calculate TPA
  PTPA <- TPA

  #Calculate TPA of top 20. This value is PTPA * .20 or 20, depending on which
  #is larger.
  TPA20 <- max(PTPA * 0.20, 20)

  #Debug on CC, PTPA, TPA20, and minDBH
  if(debug) {
    cat("CC:", CC, "\n",
        "PTPA:", PTPA, "\n",
        "TPA20:", TPA20, "\n",
        "MINDBH:", minDBH, "\n", "\n")
  }

  #Initialize DBHSQ and TPA
  TPASUM = 0
  DBHSQ = 0

  #Loop across data and sum DBH^2 * TPA and TPA until TPA20 is exceeded or
  #the end of the tree list is reached.
  for(i in 1:nrow(data))
  {
    #Process record if its DBH is greater than or equal to minDBH
    if(data[[dbh]][i] >= minDBH)
    {
      #Add TPASUM value to TPASUM.
      TPASUM = TPASUM + data[[expf]][i]

      #TPASUM has exceeded TPA20
      if(TPASUM >= TPA20)
      {
        #Do a debug
        if(debug)
        {
          cat("TPA20 exceeded.", "\n",
              "TREE EXP:", data[[expf]][i], "\n",
              "TPASUM:", TPASUM, "\n",
              "TPA20:", TPA20, "\n")
        }

        #If TPASUM is greater than TPA20 TPA and expansion factor of tree i will
        #need to be adjusted. The difference between TPASUM and TPA20 will be
        #deducted from TPASUM and expansion factor of tree i.
        if(TPASUM > TPA20)
        {
          tpaDif <- (TPASUM - TPA20)
          TPASUM <- TPASUM - tpaDif

          #Calculate DBHSQ but deduct tpaDif from expansion factor of tree
          #record.
          DBHSQ = DBHSQ + data[[dbh]][i] ^ 2 * (data[[expf]][i] - tpaDif)

          if(debug)
          {
            cat("tpaDif:", tpaDif, "\n",
                "Updated TPASUM:", TPASUM, "\n",
                "TPA20:", TPA20, "\n",
                "TREE DBH:", data[[dbh]][i], "\n",
                "EXPF:", data[[expf]][i], "\n",
                "Adjusted EXPF:", data[[expf]][i] - tpaDif, "\n", "\n")
          }
        }

        #If TPASUM and TPA20 just happen to be equal, then add new value to DBHSQ
        #without adjusting expansion factor
        else
        {
          DBHSQ = DBHSQ + data[[dbh]][i] ^ 2 * data[[expf]][i]
          cat("TREE DBH:", data[[dbh]][i], "\n",
              "EXPF:", data[[expf]][i], "\n",
              "TPASUM:", TPASUM, "\n", "\n")
        }

        if(debug) cat("Breaking out of loop.", "\n")

        #Break out of loop
        break
      }

      #TPA20 has not been exceeded. Added next value to DBHSQ
      else
      {
        DBHSQ = DBHSQ + data[[dbh]][i] ^ 2 * data[[expf]][i]

        if(debug)
        {
          cat("TREE DBH:", data[[dbh]][i], "\n",
              "TREE EXP:", data[[expf]][i], "\n",
              "DBHSQ:", DBHSQ, "\n",
              "TPASUM:", TPASUM, "\n", "\n")
        }
      }
    }
  }

  #If TPASUM is 0 for any reason set QMD20 to 0, otherwise calculate it from
  #DBHSQ and TPASUM.
  if(TPASUM <= 0) QMD20 = 0

  else
  {
    QMD20 <- sqrt(DBHSQ/TPASUM)
    if(debug)
    {
      cat("Calculating QMD20", "\n",
          "DBHSQ:", DBHSQ, "\n",
          "TPASUM:", TPASUM, "\n")
    }
  }

  #Print stand and QMD20 if debug is true.
  if(debug)
  {
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("QMDTOP20:", QMD20, "\n")
  }

  return(QMD20)
}
