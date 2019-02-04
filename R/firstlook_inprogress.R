#### firstlook_gp
#### a function for the goal prediction paradigm
#### aoianalysis package
#### Created by Nicole Burke
#### 08/2018-11/2018

#### What does function do? #####
# This is in-progress script for firstlook_gp

# I need to include trialtype into the function so that you don't have to keep
# sub-setting the df by trial each time you want to run this analysis

# There are two ways that needs to go:
#   1. Each trial is referred to uniquely
#   2. The same stim is used, so trial type is indicated by "MovieStart" and
#       "MovieEnd"

# ^ I am starting with the first one because that's easier

###### The function! ######

## The input for the function needs to be:
##      - dataframe: name of dataframe
##      - The rest of the input should be the column heads in ""

## Anything in the function that is commented out
##    that says "print(___) is just so I could check the function

## The function does not like NAs for the boolean evaluators
##    See 'getridofNA_gp' function

# input: dataframe, "columnnames"

firstlook_gp <- function(dataframe, subjectID, handAOI, AOI1, AOI2) {
  # First the variables need to be changed
  subjects <- dataframe[subjectID]
  handAOI <- dataframe[handAOI]
  aoi1 <- dataframe[AOI1]
  aoi2 <- dataframe[AOI2]
  # make new dataframe
  newdata <- cbind(subjects, handAOI, aoi1, aoi2)
  newdata <- data.frame(newdata)
  colnames(newdata) <- c("subjects", "neccessaryAOI", "aoi1", "aoi2")
  # print("newdata")
  # print(newdata)
  # Create a vector that contains unique character strings for each subj
  subjids <- levels(newdata$subjects)
  # print(subjids)
  # Create a matrix with rows = # of subjects and 1 col with the AOI subjects first looked
  firstlook <- matrix(nrow = length(subjids), ncol = 1)
  row.names(firstlook) <- subjids
  colnames(firstlook) <- c("FirstLook-GP")
  # for loop to search for each ind. subj
  for (s in 1:length(subjids)) {
    # print("subjectid")
    # print(s)
    # print(subjids[s])
    # Creates a vector with the index of subjects
    subjindex <- grep(subjids[s], newdata$subjects)
    # print("subjindex")
    # print(subjindex)
    # Where the function should start searching for hits to neccessaryAOI
    starti <- subjindex[1]
    # print(starti)
    # Where the function should stop searching for hits to neccessaryAOI
    endi <- subjindex[length(subjindex)]
    # print(endi)
    # 'count' is a dummy variable to break out of loop
    count <- 0
    # search for a hit to necessary AOI for each individual subj
    for (i in starti:(endi-1)) {
      # Look for a hit to the necessary AOI
      if (newdata$neccessaryAOI[i] == 1) {
        # Search iteratively until there is a hit in 1st AOI
        for (x in (i + 1):endi) {
          if (newdata$aoi1[x] == 1 && count == 0) {
            count <- count + 1
            # Add 1st AOI to the empy matrix
            firstlook[s, count] <- AOI1
            break
          }
          if (newdata$aoi2[x] == 1 && count == 0) {
            count <- count + 1
            # Add 2nd AOI to the empty matrix
            firstlook[s, count] <- AOI2
            break
          }
        }
      }
    }
    # If the person does not look to the hand AOI,
    #   put "none" in the column
    if (count == 0) {
      count <- count + 1
      firstlook[s, count] <- "none"
    }
  }
  return(firstlook)
}



