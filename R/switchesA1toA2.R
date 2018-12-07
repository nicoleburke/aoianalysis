#### switchesA1toA2
#### a function to calculate contigent switches b/t 2 AOIs
#### aoianalysis package
#### Created by Nicole Burke
#### 12/2018

#### What does function do? #####
# Creates a dataframe with subjects and the number
#     of contingent switches between aoi1 to aoi2.

#### How do set-up dataframe ####
# The dataframe should be the raw data export from Tobii
# In Tobii, everytime you create an AOI on a new movie/stimuli
#     it creates a new AOI column. SO, even if you have the
#     same name AOI, like "aoi1", for each different
#     stimuli you have that AOI, it gets a new column in the raw
#     data.

# Given that this is true, each unique stimuli (the column MediaName)
#   has it's own dataframe for this function to work. This way,
#   the input can be the columns that you need the function to search.

###### The function! ######

## The input for the function needs to be:
##      - dataframe: name of dataframe
##      - The rest of the input should be the column heads in ""

## Anything in the function that is commented out
##    that says "print(___) is just so I could check the function

## The function does not like NAs for the boolean evaluators
##    See 'getridofNA' function

# input: dataframe, "columnnames"

switchesA1toA2<- function(data_by_trial, subjectID, AOI1, AOI2) {
  # First variables need to be changed
  subjects <- data_by_trial[subjectID]
  aoi1 <- data_by_trial[AOI1]
  aoi2 <- data_by_trial[AOI2]
  # make new dataframe
  newdata <- cbind(subjects, aoi1, aoi2)
  newdata <- data.frame(newdata)
  colnames(newdata) <- c("subjects","aoi1", "aoi2")
  # Create a vector that contains unique character strings for each subj
  subjids <- levels(newdata$subjects)
  # Create a matrix with rows = # of subjects and 1 col # of swtiches made between A1 to A2
  numA1toA2 <- matrix(nrow = length(subjids), ncol = 1)
  row.names(numA1toA2) <- subjids
  colnames(numA1toA2) <- c("NumSwtichesA1toA2")
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
    # numswitchA1toA2 is a dummy variable to add up swtiches
    numswitchA1toA2 <- 0
    for (i in starti:endi) {
      # Look for a hit to in A1
      if (newdata$aoi1[i] == 1) {
        # Check that there is no look in the following line in A1
        if (newdata$aoi1[i+1] == 0) {
          # Finally, check if there is a contingent hit in A2
          if (newdata$aoi2[i+1] == 1) {
            numswitchA1toA2 <- numswitchA1toA2 + 1
          }
        }
      }
      # Add number of swtiches to dataframe per subject
      numA1toA2[s, 1] <- numswitchA1toA2
    }
  }
  return(numA1toA2)
}




