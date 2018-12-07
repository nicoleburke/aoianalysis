#### switches
#### a function to calculate contigent switches b/t 2 AOIs
#### aoianalysis package
#### Created by Nicole Burke
#### 12/2018

# This is a work in progress! I am adapting this from my swtiches
# function for my somewhat failed eyetracking experiment

# Here goes nothing

# Input: dataframe, "columns"

switchA1toA2_inprogress <- function(data_by_trial, subjectID, AOI1, AOI2) {
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
  colnames(numA1toA2) <- c("NumSwtiches A1toA2")
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
    # search for a hit to necessary AOI for each individual subj
    # numswitchA1toA2 is a dummy variable to add up swtiches
    numswitchA1toA2 <- 0
    for (i in starti:endi) {
      # Look for a hit to in A1
      if (newdata$aoi1[i] == 1) {
        if (newdata$aoi1[i+1] == 0) {
          if (newdata$aoi2[i+1] == 1) {
            numswitchA1toA2 <- numswitchA1toA2 + 1
          }
        }
      }
      # Add number of swtiches to dataframe
      numA1toA2[s, numswitchA1toA2]
    }
  }
  return(numA1toA2)
}








