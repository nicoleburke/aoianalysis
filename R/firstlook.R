#### firstlook function
#### aoianalysis package
#### Created by Nicole Burke
#### 08/2018-11/2018

###### The function! ######

## Anything in the function that is commented out
##    that says "print(___) is just so I could check the function

## The function does not like NAs for the boolean evaluators
##    See 'getridofNA' function

# Input needs to be: dataframe, "columnheads"

firstlook <- function(data_by_trial, subjectIDs, AOI1, AOI2) {
  # Change variables to something the function can work with
  subjects <- data_by_trial[subjectIDs]
  aoi1 <- data_by_trial[AOI1]
  aoi2 <- data_by_trial[AOI2]
  # Make a new dataframe
  newdata <- cbind(subjects, aoi1, aoi2)
  newdata <- data.frame(newdata)
  colnames(newdata) <- c("subjects", "aoi1", "aoi2")
  # Create a vector that contains unique character strings for each subj
  subjids <- levels(newdata$subjects)
  # print(subjids)
  # Create a matrix with rows = # of subjects and 1 col with the AOI subjects first looked
  firstlook <- matrix(nrow = length(subjids), ncol = 1)
  row.names(firstlook) <- subjids
  colnames(firstlook) <- c("FirstLook")
  # a 'for' loop to search for each ind. subj
  for (s in 1:length(subjids)) {
    # print(s)
    # print(subjids[s])
    subjindex <- grep(subjids[s], newdata$subjects)
    # print(subjindex)
    starti <- subjindex[1]
    # print(starti)
    endi <- subjindex[length(subjindex)]
    # print(endi)
    # 'count' is a dummy variable
    count <- 0
    # search for the first hit to an AOI for each subject
    for (i in starti:endi) {
      # Search iteratively until there is a hit in 1st AOI
      if (newdata$aoi1[i] == 1 && count == 0) {
        count <- count + 1
        # Add 1st AOI to the empy matrix
        firstlook[s, count] <- AOI1
        break
      }
      if (newdata$aoi2[i] == 1 && count == 0) {
        count <- count + 1
        # Add 2nd AOI to the empty matrix
        firstlook[s, count] <- AOI2
        break
      }
    }
    # If the person does not look to either AOI,
    #   put "none" in the column
    if (count == 0) {
      count <- count + 1
      firstlook[s, count] <- "none"
    }
  }
  return(firstlook)
}


