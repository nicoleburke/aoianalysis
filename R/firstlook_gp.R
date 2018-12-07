#### firstlook_gp
#### a function for the goal prediction paradigm
#### aoianalysis package
#### Created by Nicole Burke
#### 08/2018-11/2018

#### What does function do? #####
# Creates a dataframe with subjects and their first look for Trial1 and Trial2

#### How do set-up dataframe ####
# The dataframe should be the raw data export from Tobii
# In Tobii, everytime you create an AOI on a new movie/stimuli
#     it creates a new AOI column. SO, even if you have the
#     same name AOI, like "handAOI", for each different
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
##    See 'getridofNA_gp' function

# input: dataframe, "columnnames"
# adding something to make a point

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



