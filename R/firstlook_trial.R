#### firstlook function
#### aoianalysis package
#### Created by Nicole Burke
#### 02/2019

###### The function! ######
### currently IN PROGRESS

## Anything in the function that is commented out
##    that says "print(___) is just so I could check the function

## The function does not like NAs for the boolean evaluators
##    See 'getridofNA' function

# Input needs to be: dataframe, "columnheads"

# trying to add a layer to go through trial as well as subjects

firstlook <- function(data_all,subjectIDs,trialnames,AOI1,AOI2) {
  # Change variables to something the function can work with
  subjects <- data_all[subjectIDs]
  aoi1 <- data_all[AOI1]
  aoi2 <- data_all[AOI2]
  trials <- data_all[trialnames]
  # Make a new dataframe
  newdata <- cbind(subjects, trials, aoi1, aoi2)
  newdata <- data.frame(newdata)
  colnames(newdata) <- c("subjects", "trials", "aoi1", "aoi2")
  # Create a vector that contains unique character strings for each subj
  subjids <- levels(newdata$subjects)
  print(subjids)
  # Create a vector that contains unique character strings for each trial
  trial_names <- levels(newdata$trials)
  print(trial_names)
  rowlength <- length(subjids)*length(trial_names)
  print(rowlength)
  # Create a matrix with rows = # of subjects and cols with trials and where firstlook AOI
  firstlook <- matrix(nrow = rowlength, ncol = 2)
  #row.names(firstlook) <- subjids
  colnames(firstlook) <- c("TrialName", "FirstLook")
  # a 'for' loop to search for each ind. subj
  for (s in 1:length(subjids)) {
    print(s)
    print(subjids[s])
    subjindex <- grep(subjids[s], newdata$subjects)
    # print(subjindex)
    startt <- subjindex[1]
    print("startt")
    print(startt)
    endt <- subjindex[length(subjindex)]
    print("endt")
    print(endt)
    # 'count' is a dummy variable
    count <- 0
    # a 'for' loop to search each trial type
    for (t in startt:endt) {
      trialindex <- grep(trial_names[t], newdata$trials)
      print(t)
      print ("trialindex")
      print(trialindex)
      starti <- trialindex[1]
      endi <- trialindex[length(trialindex)]
      for (i in starti:endi) {
        # Search iteratively until there is a hit in 1st AOI
        if (newdata$aoi1[i] == 1 && count == 0) {
          count <- count + 1
          # Add 1st AOI to the empy matrix
          firstlook[s, t, count] <- AOI1
          break
        }
        if (newdata$aoi2[i] == 1 && count == 0) {
          count <- count + 1
          # Add 2nd AOI to the empty matrix
          firstlook[s, t, count] <- AOI2
          break
        }
        # If the person does not look to either AOI,
        #   put "none" in the column
        if (count == 0) {
          count <- count + 1
          firstlook[s, t, count] <- "none"
        }
      }
    }
  }
  return(firstlook)
}


firstlook(datatrial, "subjectID", "trialname", "col1", "col2")





