#### firstlook function
#### aoianalysis package
#### Created by Nicole Burke 
#### 08/2018-11/2018

###### The function! ###### 

## The input for the function needs to be "df$columnhead"
##    - In a perfect world, Nicole will fix this 
##    - But for now it's fine

## AOI1 needs to be "left" and AOI2 needs to be "right"
##    - In a perfect world, Nicole will fix this, 
##        so that it is unique to the input
##    - But for now it's fine

## Anything in the function that is commented out 
##    that says "print(___) is just so I could check the function

## The function does not like NAs for the boolean evaluators 
##    See 'getridofNA' function 

# Input needs to be "df$subjects" 
firstlook <- function(subjects, aoi1, aoi2) {
  # Create a vector that contains unique character strings for each subj
  subjids <- levels(subjects)
  # print(subjids)
  # Create a matrix with rows = # of subjects and 1 col with the AOI subjects first looked
  firstlook <- matrix(nrow = length(subjids), ncol = 1)
  row.names(firstlook) <- subjids
  colnames(firstlook) <- c("FirstLook_helping")
  # a 'for' loop to search for each ind. subj 
  for (s in 1:length(subjids)) {
    # print(s)
    # print(subjids[s])
    subjindex <- grep(subjids[s], subjects)
    # print(subjindex)
    starti <- subjindex[1]
    # print(starti)
    endi <- subjindex[length(subjindex)]
    # print(endi)
    # 'count' is a dummy variable
    count <- 0
    # search for a hit to necessary AOI for each individual subj 
    for (i in starti:endi) {
      # Search iteratively until there is a hit in 1st AOI
      if (aoi1[i] == 1 && count == 0) {
        count <- count + 1
        # Add 1st AOI to the empy matrix 
        firstlook[s, count] <- "left"
        break
      }
      if (aoi2[i] == 1 && count == 0) {
        count <- count + 1
        # Add 2nd AOI to the empty matrix 
        firstlook[s, count] <- "right"
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