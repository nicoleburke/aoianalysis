#### getridofNA_gp function
#### aoianalysis package
#### Created by Nicole Burke
#### 08/2018-11/2018

#### What does function do? #####
# Takes NeccessaryAOI (handAOI) and the other AOI columns
#   and gets rid of NAs. It replaces NAs with 999.

# Why? Because the boolean evaluations gets mad in the
#     goal prediction first look function.

# Get rid of NAs in  and AOI columns
getridofNA_gp <- function(dataframe, subjects, HandAOI, AOI1, AOI2) {
  library(tidyverse)
  # First the variables need to be changed
  subjects <- dataframe[subjects]
  handAOI <- dataframe[HandAOI]
  aoi1 <- dataframe[AOI1]
  aoi2 <- dataframe[AOI2]
  # make new dataframe
  newdata <- cbind(subjects, handAOI, aoi1, aoi2)
  newdata <- data.frame(newdata)
  colnames(newdata) <- c("subjects", "handAOI", "aoi1", "aoi2")
  # mutate variables
  newdata <- newdata %>%
    mutate(handAOI = ifelse(is.na(handAOI), 999, handAOI),
           aoi1 = ifelse(is.na(aoi1), 999, aoi1),
           aoi2 = ifelse(is.na(aoi2), 999, aoi2))
  # Change column name for AOI
  col_names <- sub("aoi1", AOI1, colnames(newdata))
  # Add new column name for AOI1
  colnames(newdata) <- col_names
  # Change column name for AOI2
  col_names2 <- sub("aoi2", AOI2, colnames(newdata))
  # Add new column name for AOI2
  colnames(newdata) <- col_names2
  return(newdata)
}










