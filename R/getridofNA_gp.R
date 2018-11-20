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
getridofNA_gp <- function(dataframe, HandAOI, AOI1, AOI2) {
  library(tidyverse)
  # Change variable input to work with in the function
  handAOI <- dataframe[HandAOI]
  aoi1 <- dataframe[AOI1]
  aoi2 <- dataframe[AOI2]
  # Make new dataframe
  newdata <- cbind(handAOI, aoi1, aoi2)
  # Get rid of NAs
  newdata <- newdata %>%
    mutate(handAOI = ifelse(is.na(handAOI), 999, handAOI),
           AOI1 = ifelse(is.na(AOI1), 999, AOI1),
           AOI2 = ifelse(is.na(AOI2), 999, AOI2))
  return(dataframe)
}
