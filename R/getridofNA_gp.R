#### getridofNA_gp function
#### aoianalysis package
#### Created by Nicole Burke
#### 08/2018-11/2018

#### What does function do? #####
# Takes NeccessaryAOI (handAOI) and the other AOI columns
#   and gets rid of NAs. It replaces NAs with 999.

# Why? Because the boolean evaluations gets mad in the
#     goal prediction first look function.

library(tidyverse)
# Get rid of NAs in  and AOI columns
getridofNA_gp <- function(dataframe, HandAOI, AOI1, AOI2) {
  dataframe <- dataframe %>%
    mutate(HandAOI = ifelse(is.na(HandAOI), 999, HandAOI),
           AOI1 = ifelse(is.na(AOI1), 999, AOI1),
           AOI2 = ifelse(is.na(AOI2), 999, AOI2))
  return(dataframe)
}
