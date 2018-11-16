#### getridofNA function
#### aoianalysis package
#### Created by Nicole Burke
#### 08/2018-11/2018

#### What does function do? #####
# Takes 2 desired AOI columns and gets rid of NAs. It replaces NAs with 999.
getridofNA_help <- function(dataframe, AOI1, AOI2) {
  library(tidyverse)
  dataframe <- dataframe %>%
    # Take columns and replace NAs with 999
    mutate(AOI1 = ifelse(is.na(AOI1), 999, AOI1),
           AOI2 = ifelse(is.na(AOI2), 999, AOI2))
  return(dataframe)
}
