library(shapefiles)
library(dplyr)
library(tidyr)
library(lazyeval)

getResults <- function(year, dem_col, rep_col, oth_col) {
  resultsYear <- read.dbf(paste("./data/dbf/", year, ".dbf", sep=""))
  resultsYear <- resultsYear$dbf
  resultsYear <- resultsYear %>% 
    distinct(FIPS) %>% 
    na.omit() %>%
    filter_(~dc != -9999)
  
  resultsYear[] <- lapply(resultsYear, function(x) type.convert(as.character(x)))
  
  countiesYear <- resultsYear %>% 
    group_by(state=STATE, county=COUNTY) %>% 
    summarize_(dem=dc,rep=rc,other=oc) %>%
    gather(party, votes, dem:other)
  
  statesYear <- countiesYear %>% 
    group_by(state, party) %>% 
    summarize(votes=sum(votes))
  
  totalYear <- statesYear %>% 
    group_by(party) %>% 
    summarize(votes=sum(votes))
  
  write.csv(countiesYear, paste("./data/csv/counties", year, ".csv", sep = ""), row.names = FALSE)
  write.csv(statesYear, paste("./data/csv/states", year, ".csv", sep = ""), row.names=FALSE)
  write.csv(totalYear, paste("./data/csv/total", year, ".csv", sep = ""), row.names = FALSE)
}

getResults(2004, VOTE_DEM, VOTE_REP, VOTE_OTH)
getResults(2008, VOTE_DEM, VOTE_REP, VOTE_OTH)
getResults(2012, OBAMA, ROMNEY, OTHERS)
