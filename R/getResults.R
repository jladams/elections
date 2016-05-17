library(shapefiles)
library(dplyr)
library(tidyr)
library(lazyeval)

# Donwloaded from http://www.american.edu/spa/ccps/Data-Sets.cfm
results00 <- read.csv("./data/raw/2000.csv")
# Downloaded from https://catalog.data.gov/dataset/2004-presidential-general-election-county-results-direct-download
df04 <- getResults(2004, "VOTE_DEM", "VOTE_REP", "VOTE_OTH")
# Downloaded from https://catalog.data.gov/dataset/2008-presidential-general-election-county-results-direct-download
df08 <- getResults(2008, "VOTE_DEM", "VOTE_REP", "VOTE_OTH")
# Downloaded from https://catalog.data.gov/dataset/presidential-general-election-results-2012-direct-download
df12 <- getResults(2012, "OBAMA", "ROMNEY", "OTHERS")

results00$YEAR <- 2000
counties00 <- results00 %>%
  group_by(state=STATE, county=COUNTY, year=YEAR) %>%
  summarize(dem=sum(GORE), rep=sum(BUSH), other=sum(OTHER)) %>%
  gather(party,votes, dem:other)

states00 <- counties00 %>%
  group_by(state,party,year) %>%
  summarize(votes=sum(votes))

total00 <- states00 %>%
  group_by(party, year) %>%
  summarize(votes=sum(votes))

getResults <- function(yr, dem_col, rep_col, oth_col) {
  resultsYear <- read.dbf(paste("./data/dbf/", yr, ".dbf", sep=""))
  resultsYear <- resultsYear$dbf
  resultsYear$YEAR <- yr
  resultsYear <- resultsYear %>% 
    distinct(FIPS) %>% 
    na.omit() %>%
    filter(FIPS != 15005)
  
  resultsYear[] <- lapply(resultsYear, function(x) type.convert(as.character(x)))
  
  countiesYear <- resultsYear %>% 
    group_by(state=STATE, county=COUNTY, year=YEAR) %>% 
    summarize_(dem=interp(as.name(dem_col)),rep=interp(as.name(rep_col)),other=interp(as.name(oth_col))) %>%
    gather(party, votes, dem:other)
  
  statesYear <- countiesYear %>% 
    group_by(state, party, year) %>% 
    summarize(votes=sum(votes))
    
  totalYear <- statesYear %>% 
    group_by(party, year) %>% 
    summarize(votes=sum(votes))
  
  write.csv(countiesYear, paste("./data/csv/counties", yr, ".csv", sep = ""), row.names = FALSE)
  write.csv(statesYear, paste("./data/csv/states", yr, ".csv", sep = ""), row.names=FALSE)
  write.csv(totalYear, paste("./data/csv/total", yr, ".csv", sep = ""), row.names = FALSE)
  
  return(countiesYear)

}

countiesAll <- rbind(counties00, df04, df08, df12)

statesAll <- countiesAll %>%
  group_by(state, party, year) %>%
  summarize(votes=sum(votes))

totalAll <- statesAll %>%
  group_by(party, year) %>%
  summarize(votes=sum(votes))

write.csv(counties00, "./data/csv/counties2000.csv", row.names = FALSE)
write.csv(states00, "./data/csv/states2000.csv", row.names=FALSE)
write.csv(total00, "./data/csv/total2000.csv", row.names = FALSE)

write.csv(countiesAll, "./data/csv/countiesAll.csv", row.names = FALSE)
write.csv(statesAll, "./data/csv/statesAll.csv", row.names=FALSE)
write.csv(totalAll, "./data/csv/totalAll.csv", row.names = FALSE)