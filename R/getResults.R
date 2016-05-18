library(shapefiles)
library(dplyr)
library(tidyr)
library(lazyeval)

# Tidy DBF files and export as CSV for county, state, and national levels
getDBF <- function(yr, dem_col, rep_col, oth_col) {

# Clean up NA values, remove invalid county from Hawaii
  resultsYear <- read.dbf(paste("./data/dbf/", yr, ".dbf", sep=""))
  resultsYear <- resultsYear$dbf
  resultsYear$YEAR <- yr
  resultsYear <- resultsYear %>% 
    distinct(FIPS) %>% 
    na.omit() %>%
    filter(FIPS != 15005)

  resultsYear[] <- lapply(resultsYear, function(x) type.convert(as.character(x)))

# County level data  
  countiesYear <- resultsYear %>% 
    group_by(state=STATE, county=COUNTY, year=YEAR) %>% 
    summarize_(dem=interp(as.name(dem_col)),rep=interp(as.name(rep_col)),other=interp(as.name(oth_col))) %>%
    gather(party, votes, dem:other)

# State level data  
  statesYear <- countiesYear %>% 
    group_by(state, party, year) %>% 
    summarize(votes=sum(votes))

# National level data  
  totalYear <- statesYear %>% 
    group_by(party, year) %>% 
    summarize(votes=sum(votes))

# Write CSVs for county, state, and national levels  
  write.csv(countiesYear, paste("./data/csv/counties", yr, ".csv", sep = ""), row.names = FALSE)
  write.csv(statesYear, paste("./data/csv/states", yr, ".csv", sep = ""), row.names=FALSE)
  write.csv(totalYear, paste("./data/csv/total", yr, ".csv", sep = ""), row.names = FALSE)

# Return county data as a data frame for use outside the function  
  return(countiesYear)
  
}


# Datasets
# Downloaded from http://www.american.edu/spa/ccps/Data-Sets.cfm
results00 <- read.csv("./data/raw/2000.csv")

# Downloaded from https://catalog.data.gov/dataset/2004-presidential-general-election-county-results-direct-download
df04 <- getDBF(2004, "VOTE_DEM", "VOTE_REP", "VOTE_OTH")

# Downloaded from https://catalog.data.gov/dataset/2008-presidential-general-election-county-results-direct-download
df08 <- getDBF(2008, "VOTE_DEM", "VOTE_REP", "VOTE_OTH")

# Downloaded from https://catalog.data.gov/dataset/presidential-general-election-results-2012-direct-download
df12 <- getDBF(2012, "OBAMA", "ROMNEY", "OTHERS")

# Tidy CSV data from 2000, similar to getDBF function above
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

# Write CSVs for 2000 data
write.csv(counties00, "./data/csv/counties2000.csv", row.names = FALSE)
write.csv(states00, "./data/csv/states2000.csv", row.names=FALSE)
write.csv(total00, "./data/csv/total2000.csv", row.names = FALSE)

# Take all county data and combine in single data frame
countiesAll <- rbind(counties00, df04, df08, df12)

# Total state data
statesAll <- countiesAll %>%
  group_by(state, party, year) %>%
  summarize(votes=sum(votes))

# Total national data
totalAll <- statesAll %>%
  group_by(party, year) %>%
  summarize(votes=sum(votes))

# Write CSVs for all data
write.csv(countiesAll, "./data/csv/countiesAll.csv", row.names = FALSE)
write.csv(statesAll, "./data/csv/statesAll.csv", row.names=FALSE)
write.csv(totalAll, "./data/csv/totalAll.csv", row.names = FALSE)