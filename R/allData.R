library(shapefiles)
library(dplyr)
library(tidyr)

# Downloaded from https://catalog.data.gov/dataset/2004-presidential-general-election-county-results-direct-download
results2004 <- read.dbf("./data/dbf/2004.dbf")
results2004 <- results2004$dbf
results2004$YEAR <- 2004
results2004 <- results2004 %>% 
  distinct(FIPS) %>% 
  na.omit() %>% 
  filter(VOTE_DEM != -9999)

results2004[] <- lapply(results2004, function(x) type.convert(as.character(x)))

counties2004 <- results2004 %>% 
  group_by(state=STATE, county=COUNTY, year=YEAR) %>% 
  summarize(dem=VOTE_DEM,rep=VOTE_REP,other=VOTE_OTH) %>%
  gather(party, votes, dem:other)

states2004 <- counties2004 %>% 
  group_by(state, party, year) %>% 
  summarize(votes=sum(votes))

total2004 <- states2004 %>% 
  group_by(party, year) %>% 
  summarize(votes=sum(votes))

# Downloaded from https://catalog.data.gov/dataset/2008-presidential-general-election-county-results-direct-download
results2008 <- read.dbf("./data/dbf/2008.dbf")
results2008 <- results2008$dbf
results2008$YEAR <- 2008
results2008 <- results2008 %>% 
  distinct(FIPS) %>% 
  na.omit() %>%
  filter(VOTE_DEM != -9999)

results2008[] <- lapply(results2008, function(x) type.convert(as.character(x)))

counties2008 <- results2008 %>% 
  group_by(state=STATE, county=COUNTY, year=YEAR) %>% 
  summarize(dem=VOTE_DEM,rep=VOTE_REP,other=VOTE_OTH) %>%
  gather(party, votes, dem:other)

states2008 <- counties2008 %>% 
  group_by(state, party, year) %>% 
  summarize(votes=sum(votes))

total2008 <- states2008 %>% 
  group_by(party, year) %>% 
  summarize(votes=sum(votes))

# Downloaded from https://catalog.data.gov/dataset/presidential-general-election-results-2012-direct-download
results2012 <- read.dbf("./data/dbf/2012.dbf")
results2012 <- results2012$dbf
results2012$YEAR <- 2012
results2012 <- results2012 %>% 
  distinct(FIPS) %>% 
  na.omit()

counties2012 <- results2012 %>% 
  group_by(state=STATE, county=COUNTY, year=YEAR) %>% 
  summarize(dem=OBAMA,rep=ROMNEY,other=OTHERS) %>%
  gather(party, votes, dem:other)

states2012 <- counties2012 %>% 
  group_by(state, party, year) %>% 
  summarize(votes=sum(votes))

total2012 <- states2012 %>% 
  group_by(party, year) %>% 
  summarize(votes=sum(votes))

countiesAll <- rbind(counties2004, counties2008, counties2012)

statesAll <- countiesAll %>%
  group_by(state, party, year) %>%
  summarize(votes=sum(votes))

totalAll <- statesAll %>%
  group_by(party, year) %>%
  summarize(votes=sum(votes))

write.csv(counties2004, "./data/csv/counties2004.csv", row.names = FALSE)
write.csv(states2004, "./data/csv/states2004.csv", row.names=FALSE)
write.csv(total2004, "./data/csv/total2004.csv", row.names = FALSE)

write.csv(counties2008, "./data/csv/counties2008.csv", row.names = FALSE)
write.csv(states2008, "./data/csv/states2008.csv", row.names=FALSE)
write.csv(total2008, "./data/csv/total2008.csv", row.names = FALSE)

write.csv(counties2012, "./data/csv/counties2012.csv", row.names = FALSE)
write.csv(states2012, "./data/csv/states2012.csv", row.names=FALSE)
write.csv(total2012, "./data/csv/total2012.csv", row.names = FALSE)

write.csv(countiesAll, "./data/csv/countiesAll.csv", row.names = FALSE)
write.csv(statesAll, "./data/csv/statesAll.csv", row.names=FALSE)
write.csv(totalAll, "./data/csv/totalAll.csv", row.names = FALSE)