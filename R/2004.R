library(shapefiles)
library(dplyr)
library(tidyr)

# Downloaded from https://catalog.data.gov/dataset/2004-presidential-general-election-county-results-direct-download
results2004 <- read.dbf("./data/dbf/2004.dbf")
results2004 <- results2004$dbf
results2004 <- results2004 %>% 
  distinct(FIPS) %>% 
  na.omit() %>% 
  filter(VOTE_DEM != -9999)

results2004[] <- lapply(results2004, function(x) type.convert(as.character(x)))

counties2004 <- results2004 %>% 
  group_by(state=STATE, county=COUNTY) %>% 
  summarize(dem=VOTE_DEM,rep=VOTE_REP,other=VOTE_OTH) %>%
  gather(party, votes, dem:other)

states2004 <- counties2004 %>% 
  group_by(state, party) %>% 
  summarize(votes=sum(votes))

total2004 <- states2004 %>% 
  group_by(party) %>% 
  summarize(votes=sum(votes))

write.csv(counties2004, "./data/csv/counties2004.csv", row.names = FALSE)
write.csv(states2004, "./data/csv/states2004.csv", row.names=FALSE)
write.csv(total2004, "./data/csv/total2004.csv", row.names = FALSE)
