library(shapefiles)
library(dplyr)
library(tidyr)

# Downloaded from https://catalog.data.gov/dataset/2008-presidential-general-election-county-results-direct-download
results2008 <- read.dbf("./data/dbf/2008.dbf")
results2008 <- results2008$dbf
results2008 <- results2008 %>% 
  distinct(FIPS) %>% 
  na.omit() %>%
  filter(VOTE_DEM != -9999)

results2008[] <- lapply(results2008, function(x) type.convert(as.character(x)))

counties2008 <- results2008 %>% 
  group_by(state=STATE, county=COUNTY) %>% 
  summarize(dem=VOTE_DEM,rep=VOTE_REP,other=VOTE_OTH) %>%
  gather(party, votes, dem:other)

states2008 <- counties2008 %>% 
  group_by(state, party) %>% 
  summarize(votes=sum(votes))

total2008 <- states2008 %>% 
  group_by(party) %>% 
  summarize(votes=sum(votes))

write.csv(counties2008, "./data/csv/counties2008.csv", row.names = FALSE)
write.csv(states2008, "./data/csv/states2008.csv", row.names=FALSE)
write.csv(total2008, "./data/csv/total2008.csv", row.names = FALSE)
