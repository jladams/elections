library(shapefiles)
library(dplyr)
library(tidyr)

# Downloaded from https://catalog.data.gov/dataset/presidential-general-election-results-2012-direct-download
results2012 <- read.dbf("./data/dbf/2012.dbf")
results2012 <- results2012$dbf
results2012 <- results2012 %>% 
  distinct(FIPS) %>% 
  na.omit()

counties2012 <- results2012 %>% 
  group_by(state=STATE, county=COUNTY) %>% 
  summarize(dem=OBAMA,rep=ROMNEY,other=OTHERS) %>%
  gather(party, votes, dem:other)

states2012 <- counties2012 %>% 
  group_by(state, party) %>% 
  summarize(votes=sum(votes))

total2012 <- states2012 %>% 
  group_by(party) %>% 
  summarize(votes=sum(votes))

write.csv(counties2012, "./data/csv/counties2012.csv", row.names = FALSE)
write.csv(states2012, "./data/csv/states2012.csv", row.names=FALSE)
write.csv(total2012, "./data/csv/total2012.csv", row.names = FALSE)
