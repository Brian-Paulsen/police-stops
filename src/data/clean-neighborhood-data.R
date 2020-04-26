library(tidyverse)

neighborhood2010 <- read.csv('../../data/raw/MSP Neighborhoods_2010.csv', stringsAsFactors = FALSE)
neighborhood2013 <- read.csv('../../data/raw/MSP Neighborhoods_2013-2017.csv', stringsAsFactors = FALSE)
incidents <- read.csv('../../data/processed/master-incidents.csv', stringsAsFactors = FALSE)

condensed2010 <- neighborhood2010 %>% 
  slice(2:35) %>% 
  select(
    Neighborhood = geography,
    Population = Total.population...Total...count,
    AgeU5 = Under.5.years...count,
    Age5To9 = X5.to.9.years...count,
    Age10To14 = X10.to.14.years...count,
    Age15To17 = X15.to.17.years...count,
    Age18To19 = X18.to.19.years...count,
    Age20To24 = X20.to.24.years...count,
    Age25To34 = X25.to.34.years...count,
    Age35To44 = X35.to.44.years...count,
    Age45To54 = X45.to.54.years...count,
    Age55To64 = X55.to.64.years...count,
    Age65To74 = X65.to.74.years...count,
    Age75To79 = X75.to.79.years...count,
    Age80To84 = X80.to.84.years...count,
    AgeO85 = X85.years.and.over...count,
    Male = Gender.and.age...Male...count,
    Female = Gender.and.age...Female...count,
    White = Race.and.Ethnicity...White.alone...count,
    Colored = Race.and.Ethnicity...Of.Color...count,
    Hispanic = Race.and.Ethnicity...Hispanic.or.Latino...count
  )

condensed2013 <- neighborhood2013 %>% 
  slice(2:69) %>% 
  select(
    Neighborhood = geography,
    Population = Total.population...Total...count,
    AgeU5 = Under.5.years...count,
    Age5To9 = X5.to.9.years...count,
    Age10To14 = X10.to.14.years...count,
    Age15To17 = X15.to.17.years...count,
    Age18To19 = X18.to.19.years...count,
    Age20To24 = X20.to.24.years...count,
    Age25To34 = X25.to.34.years...count,
    Age35To44 = X35.to.44.years...count,
    Age45To54 = X45.to.54.years...count,
    Age55To64 = X55.to.64.years...count,
    Age65To74 = X65.to.74.years...count,
    Age75To79 = X75.to.79.years...count,
    Age80To84 = X80.to.84.years...count,
    AgeO85 = X85.years.and.over...count,
    Male = Gender.and.age...Male...count.1,
    Female = Gender.and.age...Female...count.1,
    White = Race.and.Ethnicity...White.alone...count,
    Colored = Race.and.Ethnicity...Of.Color...count,
    Hispanic = Race.and.Ethnicity...Hispanic.or.Latino...count
  )

condensedData <- bind_rows(condensed2010, condensed2013)
condensedData$Neighborhood <- toupper(condensedData$Neighborhood)
condensedData$Neighborhood[condensedData$Neighborhood == 'BRYN-MAWR'] <- 'BRYN - MAWR'
condensedData$Neighborhood[condensedData$Neighborhood == 'CEDAR-ISLES-DEAN'] <- 'CEDAR - ISLES - DEAN'
condensedData$Neighborhood[condensedData$Neighborhood == 'COMO (MINNEAPOLIS)'] <- 'COMO'
condensedData$Neighborhood[condensedData$Neighborhood == 'LIND-BOHANON'] <- 'LIND - BOHANON'
condensedData$Neighborhood[condensedData$Neighborhood == 'MID-CITY INDUSTRIAL'] <- 'MID - CITY INDUSTRIAL'
condensedData$Neighborhood[condensedData$Neighborhood == 'NEAR NORTH'] <- 'NEAR - NORTH'
condensedData$Neighborhood[condensedData$Neighborhood == 'NICOLLET ISLAND-EAST BANK'] <- 'NICOLLET ISLAND - EAST BANK'
condensedData$Neighborhood[condensedData$Neighborhood == 'PROSPECT PARK-EAST RIVER ROAD'] <- 'PROSPECT PARK - EAST RIVER ROAD'
condensedData$Neighborhood[condensedData$Neighborhood == 'STEVEN\'S SQUARE-LORING HEIGHTS'] <- 'STEVEN\'S SQUARE - LORING HEIGHTS'
condensedData$Neighborhood[condensedData$Neighborhood == 'SUMNER-GLENWOOD'] <- 'SUMNER - GLENWOOD'
condensedData$Neighborhood[condensedData$Neighborhood == 'WEBBER-CAMDEN'] <- 'WEBBER - CAMDEN'
condensedData$Neighborhood[condensedData$Neighborhood == 'WILLARD-HAY'] <- 'WILLARD - HAY'


condensedDataNeighborhoods <- condensedData$Neighborhood
incidentsNeighborhoods <- unique(incidents$Neighborhood)
stPaulNeighborhoods <- setdiff(condensedDataNeighborhoods, incidentsNeighborhoods)

finalData <- condensedData %>% 
  filter(!(Neighborhood %in% stPaulNeighborhoods))

write.csv(finalData, '../../data/processed/neighborhood-demographics.csv', row.names=FALSE)
