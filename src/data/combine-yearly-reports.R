library(tidyverse)
library(chron)

incidents2010 <- read.csv('../../data/raw/Police_Incidents_2010.csv', stringsAsFactors = FALSE)
incidents2011 <- read.csv('../../data/raw/Police_Incidents_2011.csv', stringsAsFactors = FALSE)
incidents2012 <- read.csv('../../data/raw/Police_Incidents_2012.csv', stringsAsFactors = FALSE)
incidents2013 <- read.csv('../../data/raw/Police_Incidents_2013.csv', stringsAsFactors = FALSE)
incidents2014 <- read.csv('../../data/raw/Police_Incidents_2014.csv', stringsAsFactors = FALSE)
incidents2015 <- read.csv('../../data/raw/Police_Incidents_2015.csv', stringsAsFactors = FALSE)
incidents2016 <- read.csv('../../data/raw/Police_Incidents_2016.csv', stringsAsFactors = FALSE)
incidents2017 <- read.csv('../../data/raw/Police_Incidents_2017.csv', stringsAsFactors = FALSE)
incidents2018Caprs <- read.csv('../../data/raw/Police_Incidents_2018.csv', stringsAsFactors = FALSE)
incidents2018Pims <- read.csv('../../data/raw/Police_Incidents_2018_PIMS.csv', stringsAsFactors = FALSE)
incidents2019 <- read.csv('../../data/raw/Police_Incidents_2019.csv', stringsAsFactors = FALSE)


caprsData <- bind_rows(incidents2010, incidents2011, incidents2012, incidents2013, incidents2014, incidents2015,
                       incidents2016, incidents2017, incidents2018Caprs)
pimsData <- bind_rows(incidents2018Pims, incidents2019)

pimsCleaned <- pimsData %>% 
  mutate(
    BeginDate = as.Date(beginDate),
    BeginHour = str_pad(beginTime %/% 60, 2, 'left', 0),
    BeginMinute = str_pad(beginTime %% 60, 2, 'left', 0),
    BeginDateTime = paste0(BeginDate, 'T', BeginHour, ':', BeginMinute, ':', '00.000Z'),
    CaseNumber = str_remove_all(caseNumber, ' ')
  ) %>% 
  select(Long=X, Lat=Y, PublicAddress=publicaddress, CaseNumber, Precinct=precinct, 
         ReportedDateTime=reportedDateTime, BeginDateTime, Offense=offense, Description=description, 
         UCRCode, GBSID=centergbsid, CenterLong=centerLong, CenterLat=centerLat, X=centerX, Y=centerY, 
         Neighborhood=neighborhood)

pimsCleaned$Precinct[pimsCleaned$Precinct == 'UI    '] <- -1
pimsCleaned$Precinct <- as.numeric(pimsCleaned$Precinct)

caprsCleaned <- caprsData %>% 
  rename(CaseNumber=CCN, ReportedDateTime=ReportedDate, BeginDateTime=BeginDate) %>% 
  select(-Time, -EnteredDate, -LastChanged, -LastUpdateDate, -ESRI_OID, -OBJECTID, -ControlNbr)

combinedData <- bind_rows(pimsCleaned, caprsCleaned)

combinedData$Offense <- str_trim(combinedData$Offense)
combinedData$Neighborhood <- toupper(combinedData$Neighborhood)
combinedData$Neighborhood[combinedData$Neighborhood == 'STEVENS SQUARE - LORING HEIGHTS'] <- 
  'STEVEN\'S SQUARE - LORING HEIGHTS'

write.csv(combinedData, '../../data/processed/master-incidents.csv', row.names=FALSE)