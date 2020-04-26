library(tidyverse)

weatherDf <- read.csv('../../data/raw/weather.csv', stringsAsFactors = FALSE) %>% 
  rename(High=Maximum.Temperature.degrees..F.,
         Low = Minimum.Temperature.degrees..F.,
         CharPrecipitation = Precipitation..inches.,
         CharSnowPrecipitation = Snow..inches.,
         CharSnowDepth = Snow.Depth..inches.) %>% 
  mutate(
    Precipitation = ifelse(CharPrecipitation == 'T', 0, as.numeric(CharPrecipitation)),
    SnowPrecipitation = ifelse(CharSnowPrecipitation == 'T', 0, as.numeric(CharSnowPrecipitation)),
    SnowDepth = ifelse(CharSnowDepth == 'T', 0, as.numeric(CharSnowDepth)),
    IsPrecipitation = as.numeric(Precipitation > 0 | CharPrecipitation == 'T'),
    IsSnowPrecipitation = as.numeric(SnowPrecipitation > 0 | CharSnowPrecipitation == 'T'),
    IsGroundSnow = as.numeric(SnowDepth > 0 | CharSnowPrecipitation == 'T')
  ) %>% 
  select(-CharPrecipitation, -CharSnowPrecipitation, -CharSnowDepth)

write.csv(weatherDf, '../../data/processed/cleaned-weather.csv', row.names=FALSE)
