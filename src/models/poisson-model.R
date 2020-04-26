library(R2WinBUGS)
library(tidyverse)

incidents <- read.csv('../../data/processed/master-incidents.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(BeginDateTime))

demographics <- read.csv('../../data/processed/neighborhood-demographics.csv', stringsAsFactors = FALSE)

weather <- read.csv('../../data/processed/cleaned-weather.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(Date))

