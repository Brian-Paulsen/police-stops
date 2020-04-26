library(R2OpenBUGS)
library(tidyverse)

incidents <- read.csv('../../data/processed/master-incidents.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(BeginDateTime))

demographics <- read.csv('../../data/processed/neighborhood-demographics.csv', stringsAsFactors = FALSE)

weather <- read.csv('../../data/processed/cleaned-weather.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(Date))

dailyIncidents <- incidents %>% 
  count(Date) %>% 
  rename(Incidents=n)

#### Daily mean model ####
n <- nrow(dailyIncidents)
y <- dailyIncidents$Incidents

data <- list('n', 'y')
inits <- function() {
  list(sigma.y = runif(1), mu = rnorm(1))
}
parameters <- c('mu', 'sigma.y')
run1 <- bugs(data, inits, parameters, 'mean-model.txt', working.directory='bugs-models/',
            n.chains=3, n.iter=100, bugs.seed=1)
plot(run1)
print(run1)

