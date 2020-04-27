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

avgTemp <- weather %>% 
  mutate(Month = str_sub(Date, 1, 7)) %>% 
  group_by(Month) %>% 
  summarise(
    AvgHigh = mean(High)
  )

modelingDemographics <- demographics %>% 
  select(Neighborhood, Population)

monthNeighborhoodGrid <- expand.grid(
  Month = unique(str_sub(incidents$Date, 1, 7)),
  Neighborhood = unique(incidents$Neighborhood)
)

neighborhoodMonthlyIncidents <- incidents %>% 
  mutate(Month = str_sub(Date, 1, 7)) %>% 
  count(Month, Neighborhood) %>% 
  rename(Incidents = n) %>% 
  right_join(monthNeighborhoodGrid, c('Month', 'Neighborhood')) %>% 
  mutate(
    Incidents = ifelse(is.na(Incidents), 0, Incidents)
  ) %>% 
  arrange(Month, Neighborhood) %>% 
  filter(Neighborhood != '') %>% 
  left_join(avgTemp, 'Month') %>% 
  left_join(modelingDemographics, 'Neighborhood')

neighborhoodMonthlyIncidentsWithPop <- neighborhoodMonthlyIncidents %>% 
  filter(!is.na(Population))


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


#### Neighborhood mean model ####


data <- list('n', 'y', 'neighborhood', 'J')
n <- nrow(neighborhoodMonthlyIncidents)
y <- neighborhoodMonthlyIncidents$Incidents
neighborhood <- as.numeric(as.factor(neighborhoodMonthlyIncidents$Neighborhood))
J <- max(neighborhood)

inits <- function() {
  list(sigma.y=runif(1), sigma.a = runif(1), mu.a=rnorm(1))
}
parameters = c('mu', 'sigma.y', 'mu.a', 'sigma.a')
neighborhoodMeanModel <- bugs(data, inits, parameters, 'neighborhood-mean-model.txt',
                              working.directory = 'bugs-models/', n.chains=3, n.iter=1000, bugs.seed = 1)
plot(neighborhoodMeanModel)
print(neighborhoodMeanModel)


#### Neighbothood basic poisson model ####

data <- list('y', 'n', 'offset', 'temperature')
y <- neighborhoodMonthlyIncidentsWithPop$Incidents
n <- nrow(neighborhoodMonthlyIncidentsWithPop)
offset <- log(neighborhoodMonthlyIncidentsWithPop$Population)
temperature <- neighborhoodMonthlyIncidentsWithPop$AvgHigh

inits <- function() {
  list(sigma.epsilon=0.001, b=rnorm(1), mu=rnorm(1))
}

parameters <- c('mu', 'b', 'sigma.epsilon', 'epsilon')

basicPoissonModel <- bugs(data, inits, parameters, 'basic-poisson-model.txt', working.directory ='bugs-models/',
                          n.chains = 3, n.iter=3, bugs.seed=1)


#### Neighborhood varying poisson model ####

data <- list('y', 'n', 'offset', 'neighborhood', 'temperature', 'J')
y <- neighborhoodMonthlyIncidentsWithPop$Incidents
n <- nrow(neighborhoodMonthlyIncidentsWithPop)
offset <- log(neighborhoodMonthlyIncidentsWithPop$Population)
neighborhood <- as.numeric(as.factor(neighborhoodMonthlyIncidentsWithPop$Neighborhood))
temperature <- neighborhoodMonthlyIncidentsWithPop$AvgHigh
J <- max(neighborhood)

inits <- function() {
  list(sigma.a=runif(1), mu.a=rnorm(1), sigma.epsilon=runif(1), b=rnorm(1))
}
parameters = c('mu.neighborhood', 'b', 'mu.a', 'sigma.a')
varyingInterceptPoisson <- bugs(data, inits, parameters, 'varying-intercept-model.txt',
                                working.directory = 'bugs-models/', n.chains = 3, n.iter = 1000, bugs.seed = 1)
