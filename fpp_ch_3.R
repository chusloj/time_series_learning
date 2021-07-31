library(tidyverse)
library(dplyr)
library(ggplot2)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
library(fpp3)

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")


us_retail_employment %>%
  model(
    STL(Employed ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


### EXERCISES

glbe <- global_economy %>%
  as_tsibble(index = Year, key = Country)

glbe %>%
  filter(Country == "United States") %>%
  ggplot(aes(x=Year, y=GDP, colour=Country)) + geom_line()

c_l <- canadian_gas %>%
  features(Volume, features = guerrero) %>%
  pull(lambda_guerrero)
  
autoplot(canadian_gas)
canadian_gas %>% autoplot(box_cox(Volume, c_l))

tobacco <- aus_production %>% select(Quarter, Tobacco)

t_l <- tobacco %>%
  features(Tobacco, features = guerrero) %>%
  pull(lambda_guerrero)

autoplot(tobacco)
tobacco %>% autoplot(box_cox(Tobacco, t_l))


gas <- tail(aus_production, 5*4) %>% select(Gas)

autoplot(gas)

gas %>%
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) %>%
  as_tsibble() %>%
  components() %>%
  ggplot(aes(x=Quarter, y=season_adjust)) + geom_line()

# add an outlier
gas2 <- gas
gas2$Gas[2] <- gas2$Gas[20] + 100
gas2 %>%
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) %>%
  components() %>%
  as_tsibble() %>%
  ggplot(aes(x=Quarter, y=season_adjust)) + geom_line()


autoplot(canadian_gas)
gg_subseries(canadian_gas)
gg_season(canadian_gas)

canadian_gas %>%
  model(
    STL(Volume ~ trend(window = 7) +
          season(window = ),
        robust = TRUE)) %>%
  components() %>%
  autoplot()
