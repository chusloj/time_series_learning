library(tidyverse)
library(dplyr)
library(ggplot2)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
library(fpp3)
library(glue)
library(GGally)

gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)

gdppc %>%
  filter(Country == "Sweden") %>%
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

fit <- gdppc %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit %>%
  forecast(h = "3 years") %>%
  filter(Country == "Sweden") %>%
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")

bricks <- aus_production %>%
  filter_index("1970 Q1" ~ "2004 Q4")

bricks %>% model(MEAN(Bricks)) %>% forecast(h = 10) %>% autoplot(bricks)
bricks %>% model(NAIVE(Bricks)) %>% forecast(h = 10) %>% autoplot(bricks)
bricks %>% model(SNAIVE(Bricks ~ lag("year"))) %>% forecast(h = 10) %>% autoplot(bricks)
bricks %>% model(RW(Bricks ~ drift())) %>% forecast(h = 10) %>% autoplot(bricks)

google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)

google_2015 %>%
  model(NAIVE(Close)) %>%
  gg_tsresiduals()



us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade")
fit_dcmp <- us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE)
    # NAIVE(season_adjust)
  ))
fit_dcmp %>%
  forecast() %>%
  autoplot(us_retail_employment)+
  labs(y = "Number of people",
       title = "Monthly US retail employment")
