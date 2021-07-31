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


