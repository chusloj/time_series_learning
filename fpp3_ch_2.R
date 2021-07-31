library(tidyverse)
library(dplyr)
library(ggplot2)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
library(fpp3)

setwd("~/Desktop/coding/r_ts")

set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + labs(title = "White noise", y = "")

y %>%
  ACF(wn) %>%
  autoplot() + labs(title = "White noise")

### EXERCISES

gafa_stock %>%
  group_by(Symbol) %>%
  filter(Adj_Close == max(Adj_Close))

tute1 <- read_csv("tute1.csv")

mytimeseries <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)

mytimeseries %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

library(USgas)
ust <- us_total %>%
  as_tsibble(index = year, key = state)

ust %>%
  filter(state %in% c("Maine", "Vermont", "New Hampshire",
                      "Massachusetts", "Connecticut", "Rhode Island")
         ) %>%
  ggplot(aes(x = year, y=y, color = state)) +
  geom_line()

autoplot(aus_arrivals)
gg_season(aus_arrivals)
gg_subseries(aus_arrivals)


pig <- aus_livestock %>%
  mutate(year = year(Month),
         month = month(Month)) %>%
  filter(State == "Victoria",
         Animal == "Pigs",
         year %in% 1990:1995)

autoplot(pig)
ACF(pig) %>% autoplot()

