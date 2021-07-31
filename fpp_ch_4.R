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

tourism %>% features(Trips, feat_acf)

tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))


tourism_features <- tourism %>%
  features(Trips, feature_set(pkgs = "feasts"))

tourism_features %>%
  select_at(vars(contains("season"), Purpose)) %>%
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) %>%
  GGally::ggpairs(mapping = aes(colour = Purpose))

