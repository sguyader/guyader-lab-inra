library(tidyverse)
library(tidycovid19)
library(zoo)

library(skimr)

df <- download_merged_data(cached = TRUE, silent = TRUE) %>%
  filter(date < "2021-05-01")

skim(df)

head(df)
