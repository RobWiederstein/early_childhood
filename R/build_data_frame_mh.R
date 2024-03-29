## 1.0 load helper functions ----
source("./R/functions.R")

## 1.1 load libraries
library(tidyverse)

## 2.0 combine data sources ----
df <- bind_rows(breastfeeding(),
                infant_deaths(),
                live_births(),
                low_birth_weight(),
                medicaid_mothers(),
                prenatal_care(),
                preterm_births(),
                race_ethnic(),
                smoking_mothers(),
                teen_births(),
                unmarried(),
                very_low_birth_weight()
                )

## 3.0  convert from long to wide top 20 by live births ----
df.1 <-
        df %>%
        select(location, variable, data) %>%
        pivot_wider(names_from = variable,
                    id_cols = location,
                    values_from = data) %>%
        drop_na() %>%
        mutate(infant_deaths = divide_by(infant_deaths, live_births) %>% round(4)) %>%
        arrange(-live_births) %>%
        slice_head(n = 20) %>%
        select(-live_births)
## 4.0 write out dataframe to data/tidy ----
file <- "./data/tidy/2021-06-25-maternal-infant-health.csv"
write.csv(df.1, file = file, row.names = F)
file <- "~/Dropbox/public/datasets/2021-06-25-maternal-infant-health.csv"
write.csv(df.1, file = file, row.names = F)









