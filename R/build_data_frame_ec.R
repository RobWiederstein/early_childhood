## 1.0 load helper functions ----
source("./R/functions.R")

## 1.1 load libraries
library(tidyverse)

## 2.0 convert file names
convert_file_names(path = "./data/raw/ec")

## 2.0 combine data sources ----
df <- bind_rows(capacity_licensed_centers_homes(),
                child_care_facilities_by_type(),
                child_pop_0_to_4(),
                child_pop_race_ethnicity(),
                children_receiving_child_care_vouchers(),
                children_served_by_first_steps(),
                head_start_slots(),
                licensed_child_care_slots_per_100_child(),
                monthly_avg_children_on_waiting_list(),
                live_births(),
                wic_participants()
                )

## 3.0  convert from long to wide ----
df.1 <-
        df %>%
        select(location, variable, data) %>%
        pivot_wider(names_from = variable,
                    id_cols = location,
                    values_from = data) %>%
        drop_na() %>%
        rename_with(~ tolower(gsub(" |-", "_", .x))) %>%
        arrange(-ages_0_4) %>%
        slice_head(n = 20)

## 4.0 create features ----
df.2 <-
        df.1 %>%
        mutate(across(cap_lic_ctrs_homes:registered_ministry, ~ .x / ages_0_4, .names = "{.col}_per_cap")) %>%
        mutate(across(white:am_indian, ~ .x / total_child, .names = "pct_{.col}")) %>%
        mutate(across(child_care_vouchers:waiting_list, ~ .x / ages_0_4, .names = "{.col}_per_cap")) %>%
        mutate(across(c(child_care_vouchers:head_start,
                        waiting_list), ~ .x / ages_0_4, .names = "{.col}_per_cap")) %>%
        mutate(wic_breastfeeding_per_lb = wic_breastfeeding / live_births) %>%
        mutate(wic_child_per_cap = wic_child / ages_0_4) %>%
        mutate(across(wic_infant:wic_pregnant, ~ .x / live_births, .name = "{.col}_per_lb")) %>%
        mutate(wic_total_part_per_tot = wic_total_part / total_child)
## 5.0 select variables ----
df.3 <-
        df.2 %>%
        select(location,
               ages_0_4,
               cap_lic_ctrs_homes_per_cap:wic_total_part_per_tot
               )

## 6.0 write out dataframe to data/tidy ----
file <- "./data/tidy/2021-06-28-in-early-childhood.csv"
write.csv(df.3, file = file, row.names = F)
file <- "~/Dropbox/public/datasets/2021-06-28-in-early-childhood.csv"
write.csv(df.3, file = file, row.names = F)







