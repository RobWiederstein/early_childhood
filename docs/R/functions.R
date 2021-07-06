# 1.0 general functions ----
convert_file_names <- function(path){
        from <- list.files(path = path, full.names = T)
        to <- from %>% gsub(" |, ", "_", .) %>% tolower
        if(any(!file.exists(to))){file.rename(from = from, to = to)}
}
# 2.0 birth indicators ----
breastfeeding <- function(){
        path <- "./data/raw/new_mothers_breastfeeding.xlsx"
        col_types <- c("text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame)&
                                             location_type == "County")
        df.1$variable <- "breastfeeding"
        df.1
}
live_births <- function(){
        path <- "./data/raw/live_births.xlsx"
        col_types <- c("text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame)&
                                             location_type == "County")
        df.1$variable <- "live_births"
        df.1
}
low_birth_weight <- function(){
        path <- "./data/raw/low_birth-weight_babies.xlsx"
        col_types <- c("text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame)&
                                             location_type == "County")
        df.1$variable <- "low_birth_weight"
        df.1
}
very_low_birth_weight <- function(){
        path <- "./data/raw/very_low_birth-weight_babies.xlsx"
        df <- readxl::read_xlsx(path = path, na = "LNE")#, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>%
                mutate(time_frame = time_frame %>% as.integer) %>%
                mutate(data = data %>% as.numeric) %>%
                dplyr::filter(time_frame == max(time_frame) &
                                             location_type == "County")
        df.1$variable <- "very_low_birth_weight"
        df.1

}
medicaid_mothers <- function(){
        path <- "./data/raw/births_to_mothers_on_medicaid.xlsx"
        col_types <- c("text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame) &
                                             location_type == "County")
        df.1$variable <- "mothers_medicaid"
        df.1
}
prenatal_care <- function(){
        path <- "./data/raw/mothers_who_received_first_trimester_prenatal_care_(2007-2017).xlsx"
        col_types <- c("text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame)&
                                             location_type == "County")
        df.1$variable <- "prenatal_care"
        df.1
}
preterm_births <- function(){
        path <- "./data/raw/preterm_births.xlsx"
        col_types <- c("text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame)&
                                             location_type == "County")
        df.1$variable <- "preterm_births"
        df.1
}
smoking_mothers <- function(){
        path <- "./data/raw/mothers_who_reported_smoking_during_pregnancy_(2007-2017).xlsx"
        col_types <- c("text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame)&
                                             location_type == "County")
        df.1$variable <- "smoking_mothers"
        df.1
}
teen_births <- function(){
        path <- "./data/raw/teen_birth_rate.xlsx"
        col_types <- c("text", "text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame) &
                                             location_type == "County" &
                                             age_group == "Ages 15-19")
        df.1$variable <- "teen_births_15_19"
        df.2 <- df.1 %>% select(-age_group)
        df.3 <- df.2 %>% mutate(data_format = "percent",
                                data = data / 1000)
        df.3
}
race_ethnic <- function(){
        path <- "./data/raw/live_births_by_race_and_ethnicity.xlsx"
        col_types <- c("text", "text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame) &
                                             location_type == "County")
        df.2 <- df.1 %>%
                pivot_wider(names_from = "race",
                            values_from = "data") %>%
                rename_with(tolower) %>%
                mutate(pct_white = divide_by(white, total) %>% round(3),
                       pct_black = divide_by(black, total) %>% round(3),
                       pct_hisp =  divide_by(hispanic, total) %>% round(3),
                       data_format = "percent"
                       )

        df.3 <- df.2 %>%
                select(location_type:data_format,
                       pct_white, pct_black, pct_hisp) %>%
                pivot_longer(cols = c(pct_white, pct_black, pct_hisp),
                             names_to = "variable",
                             values_to = "data")
        df.3
}
unmarried <- function(){
        path <- "./data/raw/births_to_unmarried_parents.xlsx"
        col_types <- c("text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame) &
                                             location_type == "County"
                                     )
        df.1$variable <- "unmarried"
        df.1
}
infant_deaths <- function(){
        path <- "./data/raw/infant_deaths.xlsx"
        col_types <- c("text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame) &
                                             location_type == "County"
        )
        df.1$variable <- "infant_deaths"
        df.1
}
# 3.0 early childcare ----
capacity_licensed_centers_homes <- function(){
        path <- "./data/raw/ec/capacity_of_licensed_child_care_centers_and_homes.xlsx"
        col_types <- c("text", "text", "numeric", "text", "numeric")
        df <- readxl::read_xlsx(path = path, col_types = col_types)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>% dplyr::filter(time_frame == max(time_frame) &
                                             location_type == "County"
        )
        df.1$variable <- "cap_lic_ctrs_homes"
        df.1
}
child_care_facilities_by_type <- function(){
        path <- "./data/raw/ec/child_care_facilities_by_type.xlsx"
        df <- readxl::read_xlsx(path = path)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <- df %>%
                mutate(time_frame = time_frame %>% as.integer,
                       data = data %>% as.integer) %>%
                dplyr::filter(time_frame == max(time_frame) &
                                             location_type == "County"
        ) %>%
                rename(variable = child_care_facilities)
}
child_pop_0_to_4 <- function(){
        path <- "./data/raw/ec/child_population_by_age_group.xlsx"
        df <- readxl::read_xlsx(path = path)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <-
                df %>%
                dplyr::filter(age_group == "Ages 0-4") %>%
                dplyr::filter(time_frame == max(as.integer(time_frame))) %>%
                dplyr::filter(location_type == "County") %>%
                dplyr::filter(data_format == "Number") %>%
                rename(variable = age_group) %>%
                mutate(time_frame = time_frame %>% as.integer,
                       data = data %>% as.integer)
        df.1
}
child_pop_race_ethnicity <- function(){
        #https://bit.ly/3y28if1
        path <- "./data/raw/ec/child_population_by_race_and_ethnicity.xlsx"
        df <- readxl::read_xlsx(path = path)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <-
                df %>%
                dplyr::filter(time_frame == max(as.integer(time_frame))) %>%
                dplyr::filter(location_type == "County") %>%
                dplyr::filter(data_format == "Number") %>%
                rename(variable = race) %>%
                mutate(time_frame = time_frame %>% as.integer,
                       data = data %>% as.integer)
        #change variable names
        df.1$variable <- gsub(" (of any race)", "", df.1$variable, fixed = T)
        df.1$variable <- gsub("American Indian", "Am_Indian", df.1$variable)
        df.1$variable <- gsub("Total", "total_child", df.1$variable)
        df.1$variable <- tolower(df.1$variable)

        df.1
}
children_receiving_child_care_vouchers <- function(){
        path <- "./data/raw/ec/children_receiving_child_care_vouchers.xlsx"
        df <- readxl::read_xlsx(path = path)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <-
                df %>%
                mutate(across(c(time_frame, data), as.integer)) %>%
                dplyr::filter(time_frame == max(time_frame)) %>%
                dplyr::filter(location_type == "County") %>%
                mutate(variable = "child_care_vouchers")
        df.1
}
children_served_by_first_steps <- function(){
     path <- "./data/raw/ec/children_served_by_first_steps.xlsx"
     df <- readxl::read_xlsx(path = path)
     names(df) <- janitor::make_clean_names(names(df))
     df.1 <-
             df %>%
             mutate(across(c(time_frame, data), as.integer)) %>%
             dplyr::filter(time_frame == max(time_frame)) %>%
             dplyr::filter(location_type == "County") %>%
             mutate(variable = "first_steps")
     df.1
}
head_start_slots <- function(){
        path <- "./data/raw/ec/early_head_start_and_head_start_funded_enrollment_slots.xlsx"
        df <- readxl::read_xlsx(path = path)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <-
                df %>%
                mutate(across(c(time_frame, data), as.integer)) %>%
                dplyr::filter(time_frame == max(time_frame)) %>%
                dplyr::filter(location_type == "County") %>%
                dplyr::filter(head_start != "Total") %>%
                rename(variable = head_start)
        df.1
}
licensed_child_care_slots_per_100_child <- function(){
        path <- "./data/raw/ec/licensed_child_care_slots_per_100_children_ages_0-5.xlsx"
        df <- readxl::read_xlsx(path = path)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <-
                df %>%
                mutate(across(c(time_frame, data), as.integer)) %>%
                dplyr::filter(time_frame == max(time_frame)) %>%
                dplyr::filter(location_type == "County") %>%
                mutate(data = divide_by(data, 100)) %>%
                mutate(variable = "child_care_slots_0-5")
        df.1
}
monthly_avg_children_on_waiting_list <- function(){
        path <- "./data/raw/ec/monthly_average_number_of_children_on_waiting_list_for_child_care_vouchers.xlsx"
        df <- readxl::read_xlsx(path = path)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <-
                df %>%
                mutate(across(c(time_frame, data), as.integer)) %>%
                dplyr::filter(time_frame == max(time_frame)) %>%
                dplyr::filter(location_type == "County") %>%
                mutate(variable = "waiting_list")
        df.1
}
wic_participants <- function(){
        path <- "./data/raw/ec/women_infants_and_children_(wic)_participants.xlsx"
        df <- readxl::read_xlsx(path = path)
        names(df) <- janitor::make_clean_names(names(df))
        df.1 <-
                df %>%
                mutate(across(c(time_frame, data), as.integer)) %>%
                dplyr::filter(time_frame == max(time_frame)) %>%
                dplyr::filter(location_type == "County") %>%
                dplyr::filter(data_format == "Number") %>%
                rename(variable = wic_category)
        df.1$variable <- gsub("Total Participants", "total_part", df.1$variable)
        df.1$variable <- paste("wic_", df.1$variable, sep = "")
        df.1
}






