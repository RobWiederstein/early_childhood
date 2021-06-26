
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



