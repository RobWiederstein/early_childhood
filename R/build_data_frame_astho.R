# 1.0 FY 2018, 2017, 2016 ----
# Part 5 finances
#state general funds = FTEXSGF 01 = FY2016, 02 = FY2017, 03 = FY2018
#other state funds = FTEXOSF 01 = FY2016, 02 = FY2017, 03 = FY2018
# federal funds = FTEXFED 01 = FY2016, 02 = FY2017, 03 = FY2018
# other sources = FTEXOTH  01 = FY2016 02 = FY2017, 03 = FY2018
get_2018_2017_2016_astho_finances <- function(){
        path <- "./data/raw/astho/profile_data_and_codebooks/2019_states_astho_profile_dataset_final_icpsr.xlsx"
        df <- readxl::read_xlsx(path = path)
        df.1 <-
                df %>%
                select(starts_with(c("StateA", "FTEX")) & !FTEXCAV01) %>%
                setNames(gsubfn::gsubfn("01|02|03", list("01" = "2016",
                                                         "02" = "2017",
                                                         "03" = "2018"),
                                        names(.))) %>%
                pivot_longer(cols = (FTEXSGF2016:FTEXOTH2018),
                             names_to = "variable",
                             values_to = "value") %>%
                separate(col = variable,
                         into = c("variable", "year"),
                         sep = -4,
                         remove = F,
                         convert = T) %>%
                rename_all(tolower) %>%
                select(stateabbrev, year, variable, value) %>%
                arrange(year, stateabbrev)
        df.1
}
# 2.0 FY 2015 ----
get_2015_astho_finance <- function(){
        path <- "./data/raw/astho/profile_data_and_codebooks//2016_profile_dataset_states_updated_030221.xlsx"
        df <- readxl::read_xlsx(path = path)
        # FY15 = 02, 04, 06, 08, 10
        df.1 <-
                df %>%
                select(StateAbbrev, matches("FEXP...(02|04|06|08|10)") & !FEXPOTH10A) %>%
                pivot_longer(cols = FEXPCHR02:FEXPOTH10,
                             names_to = "variable",
                             values_to = "value") %>%
                mutate(year = 2015) %>%
                separate(col = variable,
                         into = c("variable", "source"),
                         sep = -2,
                         remove = F,
                         convert = T) %>%
                rename_all(tolower)
        df.2 <-
                df.1 %>%
                group_by(stateabbrev, year, source) %>%
                summarize(value = sum(value, na.rm = T), .groups = "drop")
        #Fees and fines category does not exist in the later years
        df.source <- data.frame(source = c(2, 4, 6, 8, 10),
                                variable = c("FTEXSGF",
                                             "FTEXOSF",
                                             "FTEXFED",
                                             "FTEXFNF",
                                             "FTEXOTH")
        )
        df.3 <- left_join(df.2, df.source, by = "source")
        df.4 <-
                df.3 %>%
                select(stateabbrev, year, variable, value) %>%
                pivot_wider(id_cols = c("stateabbrev", "year"),
                            names_from = variable,
                            values_from = value) %>%
                mutate(FTEXOTH = add(FTEXOTH, FTEXFNF)) %>%
                select(-FTEXFNF) %>%
                pivot_longer(cols = contains("FTEX"),
                             names_to = "variable",
                             values_to = "value")
        df.4
}

# 3.0  FY 2014 ----
get_2014_astho_finance <- function(){
        path <- "./data/raw/astho/profile_data_and_codebooks//2016_profile_dataset_states_updated_030221.xlsx"
        df <- readxl::read_xlsx(path = path)
        # FY14 = 01, 03, 05, 07, 09
        df.1 <-
                df %>%
                select(StateAbbrev, matches("FEXP...0[13579]") & !FEXPCAV01) %>%
                pivot_longer(cols = FEXPCHR01:FEXPOTH09,
                             names_to = "variable",
                             values_to = "value") %>%
                mutate(year = 2014) %>%
                separate(col = variable,
                         into = c("variable", "source"),
                         sep = -2,
                         remove = F,
                         convert = T) %>%
                rename_all(tolower)
        df.2 <-
                df.1 %>%
                group_by(stateabbrev, year, source) %>%
                summarize(value = sum(value, na.rm = T), .groups = "drop")
        #Fees and fines category not exist in the later years
        df.source <- data.frame(source = c(1, 3, 5, 7, 9),
                                variable = c("FTEXSGF",
                                         "FTEXOSF",
                                         "FTEXFED",
                                         "FTEXFNF",
                                         "FTEXOTH")
                                )
        df.3 <- left_join(df.2, df.source, by = "source")
        df.4 <-
                df.3 %>%
                select(stateabbrev, year, variable, value) %>%
                pivot_wider(id_cols = c("stateabbrev", "year"),
                            names_from = variable,
                            values_from = value) %>%
                mutate(FTEXOTH = add(FTEXFNF, FTEXOTH)) %>%
                select(-FTEXFNF) %>%
                pivot_longer(cols = contains("FTEX"),
                             names_to = "variable",
                             values_to = "value")
        df.4
}
# 4.0 FY 2011 ----
get_2011_astho_finance <- function(){
        path <- paste0("./data/raw/astho/profile_data_and_codebooks/",
                       "2012_astho_profile_dataset-states_updated.xlsx" )
        df <- readxl::read_xlsx(path = path, na = "#NULL!")

        df.1 <-
                df %>%
                select(CSHASTAT, matches("FEXP...(02|04|06|08|10)")) %>%
                pivot_longer(cols = FEXPCHR02:FEXPOTH10,
                                     names_to = "variable",
                                     values_to = "value") %>%
                mutate(year = 2011) %>%
                separate(col = variable,
                         into = c("variable", "source"),
                         sep = -2,
                         remove = F,
                         convert = T) %>%
                rename_all(tolower) %>%
                rename(stateabbrev = cshastat)
        df.2 <-
                df.1 %>%
                group_by(stateabbrev, year, source) %>%
                summarize(value = sum(value, na.rm = T), .groups = "drop")
        #Fees and fines category does not exist in the later years
        df.source <- data.frame(source = c(2, 4, 6, 8, 10),
                                variable = c("FTEXSGF",
                                             "FTEXOSF",
                                             "FTEXFED",
                                             "FTEXFNF",
                                             "FTEXOTH")
        )
        df.3 <- left_join(df.2, df.source, by = "source")
        df.4 <-
                df.3 %>%
                select(stateabbrev, year, variable, value) %>%
                pivot_wider(id_cols = c(stateabbrev, year),
                            names_from = variable) %>%
                mutate(FTEXOTH = add(FTEXOTH, FTEXFNF)) %>%
                select(-FTEXFNF) %>%
                pivot_longer(cols = contains("FTEX"),
                             names_to = "variable",
                             values_to = "value")
        df.4
}
# 5.0 FY 2009 ----
get_2009_astho_finance <- function(){
path <- paste0("./data/raw/astho/profile_data_and_codebooks/",
               "2010_astho_profile_survey_data_for_public_use_updated.xlsx")
df <- readxl::read_xlsx(path = path, na = "#NULL!")
df.1 <-
        df %>%
        select(AC2, matches("E01[ABDEF][2]_2010")) %>% # '2' is for FY 2009 omit medicaid & medicare
        dplyr::filter(!AC2 %in% c("MP", "VI")) %>% # Marianna & Virgin
        mutate(E01E2_2010 = add(E01D2_2010, E01E2_2010),
       year = 2009) %>% #add fees to other
        select(-E01D2_2010) %>% # drop fines and fees
        pivot_longer(cols = E01A2_2010:E01F2_2010,
                     names_to = "variable",
                     values_to = "value") %>%
        rename(stateabbrev = AC2)
        df.1$variable <- gsub("2_2010", "", df.1$variable)
        df.update_vars <- data.frame(variable = c("E01A", "E01B", "E01E", "E01F"),
                                     source = c("FTEXSGF", "FTEXFED", "FTEXOTH", "FTEXOSF")
        )
        df.2 <- left_join(df.1, df.update_vars, by = "variable")
        df.3 <-
                df.2 %>%
                select(-variable) %>%
                rename(variable = source) %>%
                select(stateabbrev, year, variable, value)
        df.3
}
# 6.0 FY 2008 ----
get_2008_astho_finance <- function(){
        path <- paste0("./data/raw/astho/profile_data_and_codebooks/",
                       "2010_astho_profile_survey_data_for_public_use_updated.xlsx")
        df <- readxl::read_xlsx(path = path, na = "#NULL!")

        df.1 <-
                df %>%
                select(AC2, matches("E01[ABDEF][1]_2010")) %>% #omit medicaid & medicare
                dplyr::filter(!AC2 %in% c("MP", "VI")) %>% # Marianna & Virgin
                mutate(E01E1_2010 = add(E01D1_2010, E01E1_2010),
                       year = 2008) %>% #add fees to other
                select(-E01D1_2010) %>% # drop fines and fees
                pivot_longer(cols = E01A1_2010:E01F1_2010,
                             names_to = "variable",
                             values_to = "value") %>%
                rename(stateabbrev = AC2)
        df.1$variable <- gsub("1_2010", "", df.1$variable)
        df.update_vars <- data.frame(variable = c("E01A", "E01B", "E01E", "E01F"),
                                source = c("FTEXSGF", "FTEXFED", "FTEXOTH", "FTEXOSF")
                                )
        df.2 <- left_join(df.1, df.update_vars, by = "variable")
        df.3 <-
                df.2 %>%
                select(-variable) %>%
                rename(variable = source) %>%
                select(stateabbrev, year, variable, value)
        df.3
}

# 8.0 FY 2007 data not collected ----















# 9.0 combine years ----
df <- bind_rows(get_2008_astho_finance(),
                get_2009_astho_finance(),
                get_2011_astho_finance(),
                get_2014_astho_finance(),
                get_2015_astho_finance(),
                get_2018_2017_2016_astho_finances()
                )
# 10 Merge with population data ----
uscb_pop_2000_to_2019 <- function(){
        get_uscb_2010_to_2019 <- function(){
                path <- paste0("./data/raw/uscb/",
                               "nst-est2019-01.xlsx")
                df <- readxl::read_xlsx(path = path,
                                        sheet = 1,
                                        skip = 9,
                                        n_max = 51,
                                        col_names = F)
                names_df <- readxl::read_xlsx(path = path,
                                              range = "B4:M4",
                                              col_names = F)
                df.1 <-
                        df %>%
                        setNames(c("state", names_df)) %>%
                        select(-c(Census, `Estimates Base`)) %>%
                        pivot_longer(cols = `2010`:`2019`,
                                     names_to = "year",
                                     values_to = "pop") %>%
                        mutate(state = gsub("\\.", "", state))

                df.states <- data.frame(state = c(state.name, "District of Columbia"),
                                        stateabbrev = c(state.abb, "DC")
                )
                df.2 <-
                        df.1 %>%
                        left_join(. , df.states, by = "state") %>%
                        mutate(year = year %>% as.integer) %>%
                        select(stateabbrev, year, pop)
        }
        get_uscb_2000_to_2009 <- function(){
                path <- paste0("./data/raw/uscb/",
                               "st-est00int-01.xls")
                df <- readxl::read_xls(path = path,
                                       skip = 9,
                                       col_names = F,
                                       n_max = 51
                )
                df.1 <-
                        df %>%
                        select(-2, -13, -14) %>%
                        setNames(c("state", paste0("x", 2000:2009))) %>%
                        pivot_longer(cols = x2000:x2009,
                                     names_to = "year",
                                     values_to = "pop") %>%
                        mutate(state = gsub("\\.", "", state),
                               year = gsub("x", "", year) %>% as.integer)

                df.states <- data.frame(state = c(state.name, "District of Columbia"),
                                        stateabbrev = c(state.abb, "DC")
                )
                df.2 <-
                        left_join(df.1, df.states, by = "state") %>%
                        select(stateabbrev, year, pop)
        }
        df <- bind_rows(get_uscb_2000_to_2009(),
                        get_uscb_2010_to_2019())
        df
}

pop <- uscb_pop_2000_to_2019()

df <- left_join(df, pop, by = c("stateabbrev", "year")) %>%
        group_by(year, variable) %>%
        summarize(tot_amt = sum(value, na.rm = T),
                  tot_pop = sum(pop)
                  ) %>%
        mutate(spend_per_cap = divide_by(tot_amt, tot_pop) %>% round(2))





# X.X plot results ----
df$variable <- factor(df$variable,
                      levels = c("FTEXFED",
                                 "FTEXSGF",
                                 "FTEXOTH",
                                 "FTEXOSF"),
                      labels = c("federal",
                                 "state",
                                 "other",
                                 "other_sf")
                      )

ggplot(df, aes(year, spend_per_cap, total, group = variable, color = variable)) +
geom_line() +
geom_point() +
scale_x_continuous(name  = "",
                   breaks = seq(from = 2008, to = 2018, by = 2)
                   ) +
# scale_y_continuous(name = "",
#                    breaks = c(0, 4e9, 8e9, 12e9, 16e9),
#                    labels = c("$0", "$4b", "$8b", "$12b", "$16b")
#                    ) +
labs(title = "Public Health Spending by Source",
     caption = "ASTHO",
     color='Source:')
# convert to spending per person
