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

# 7.0 FY 2007 data not collected ----
# 8.0 combine years ----
df <- bind_rows(get_2008_astho_finance(),
                get_2009_astho_finance(),
                get_2011_astho_finance(),
                get_2014_astho_finance(),
                get_2015_astho_finance(),
                get_2018_2017_2016_astho_finances()
                )
df
## 8.1 Adjust for inflation `priceR` ----
#World Bank -- Consumer price index reflects changes in the cost to the
#average consumer of acquiring a basket of goods and services
#that may be fixed or changed at specified intervals, such as
#yearly. The Laspeyres formula is generally used. Data are period
#averages.
library(priceR)
country <- "US"
inflation_dataframe <- retrieve_inflation_data(country)
#designate real
df <- df %>% rename(nominal = value)
df$wbi <- adjust_for_inflation(df$nominal,
                                    df$year,
                                    country,
                                    to_date = 2018)
df <-
        df %>%
        pivot_longer(cols = nominal:wbi,
                     names_to = "dollars",
                     values_to = "value")


## 8.2  Merge with US Census Bureau population data ----
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
df.1 <- left_join(df, pop, by = c("stateabbrev", "year"))

## 8.3 Merge with structure data ----
#State Health Agency Structure code = 'GSTRPHA01'
#Note: 1=Freestanding/independent, 2=Under an umbrella
path <- "./data/raw/astho/profile_data_and_codebooks/2019_states_astho_profile_dataset_final_icpsr.xlsx"
df.s <- readxl::read_xlsx(path = path)
df.s <-
        df.s %>%
        select(StateAbbrev, GSTRPHA01) %>%
        rename(stateabbrev = StateAbbrev,
               structure = GSTRPHA01)
df.1 <- left_join(df.1, df.s, by = c("stateabbrev"))
df.1$structure <- factor(df.1$structure,
                         labels = c("independent", "umbrella"))
## 8.3 Other inflation adjusters ----
#https://meps.ahrq.gov/about_meps/Price_Index.shtml

# 10.0 Plots ----

## 10.1 Plot total national healthcare expenditures ----

df.2 <-
        df.1 %>%
        group_by(year, variable, dollars) %>%
        summarize(exp_per_cap = sum(value, na.rm = T) / sum(pop)) %>%
        ungroup()

df.2$variable <- factor(df.2$variable,
                      levels = c("FTEXFED",
                                 "FTEXSGF",
                                 "FTEXOTH",
                                 "FTEXOSF"),
                      labels = c("federal",
                                 "state",
                                 "other",
                                 "other_sf")
                      )

save(df.2, file = "./data/tidy/astho_health_care_exp_tot_per_capita")
save(df.2, file = "./astho_health_care_exp_tot_per_capita")
ggplot(dplyr::filter(df.2, dollars == "wbi"), aes(year, exp_per_cap, total, group = variable, color = variable)) +
geom_line() +
geom_point() +
scale_x_continuous(name  = "",
                   breaks = seq(from = 2008, to = 2018, by = 2)
                   ) +
scale_y_continuous(name = "",
                   limits = c(0, 60),
                   breaks = seq(from = 0, to = 60, by = 15),
                   labels = paste0("$", seq(from = 0, to = 60, by = 15))
                                   ) +
labs(title = "Public Health Spending by Source",
     subtitle = "2008 to 2018",
     caption = "infl. adj. dollars / per capita",
     color='Source:') +
theme_minimal()

## 10.2 Plot State General Healthcare Results
df.3 <-
        df.1 %>%
        dplyr::filter(variable == 'FTEXSGF') %>%
        dplyr::filter(dollars == 'wbi') %>%
        dplyr::filter(year %in% dput(range(df.1$year))) %>%
        mutate(per_cap = divide_by(value, pop))

df.in <- df.3 %>% filter(stateabbrev == "IN")
df.ky <- df.3 %>% filter(stateabbrev == "KY")

p <- ggplot(df.3, aes(year, per_cap, group = stateabbrev)) +
        geom_line(color = "gray84") +
        facet_wrap(vars(structure)) +
scale_y_continuous(name = "",
                           limits = c(0, 100),
                           breaks = c(0, 33, 66, 100),
                           labels = c("$0", "$33", "$66", "$100")) +
        scale_x_continuous(name = "",
                           limits = c(2008, 2018),
                           breaks = c(2008, 2018)) +
        geom_line(aes(year, per_cap), data = df.in, color = "red")+
        geom_line(aes(year, per_cap), data = df.ky, color = "blue")+
        theme_minimal() +
        labs(title = "State General Funds Per Capita")
p
save(p, file = "./data/tidy/plot-state-general-funds-per-cap.Rdata")
# 11 Get caveats ----
path <- "./data/raw/astho/profile_data_and_codebooks/2019_states_astho_profile_dataset_final_icpsr.xlsx"
df <- readxl::read_xlsx(path = path, na = c("NA", "N/A"))
df.1 <-
        df %>%
        select(StateAbbrev, "FEXPCAV01") %>%
        mutate(year = 2018) %>%
        select(StateAbbrev, year, FEXPCAV01) %>%
        rename(state = StateAbbrev,
               year = year,
               caveat = FEXPCAV01) %>%
        drop_na()
save(df.1, file = "./data/tidy/fiscal_report_caveats.Rdata")

# 12 State rankings
caveats <- c("AK", "MN", "NH", "NM", "NY", "OH", "OK", "RI", "SC", "SD",
             "TX", "UT", "VT", "WV", "WY")

df.4 <-
        df.3 %>%
        filter(year == max(year)) %>%
        select(stateabbrev, year, pop, per_cap, structure) %>%
        mutate(caveat = "no") %>%
        mutate(rank = dense_rank(desc(per_cap))) %>%
        rename(state = stateabbrev,
               per_cap_exp = per_cap) %>%
        arrange(rank) %>%
        select(rank, state, year, pop, per_cap_exp, structure,caveat)


df.4$caveat[match(caveats, df.4$state)] <- "yes"
save(df.4, file = "./data/tidy/state-rankings.Rdata")
