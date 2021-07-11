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
