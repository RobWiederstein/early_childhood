path <- "./data/raw/astho/profile_data_and_codebooks/2019_states_astho_profile_dataset_final_icpsr.xlsx"
df <- readxl::read_xlsx(path = path)
#code sheet
path <- paste0("./data/raw/astho/profile_data_and_codebooks/",
               "2019-astho-codes.xlsx")
code <- readxl::read_xlsx(path = path)
df.code <-
        code %>%
        drop_na() %>%
        slice(71:99) %>%
        rename(variable = `Code 2019 Only`, question = `Questions in 2019`) %>%
        mutate(new_name = sapply(str_split(question, "-"), function(x) str_trim(x[2], side = "both"))
        ) %>%
        mutate(new_name = gsub(" ", "_", new_name)) %>%
        select(-question)
#2.3 Maternal, child, and adolescent health services
df.1 <-
        df %>%
        select(StateAbbrev, AMCHWIC01:APOPABS01) %>%
        pivot_longer(!StateAbbrev,
                     names_to = "variable",
                     values_to = "values"
                     )
#reorder states by services provided
df.1 %>%
        group_by(StateAbbrev) %>%
        tally(values) %>%
        arrange(-n) %>%
        select(StateAbbrev) %>%
        pull() %>%
        dput()
#reorder services by number of states providing
df.1 %>%
        group_by(variable) %>%
        tally(values) %>%
        arrange(n) %>%
        select(variable) %>%
        pull() %>%
        dput()
df.1$StateAbbrev <- factor(df.1$StateAbbrev,
                           labels = c("NC", "NY", "CT", "FL", "IL", "WV", "IA", "IN", "MA", "MI",
                                      "NM", "OK", "DE", "GA", "KS", "TX", "AL", "NE", "SC", "VA", "AK",
                                      "MD", "MN", "MS", "NH", "OH", "RI", "TN", "VT", "WI", "AR", "CO",
                                      "DC", "MT", "NV", "AZ", "HI", "MO", "NJ", "OR", "PA", "SD", "WA",
                                      "ND", "UT", "ID", "LA", "CA", "WY", "ME", "KY"))
df.1$variable <- factor(df.1$variable,
                        c("AMCHPCC01", "AMCHSCS01", "AMCHOBS01", "AMCHPTR01", "AMCHREG01",
                          "AMCHNUT01", "AMCHPRE01", "AMCHEPS01", "AMCHWCS01", "APOPABS01",
                          "AMCHNAC01", "AMCHSHS01", "AMCHEIS01", "APOPSEX01", "AMCHSPN01",
                          "AMCHHMV01", "AMCHFMP01", "AMCHWIC01", "APOPPRG01", "ASCRNEW01"
                        ))

ggplot(df.1, aes(StateAbbrev, variable)) +
geom_tile(aes(fill = factor(values)))
df.1
df.2 <- left_join(df.1, df.code)
