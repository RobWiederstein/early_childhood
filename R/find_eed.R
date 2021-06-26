input <- "https://data.cms.gov/provider-data/sites/default/files/resources/350f34f9ef3d484925d49dfcce7a0f54_1616702724/Timely_and_Effective_Care-Hospital.csv"
df <- data.table::fread(input = input, na.strings = "Not Available")
names(df) <- janitor::make_clean_names(names(df))
tail(df)
df.1 <-
        df %>%
        filter(condition == "Pregnancy and Delivery Care") %>%
        drop_na(score) %>%
        mutate(score = score %>% as.integer) %>%
        group_by(score) %>%
        tally()
