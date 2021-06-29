# 1.0 library ----
library(heatmaply)
# 2.0 import ----
input <-  "./data/tidy/2021-06-28-in-early-childhood.csv"
df <- data.table::fread(input = input)
# 3.0 prepare data ----
rownames(df) <- df$location
df <- df %>% select(-c(location, ages_0_4))
# 4.0 heatmaply_cor ----
cg_ec <-heatmaply_cor(
        cor(df),
        k_col = 3,
        k_row = 3
)
save(cg_ec, file = "./data/tidy/cg_ec.Rdata")
# 5.0 heatmaply ----
hm_ec <- heatmaply(df, scale = "column", k_row = 3, k_col = 3)
save(hm_ec, file = "./data/tidy/hm_ec.Rdata")
