# 1.0 library ----
library(heatmaply)
# 2.0 import ----
input <- "./data/tidy/2021-06-25-maternal-infant-health.csv"
df <- data.table::fread(input = input)
# 3.0 prepare data ----
rownames(df) <- df$location
df <- df %>% select(-location)
# 4.0 heatmaply_cor ----
cg_mh <- heatmaply_cor(
        cor(df),
        k_col = 3,
        k_row = 3
)
file <- "./data/tidy/cg_mh.Rdata"
save(cg_mh, file = file)
# 5.0 heatmaply
hm_mh <- heatmaply(df, scale = "column", k_row = 3, k_col = 3)
file <- "./data/tidy/hm_mh.Rdata"
save(hm_mh, file = file)
