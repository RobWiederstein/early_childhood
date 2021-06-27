# 1.0 library ----
library(heatmaply)
# 2.0 import ----
input <- "./data/tidy/2021-06-25-maternal-infant-health.csv"
df <- data.table::fread(input = input)
# 3.0 prepare data ----
rownames(df) <- df$location
df <- df %>% select(-location)
# 4.0 heatmaply_cor ----
#vignette("heatmaply")
#?dendextend::color_branches
heatmaply_cor(
        cor(df),
        k_col = 3,
        k_row = 3
)
# 5.0 heatmaply
m <- heatmaply(df, scale = "column", k_row = 3, k_col = 3)
save(m, file = "heatmap.Rdata")
