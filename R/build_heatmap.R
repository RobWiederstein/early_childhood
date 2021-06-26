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
        xlab = "Features",
        ylab = "Features",
        k_col = 2,
        k_row = 2
)
# 5.0 heatmaply
heatmaply(df, scale = "column")
