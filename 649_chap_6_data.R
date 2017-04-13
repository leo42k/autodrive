library(dplyr)

set.seed(20170413)
sample <- rnorm(200, 0, sqrt(5))
sample %>% var
sample %>% length

# getwd()
saveRDS(sample, "sample.rds")
x <- readRDS("sample.rds")
mean(x)
var(x)
sd(x)