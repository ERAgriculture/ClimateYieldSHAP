library(dplyr)
library(tidyr)
library(knitr)

generate_summary <- function(x) {
  c(min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE))
}

features <- c("yield", "Rain.sum", "Rain.sum.Dev.Mean", "Rain.Max.RSeq.5", 
              "Rain.Days.L.1", "Rain.Days.L.1.Dev.Mean", "Tmax.mean", "Tmax.mean.Dev.Mean")

summary_stats <- data %>%
  select(all_of(features)) %>%
  summarise(across(everything(), generate_summary))

summary_stats