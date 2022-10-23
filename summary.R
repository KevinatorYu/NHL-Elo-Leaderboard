library(tidyverse)

elo_hist <- elo_table %>%
    ggplot(aes(x = elo)) +
    geom_histogram(aes(fill = division), bins = 8)
elo_hist

elo_table %>% group_by(division) %>% summarize(mean(elo))
