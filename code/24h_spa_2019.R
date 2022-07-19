library(tidyverse)
library(lubridate)


## Loading the data --------------------------------------------

file_name <- "data\\spa_24_gt3_2019.csv"


fac_level <- c("Pro Cup", "Silver Cup", "Pro-AM Cup", "AM Cup")
finished <- read.csv(file = file_name) %>%
  select(-GAP) %>%
  mutate(CLASS = if_else(CLASS == "", "Pro Cup", CLASS),
         CLASS = factor(CLASS, levels = fac_level),
         TIME = ms(TIME),
         TIME = period_to_seconds(TIME))

## Pos VS. FL ---------------------------------------------

finished_plot <- finished %>%
  select(POS, TIME, LAPS, CLASS)


finished_plot %>%
  ggplot(aes(POS, TIME)) +
  geom_point(aes(color = LAPS), size = 3) +
  labs(y = "Fastest lap in seconds",
       x = "Position",
       title = "Positon VS. Fastest lap") +
  facet_wrap(~CLASS)
