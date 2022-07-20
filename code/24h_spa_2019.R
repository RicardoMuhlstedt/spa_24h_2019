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

## Pos VS. FL by laps ------------------------------------------

finished_plot <- finished %>%
  select(POS, TIME, LAPS, CLASS)


finished_plot %>%
  ggplot(aes(POS, TIME)) +
  geom_point(aes(color = LAPS), size = 3) +
  labs(y = "Fastest lap in seconds",
       x = "Position",
       title = "Positon VS. Fastest lap") +
  facet_wrap(~CLASS)

## Pos VS. FL by cars ------------------------------------------

finished_plot2 <- finished %>%
  select(POS, TIME, LAPS, CAR)


finished_plot2 %>%
  ggplot(aes(POS, TIME)) +
  geom_point(aes(color = CAR), size = 3) +
  labs(y = "Fastest lap in seconds",
       x = "Position",
       title = "Positon VS. Fastest lap") +
  scale_color_discrete(name = "Car model")

## Car VS. Mean(FL) --------------------------------------------

finished_plot3 <- finished_plot2 %>%
  group_by(CAR) %>%
  summarise(time_mean = mean(TIME %% 60)^2)


finished_plot3 %>%
  ggplot(aes(time_mean, CAR)) +
  geom_bar(fill = "darkolivegreen", stat = "identity") +
  labs(x = "Mean fastest lap",
       y = "Car model",
       title = "Car model VS. Mean fastest lap")

## Mean Pos VS. Car --------------------------------------------

finished_plot2 %>%
  group_by(CAR) %>%
  summarise(pos_mean = mean(POS)) %>%
  ggplot(aes(pos_mean, CAR)) +
  geom_bar(stat = "identity", fill = "darksalmon") +
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  labs(x = "Positon mean",
       y = "Car model",
       title = "Mean position VS. Car")
  
##  Laps VS. Car -------------------------------------------

finished_plot2 %>%
  group_by(CAR) %>%
  summarise(mean_laps = mean(LAPS %% 100)) %>%
  ggplot(aes(mean_laps, CAR)) +
  geom_bar(stat = "identity", fill = "deeppink2" ) +
  labs(x = "Laps",
       y = "Car model",
       title = "Laps VS. Car model")
  
  