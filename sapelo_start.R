library(ggplot2)
library(tidyverse)

ggplot(data=gc_graph_data) +
  geom_point(mapping = aes(x = date, y = mean_burrow_count_total, color=treatment)) +
  geom_line(mapping = aes(x = date, y = mean_burrow_count_total, color=treatment))

ggplot(data=gc_graph_data) +
  geom_point(mapping = aes(x = date, y = mean_burrow_diameter_total, color=treatment))+
  geom_line(mapping = aes(x = date, y = mean_burrow_diameter_total, color=treatment))


