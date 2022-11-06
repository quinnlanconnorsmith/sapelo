library(ggplot2)
library(tidyverse)

ggplot(data=gc_count_dia) +
  geom_point(mapping = aes(x = date, y = burrow_count))
