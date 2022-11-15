library(ggplot2)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(wesanderson)

ggplot(data=gc_graph_data) +
  geom_point(mapping = aes(x = date, y = mean_burrow_count_total, color=treatment)) +
  geom_line(mapping = aes(x = date, y = mean_burrow_count_total, color=treatment))

ggplot(data=gc_graph_data) +
  geom_point(mapping = aes(x = date, y = mean_burrow_diameter_total, color=treatment))+
  geom_line(mapping = aes(x = date, y = mean_burrow_diameter_total, color=treatment))

#From the larger dataset for CI's 

ggplot() +
  geom_point(data=gc_graph_data, aes(x = date, y = mean_burrow_count_total, color=treatment), size=5) +
  geom_line(data=gc_graph_data, aes(x = date, y = mean_burrow_count_total, color=treatment, group=treatment), linewidth=1) +
  geom_point(data=gc_count_dia, aes(x = date, y = burrow_count))

#Jitter
#Burrow count 
ggplot() +
  geom_point(data=gc_graph_data, aes(x = date, y = mean_burrow_count_total, color=treatment), size=5) +
  geom_line(data=gc_graph_data, aes(x = date, y = mean_burrow_count_total, color=treatment, group=treatment), linewidth=1) +
  geom_jitter(data=gc_count_dia, aes(x = date, y = burrow_count, color=treatment), width=0.1, alpha=0.3)

#Burrow diamater 
ggplot() +
  geom_point(data=gc_graph_data, aes(x = date, y = mean_burrow_diameter_total, color=treatment), size=5) +
  geom_line(data=gc_graph_data, aes(x = date, y = mean_burrow_diameter_total, color=treatment, group=treatment), linewidth=1) +
  geom_jitter(data=gc_count_dia, aes(x = date, y = mean_burrow_diameter, color=treatment), width=0.1, alpha=0.3)

      
#Boxplots 

ggplot(data=gc_count_dia, aes(x = date, y = burrow_count, factor=treatment, color=treatment)) +
  geom_boxplot(outlier.size=2, outlier.shape=21)+
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #geom_jitter(data=gc_count_dia, width=0.10, group='treatment')
#Remove 0's NVM 

#I have to make 0's not count for burrow diameter graphing 
ggplot(data=gc_count_dia, aes(x = date, y = mean_burrow_diameter, factor=treatment, color=treatment)) +
  geom_boxplot(outlier.size=2, outlier.shape=21)+
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
#Remove 0's 
ggplot(gc_count_dia[which(gc_count_dia$mean_burrow_diameter>0),], aes(x = date, y = mean_burrow_diameter, factor=treatment, color=treatment)) +
  geom_boxplot(outlier.size=2, outlier.shape=21)+
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Adding a second axis (TEMP) to previous data
ggplot() +
  geom_point(data=gc_temp_graph, aes(x = date, y = mean_burrow_count_total, color=treatment), size=5) +
  geom_line(data=gc_temp_graph, aes(x = date, y = mean_burrow_count_total, color=treatment, group=treatment), linewidth=1) +
  geom_point(data=gc_temp_graph, aes(x = date, y = mean_air_temp/1.5), size=2) +
  geom_line(data=gc_temp_graph, aes(x = date, y = mean_air_temp/1.5, color="Temperature", group=treatment), linewidth=1) +
  geom_jitter(data=gc_count_dia, aes(x = date, y = burrow_count, color=treatment), width=0.1, alpha=0.3) +
  scale_y_continuous(name="Number of Burrows", sec.axis = sec_axis(~.*1.5, name="Average Air Temperature")) +
  labs(x="Date") +
  guides(color=guide_legend(title="Treatment"))+
  scale_color_brewer(palette="Set1")


ggplot() +
  geom_point(data=gc_temp_graph, aes(x = date, y = mean_burrow_diameter_total, color=treatment), size=5) +
  geom_line(data=gc_temp_graph, aes(x = date, y = mean_burrow_diameter_total, color=treatment, group=treatment), linewidth=1) +
  geom_point(data=gc_temp_graph, aes(x = date, y = 2.5*mean_air_temp), size=2) +
  geom_line(data=gc_temp_graph, aes(x = date, y = 2.5*mean_air_temp, color='Temperature', group=treatment), linewidth=1) +
  geom_jitter(data=gc_count_dia, aes(x = date, y = mean_burrow_diameter, color=treatment), width=0.1, alpha=0.3) +
  scale_y_continuous(name="Mean Burrow Diameter", sec.axis = sec_axis(~./2.5, name="Average Air Temperature"))+
  labs(x="Date")+
  guides(color=guide_legend(title="Treatment"))+
  scale_color_brewer(palette="Set1")

#Without Jitter - kinda meh 
ggplot() +
  geom_point(data=gc_temp_graph, aes(x = date, y = mean_burrow_count_total, color=treatment), size=5) +
  geom_line(data=gc_temp_graph, aes(x = date, y = mean_burrow_count_total, color=treatment, group=treatment), linewidth=1) +
  geom_point(data=gc_temp_graph, aes(x = date, y = mean_air_temp/2), size=2) +
  geom_line(data=gc_temp_graph, aes(x = date, y = mean_air_temp/2, color="Temperature", group=treatment), linewidth=1) +
  scale_y_continuous(name="Number of Burrows", sec.axis = sec_axis(~.*2, name="Average Air Temperature")) +
  labs(x="Date") +
  guides(color=guide_legend(title="Treatment"))+
  scale_color_brewer(palette="Set1")

ggplot() +
  geom_point(data=gc_temp_graph, aes(x = date, y = mean_burrow_diameter_total, color=treatment), size=5) +
  geom_line(data=gc_temp_graph, aes(x = date, y = mean_burrow_diameter_total, color=treatment, group=treatment), linewidth=1) +
  geom_point(data=gc_temp_graph, aes(x = date, y = 1.5*mean_air_temp), size=2) +
  geom_line(data=gc_temp_graph, aes(x = date, y = 1.5*mean_air_temp, color='Temperature', group=treatment), linewidth=1) +
  scale_y_continuous(name="Mean Burrow Diameter", sec.axis = sec_axis(~./1.5, name="Average Air Temperature"))+
  labs(x="Date")+
  guides(color=guide_legend(title="Treatment"))+
  scale_color_brewer(palette="Set1")

#Modeling below 













[which(gc_count_dia$mean_burrow_diameter>0)]


#ggplot(data=WAEPV, aes(x = factor(Year), y = Estimate)) +
#  ylim(0,100000) +
#  geom_boxplot(outlier.size=2, outlier.shape=21)+
#  theme_bw(base_size = 16) +
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#  labs(y=bquote('Home Range'~ (m^2)), x="Year") +
#  geom_jitter(data=WAEPP, width=0.10)