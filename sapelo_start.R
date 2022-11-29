library(ggplot2)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(wesanderson)
library(nlme)
library(leaflet)
library(lme4)
#ggplot(data=WAEPV, aes(x = factor(Year), y = Estimate)) +
#  ylim(0,100000) +
#  geom_boxplot(outlier.size=2, outlier.shape=21)+
#  theme_bw(base_size = 16) +
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#  labs(y=bquote('Home Range'~ (m^2)), x="Year") +
#  geom_jitter(data=WAEPP, width=0.10)


#### Visual Graphs ####

b_d <- gc_count_dia$mean_burrow_diameter
hist(b_d)
b_c <- gc_count_dia$burrow_count
hist(b_c)

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

#### Maps ####


gc_sites_map <- read.csv("gc_sites_fix.csv", header=T)

gc_sapelo_22 <- leaflet(data = gc_sites_map) %>% 
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Site), radius=1, fillOpacity=0.9, color="blue")
gc_sapelo_22



####Model Play####


#Modeling below 
lm <- lm(burrow_count~treatment, data=gc_glmm)
summary(lm)
plot(lm)
mlme1 <- lme(burrow_count~treatment, random=~1 | mean_air_temp, data=gc_glmm)
summary(mlme1)
plot(mlme1)
mlme2 <- lme(mean_burrow_diameter~treatment, random=~1 | mean_air_temp, data=gc_glmm)
summary(mlme2)
plot(mlme2)
mlme3 <- lme(burrow_count~treatment, random=~1 | set, data=gc_glmm)
summary(mlme3)
plot(mlme3)
glmm <- glmer(burrow_count~treatment + (1|mean_air_temp), data=gc_glmm)
summary(glmm)

#Stepwise modeling 
mod1 <- lm(burrow_count~treatment, data=gc_mod_df)
summary(mod1)
plot(mod1, ask=F)
#treatment*air temp 
#replicate as a RE 


#### Big Models ####


#Following bee code 
gc_mod_df <- read_csv('gc_glmm.csv') %>% 
  mutate(replicate = as.factor(replicate),
         treatment= as.factor(treatment),
         site = as.factor(site))

model1 <- lm(burrow_count~replicate, data=gc_mod_df)
summary(model1)
plot(model1, ask=F)
#Varaince of burrow count appears homogenous among replicates 
model2 <- lm(mean_burrow_diameter~replicate, data=gc_mod_df)
summary(model2)
plot(model2, ask=F)

#Focus on burrow count for now 
model1.5 <- lm(burrow_count~replicate+mean_air_temp, data=gc_mod_df)
anova(model1, model1.5)

model2.5 <-lm(burrow_count~treatment + mean_air_temp + treatment*mean_air_temp, data=gc_mod_df)
summary(model2.5)
plot(gc_mod_df$replicate, residuals(model2.5, type='pearson'))

model3 <- lme(burrow_count~treatment + mean_air_temp + treatment*mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
summary(model3)
plot(model3)

gls_model1 <- gls(burrow_count~treatment + mean_air_temp + treatment*mean_air_temp, data=gc_mod_df)
anova(gls_model1, model3)
#So the random effect can be kept! 
#Checking standardized residials and fitted values 

model3_residuals <- residuals(model3, type='pearson')
model3_fitted <- fitted.values(model3)
plot(model3_fitted,model3_residuals)
abline(h=0)
plot(model3_residuals~treatment, data=gc_mod_df)
abline(h=0)
plot(model3_residuals~mean_air_temp, data=gc_mod_df)
abline(h=0)
plot(model3_residuals~replicate, data=gc_mod_df)
abline(h=0)

#Refit model with ML and drop interaction 
model4 <- lme(burrow_count~treatment + mean_air_temp + treatment*mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
model5 <- lme(burrow_count~treatment + mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
anova(model4,model5)
#So the interaction term can be dropped 

model5 <- lme(burrow_count~treatment + mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
model5_droptreat <- lme(burrow_count~mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
model5_droptemp <- lme(burrow_count~treatment, random= ~1|replicate, method = 'ML', data=gc_mod_df)

summary(model5)
summary(model5_droptreat)
summary(model5_droptemp)

anova(model5,model5_droptemp)
anova(model5,model5_droptreat)
#So both of these are significant 

summary(model5)

bc_model_final <-lme(burrow_count~treatment + mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
hist(residuals(bc_model_final))
bc_final_residuals <-residuals(bc_model_final, type='pearson')
bc_final_fitted <-fitted.values(bc_model_final)
#Checking assumptions 
plot(bc_final_fitted,bc_final_residuals)
abline(h=0)

#The rest 
plot(bc_final_residuals~treatment, data=gc_mod_df)
abline(h=0)
plot(bc_final_residuals~mean_air_temp, data=gc_mod_df)
abline(h=0)
plot(bc_final_residuals~replicate, data=gc_mod_df)
abline(h=0)

summary(bc_model_final)
#Comapct is sig dif 
#Rake is sig dif 
#MD is  not sig dif 
#Need to fix air temp?

var_rep_resid <- VarCorr(bc_model_final)
var_rep <- as.numeric(var_rep_resid[1])
var_resid <- as.numeric(var_rep_resid[2])

var_rep/(var_rep + var_resid)

#Correlation between observations is low (0.15)



#Now lets try it for burrow diameter 
model2 <- lm(mean_burrow_diameter~replicate, data=gc_mod_df)
summary(model2)
plot(model2, ask=F)

model6 <-lm(mean_burrow_diameter~treatment + mean_air_temp + treatment*mean_air_temp, data=gc_mod_df)
summary(model6)
plot(gc_mod_df$replicate, residuals(model6, type='pearson'))

model7 <- lme(mean_burrow_diameter~treatment + mean_air_temp + treatment*mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
summary(model7)
plot(model7)

gls_model2 <- gls(mean_burrow_diameter~treatment + mean_air_temp + treatment*mean_air_temp, data=gc_mod_df)
anova(gls_model2, model7)
#So the random effect can be kept! 
#Checking standardized residuals and fitted values 

model7_residuals <- residuals(model7, type='pearson')
model7_fitted <- fitted.values(model7)
plot(model7_fitted,model7_residuals)
abline(h=0)
plot(model7_residuals~treatment, data=gc_mod_df)
abline(h=0)
plot(model7_residuals~mean_air_temp, data=gc_mod_df)
abline(h=0)
plot(model7_residuals~replicate, data=gc_mod_df)
abline(h=0)

#Refit model with ML and drop interaction 
model8 <- lme(mean_burrow_diameter~treatment + mean_air_temp + treatment*mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
model9 <- lme(mean_burrow_diameter~treatment + mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
anova(model8,model9)
#So the interaction term can be dropped 

model10 <- lme(mean_burrow_diameter~treatment + mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
model10_droptreat <- lme(mean_burrow_diameter~mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
model10_droptemp <- lme(mean_burrow_diameter~treatment, random= ~1|replicate, method = 'ML', data=gc_mod_df)

summary(model10)
summary(model10_droptreat)
summary(model10_droptemp)

anova(model10,model10_droptemp)
anova(model10,model10_droptreat)
#So dropping treatment is actually more parsimonious  

summary(model10)

bd_model_final <-lme(burrow_count~ mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
hist(residuals(bd_model_final))
bd_final_residuals <-residuals(bd_model_final, type='pearson')
bd_final_fitted <-fitted.values(bd_model_final)
#Checking assumptions 
plot(bd_final_fitted,bd_final_residuals)
abline(h=0)

#The rest 

plot(bd_final_residuals~mean_air_temp, data=gc_mod_df)
abline(h=0)
plot(bd_final_residuals~replicate, data=gc_mod_df)
abline(h=0)

summary(bd_model_final)
#Air temp is significant 

bd_var_rep_resid <- VarCorr(bd_model_final)
bd_var_rep <- as.numeric(bd_var_rep_resid[1])
bd_var_resid <- as.numeric(bd_var_rep_resid[2])

bd_var_rep/(bd_var_rep + bd_var_resid)
#Correlation is low (0.12)

####Goofin####

goof <-lme(burrow_count~treatment + mean_air_temp + mean_baro + mean_PAR + mean_humidity, random= ~1|replicate, method = 'ML', data=gc_mod_df)
summary(goof)

goof_dropPAR <-lme(burrow_count~treatment + mean_air_temp + mean_baro + mean_humidity, random= ~1|replicate, method = 'ML', data=gc_mod_df)
summary(goof_dropPAR)

anova(goof, goof_dropPAR)

goof_drophum <-lme(burrow_count~treatment + mean_air_temp + mean_baro, random= ~1|replicate, method = 'ML', data=gc_mod_df)
summary(goof_drophum)

#Currently treating each group as a 'replicate' 
#e.g. plots 1-4 are replicate 1
#does each plot need it's own replicate? 

#Trying individual plot things 
lmsite <- lm(burrow_count~site, data=gc_mod_df)
summary(lmsite)
rsite <- bc_model_final <-lme(burrow_count~treatment + mean_air_temp, random= ~1|site, method = 'REML', data=gc_mod_df)
summary(rsite)
gls_model3 <- gls(burrow_count~treatment + mean_air_temp, data=gc_mod_df)
anova(gls_model3, rsite)


goof2 <- lme(burrow_count~treatment, random= ~1|replicate, method = 'REML', data=gc_mod_df)
summary(goof2)

goof3 <- lme(burrow_count~treatment, random= ~1|site, method = 'REML', data=gc_mod_df)
summary(goof3)

