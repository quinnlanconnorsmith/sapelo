library(ggplot2)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(wesanderson)
library(nlme)
library(leaflet)
library(lme4)
library(ggiraph)
library(ggiraphExtra)
library(ggeffects)
library(lmeSplines)
library(sjPlot)
library(effects)
#ggplot(data=WAEPV, aes(x = factor(Year), y = Estimate)) +
#  ylim(0,100000) +
#  geom_boxplot(outlier.size=2, outlier.shape=21)+
#  theme_bw(base_size = 16) +
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#  labs(y=bquote('Home Range'~ (m^2)), x="Year") +
#  geom_jitter(data=WAEPP, width=0.10)


### Model comp table with AIC ###

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
  geom_jitter(data=gc_count_dia, aes(x = date, y = burrow_count, color=treatment), width=0.1, alpha=0.5)+
  scale_colour_manual(values = c("#56B4E9", "#999999", "#009E73", "#D55E00","black")) +
  theme(text = element_text(size = 13)) +
  labs(x="Date", y="Number of Burrows") +
  guides(color=guide_legend(title="Treatment"))
  

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
#Colors 
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Adding a second axis (TEMP) to previous data
ggplot() +
  geom_point(data=gc_temp_graph, aes(x = date, y = mean_burrow_count_total, color=treatment), size=5) +
  geom_line(data=gc_temp_graph, aes(x = date, y = mean_burrow_count_total, color=treatment, group=treatment), linewidth=1) +
  geom_point(data=gc_temp_graph, aes(x = date, y = mean_air_temp/1.5), size=2) +
  geom_line(data=gc_temp_graph, aes(x = date, y = mean_air_temp/1.5, color="Temperature", group=treatment), linewidth=1) +
  geom_jitter(data=gc_count_dia, aes(x = date, y = burrow_count, color=treatment), width=0.1, alpha=0.5) +
  scale_y_continuous(name="Number of Burrows", sec.axis = sec_axis(~.*1.5, name="Average Air Temperature (°C)")) +
  labs(x="Date") +
  guides(color=guide_legend(title="Treatment"))+
  scale_colour_manual(values = c("#56B4E9", "#999999", "#009E73", "#D55E00","black")) +
  theme(text = element_text(size = 13))    
  #scale_colour_manual(values = c("#E69F00", "#999999", "#009E73", "#D55E00","black")) +
  #scale_color_brewer(palette="Dark2") +
  #Below this is new
  #theme(base_size = 14) +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      #panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot() +
  geom_point(data=gc_temp_graph, aes(x = date, y = mean_burrow_diameter_total, color=treatment), size=5) +
  geom_line(data=gc_temp_graph, aes(x = date, y = mean_burrow_diameter_total, color=treatment, group=treatment), linewidth=1) +
  geom_point(data=gc_temp_graph, aes(x = date, y = 2.5*mean_air_temp), size=2) +
  geom_line(data=gc_temp_graph, aes(x = date, y = 2.5*mean_air_temp, color='Temperature', group=treatment), linewidth=1) +
  geom_jitter(data=gc_count_dia, aes(x = date, y = mean_burrow_diameter, color=treatment), width=0.1, alpha=0.5) +
  scale_y_continuous(name="Mean Burrow Diameter (mm)", sec.axis = sec_axis(~./2.5, name="Average Air Temperature (°C)"))+
  labs(x="Date")+
  guides(color=guide_legend(title="Treatment"))+
  scale_colour_manual(values = c("#56B4E9", "#999999", "#009E73", "#D55E00","black")) +
  theme(text = element_text(size = 13)) 
  #scale_color_brewer(palette="Set1")+
#Below this is new
  #theme_bw(base_size = 14) +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
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

gc_sites_map_n <- read.csv("gc_sites_fix_north.csv", header=T)

gc_sapelo_22_n <- leaflet(data = gc_sites_map_n) %>% 
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Site), radius=1, fillOpacity=0.9, color="blue")
gc_sapelo_22_n



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

####Visualizing non-Comp Models####
sjPlot::plot_model(bc_model_final)

sjPlot::plot_model(bc_model_final,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(bc_model_final, 
                   axis.labels=c("Temperature", "Rake", "Marine Debris", "Compact"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Treatments on Burrow Count")

sjPlot::tab_model(bc_model_final, 
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)","Compact", "Marine Debris", "Rake", "Temperature"),
                  dv.labels= "Effect of Treatments on Burrow Count")

model_coefs <- coef(bc_model_final)$treatment %>% 
  rename(Intercept = `(Intercept)`, Slope = mean_air_temp) %>% 
  rownames_to_column("Group")




####New Comparative Response####

cmod1 <- lm(burrow_count_comp~replicate, data=gc_mod_df)
summary(cmod1)
plot(cmod1, ask=F)
#Varaince of burrow count comp appears homogeneous among replicates 

cmod2 <- lm(burrow_count_comp~replicate+mean_air_temp, data=gc_mod_df)
anova(cmod1, cmod2)

cmod3 <-lm(burrow_count_comp~treatment + mean_air_temp + treatment*mean_air_temp, data=gc_mod_df)
summary(cmod3)
plot(gc_mod_df$replicate, residuals(cmod3, type='pearson'))

cmod4 <- lme(burrow_count_comp~treatment + mean_air_temp + treatment*mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
summary(cmod4)
plot(cmod4)

gls_cmod1 <- gls(burrow_count_comp~treatment + mean_air_temp + treatment*mean_air_temp, data=gc_mod_df)
anova(gls_cmod1, cmod4)

#So the random effect can be kept! 
#Checking standardized residials and fitted values 

cmod4_residuals <- residuals(cmod4, type='pearson')
cmod4_fitted <- fitted.values(cmod4)
plot(cmod4_fitted,cmod4_residuals)
abline(h=0)
plot(cmod4_residuals~treatment, data=gc_mod_df)
abline(h=0)
plot(cmod4_residuals~mean_air_temp, data=gc_mod_df)
abline(h=0)
plot(cmod4_residuals~replicate, data=gc_mod_df)
abline(h=0)

#Refit model with ML and drop interaction 
cmod5 <- lme(burrow_count_comp~treatment + mean_air_temp + treatment*mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
cmod6 <- lme(burrow_count_comp~treatment + mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
anova(cmod5,cmod6)
#So the interaction term can be dropped 

cmod7 <- lme(burrow_count_comp~treatment + mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
cmod7_droptreat <- lme(burrow_count_comp~mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
cmod7_droptemp <- lme(burrow_count_comp~treatment, random= ~1|replicate, method = 'ML', data=gc_mod_df)

summary(cmod7)
summary(cmod7_droptreat)
summary(cmod7_droptemp)

anova(cmod7,cmod7_droptemp)
anova(cmod7,cmod7_droptreat)
#So both of these are very significant 

summary(cmod7)

bc_comp_model_final <-lme(burrow_count_comp~treatment + mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
hist(residuals(bc_comp_model_final))
bc_comp_final_residuals <-residuals(bc_comp_model_final, type='pearson')
bc_comp_final_fitted <-fitted.values(bc_comp_model_final)
#Checking assumptions 
plot(bc_comp_final_fitted,bc_comp_final_residuals)
abline(h=0)

#The rest 
plot(bc_comp_final_residuals~treatment, data=gc_mod_df)
abline(h=0)
plot(bc_comp_final_residuals~mean_air_temp, data=gc_mod_df)
abline(h=0)
plot(bc_comp_final_residuals~replicate, data=gc_mod_df)
abline(h=0)

summary(bc_comp_model_final)

bc_comp_model_final_rand <-lme(burrow_count_comp~treatment + mean_air_temp, random= (~1+treatment|replicate), method = 'REML', data=gc_mod_df)

anova(bc_comp_model_final, bc_comp_model_final_rand)

#Ah shit we need random intercept and slope 
summary(bc_comp_model_final_rand)

hist(residuals(bc_comp_model_final_rand))
bc_comp_final_residuals_rand <-residuals(bc_comp_model_final_rand, type='pearson')
bc_comp_final_fitted_rand <-fitted.values(bc_comp_model_final_rand)
#Checking assumptions 
plot(bc_comp_final_fitted_rand,bc_comp_final_residuals_rand)
abline(h=0)

#The rest 
plot(bc_comp_final_residuals_rand~treatment, data=gc_mod_df)
abline(h=0)
plot(bc_comp_final_residuals_rand~mean_air_temp, data=gc_mod_df)
abline(h=0)
plot(bc_comp_final_residuals_rand~replicate, data=gc_mod_df)
abline(h=0)

#Compact is not sig dif 
#Rake is not sig diff 
#MD is sig dif 
#So this is the exact opposite of the original model 

bc_comp_var_rep_resid_rand <- VarCorr(bc_comp_model_final_rand)
bc_comp_var_rep_rand <- as.numeric(bc_comp_var_rep_resid_rand[1])
bc_comp_var_resid_rand <- as.numeric(bc_comp_var_rep_resid_rand[2])

bc_comp_var_rep_rand/(bc_comp_var_rep_rand + bc_comp_var_resid_rand)

#Correlation between observations is low (0.381)

#Burrow diameter time with new comp response 

cmod8 <- lm(mean_burrow_diameter_comp~replicate, data=gc_mod_df)
summary(cmod8)
plot(cmod8, ask=F)
#Varaince of burrow diameter comp appears homogeneous among replicates 

cmod9 <- lm(mean_burrow_diameter_comp~replicate+mean_air_temp, data=gc_mod_df)
anova(cmod8, cmod9)

cmod10 <-lm(mean_burrow_diameter_comp~treatment + mean_air_temp + treatment*mean_air_temp, data=gc_mod_df)
summary(cmod10)
plot(gc_mod_df$replicate, residuals(cmod3, type='pearson'))

cmod11 <- lme(mean_burrow_diameter_comp~treatment + mean_air_temp + treatment*mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
summary(cmod11)
plot(cmod11)

gls_cmod2 <- gls(mean_burrow_diameter_comp~treatment + mean_air_temp + treatment*mean_air_temp, data=gc_mod_df)
anova(gls_cmod2, cmod11)

#So the random effect can be kept! But it's real close 
#Checking standardized residuals and fitted values 

cmod11_residuals <- residuals(cmod11, type='pearson')
cmod11_fitted <- fitted.values(cmod11)
plot(cmod11_fitted,cmod11_residuals)
abline(h=0)
plot(cmod11_residuals~treatment, data=gc_mod_df)
abline(h=0)
plot(cmod11_residuals~mean_air_temp, data=gc_mod_df)
abline(h=0)
plot(cmod11_residuals~replicate, data=gc_mod_df)
abline(h=0)

#Refit model with ML and drop interaction 
cmod12 <- lme(mean_burrow_diameter_comp~treatment + mean_air_temp + treatment*mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
cmod13 <- lme(mean_burrow_diameter_comp~treatment + mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
anova(cmod12,cmod13)
#So the interaction term can be dropped - but just barely 

cmod14 <- lme(mean_burrow_diameter_comp~treatment + mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
cmod14_droptreat <- lme(mean_burrow_diameter_comp~mean_air_temp, random= ~1|replicate, method = 'ML', data=gc_mod_df)
cmod14_droptemp <- lme(mean_burrow_diameter_comp~treatment, random= ~1|replicate, method = 'ML', data=gc_mod_df)

summary(cmod14)
summary(cmod14_droptreat)
summary(cmod14_droptemp)

anova(cmod14,cmod14_droptemp)
anova(cmod14,cmod14_droptreat)
#So both of these are significant - treatment is barely so 

summary(cmod14)

bd_comp_model_final <-lme(mean_burrow_diameter_comp~treatment + mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
hist(residuals(bd_comp_model_final))
bd_comp_final_residuals <-residuals(bd_comp_model_final, type='pearson')
bd_comp_final_fitted <-fitted.values(bd_comp_model_final)
#Checking assumptions 
plot(bd_comp_final_fitted,bd_comp_final_residuals)
abline(h=0)

#The rest 
plot(bd_comp_final_residuals~treatment, data=gc_mod_df)
abline(h=0)
plot(bd_comp_final_residuals~mean_air_temp, data=gc_mod_df)
abline(h=0)
plot(bd_comp_final_residuals~replicate, data=gc_mod_df)
abline(h=0)

summary(bd_comp_model_final)

ctrl <- lmeControl(opt='optim')
bd_comp_model_final_rand <-lme(mean_burrow_diameter_comp~treatment + mean_air_temp, random= ~1+treatment|replicate, control=ctrl, method = 'REML', data=gc_mod_df)


anova(bd_comp_model_final, bd_comp_model_final_rand )
#Compact is sig dif 
#Rake is not sig diff 
#MD is not sig dif 
#Treamtent is actually significant (barely) when looking at comparison 

bd_comp_var_rep_resid <- VarCorr(bd_comp_model_final)
bd_comp_var_rep <- as.numeric(bd_comp_var_rep_resid[1])
bd_comp_var_resid <- as.numeric(bd_comp_var_rep_resid[2])

bd_comp_var_rep/(bd_comp_var_rep + bd_comp_var_resid)

#Correlation between observations is low (0.04)


####Visualizing Comparison Models####

sjPlot::plot_model(bc_comp_model_final_rand)

sjPlot::plot_model(bc_comp_model_final_rand,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(bc_comp_model_final_rand, 
                   axis.labels=c("Temperature", "Rake", "Marine Debris", "Compact"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Treatments on Burrow Count")

sjPlot::tab_model(bc_comp_model_final_rand, 
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)","Compact", "Marine Debris", "Rake", "Temperature"),
                  dv.labels= "Effect of Treatments on Burrow Count")

effects_treatment<- effects::effect(term= "mean_air_temp", mod= bc_comp_model_final)
summary(effects_treatment)

df_treat <- as.data.frame(effects_treatment)

#test_plot <- ggplot() + 
  #2
  #geom_point(data=gc_temp_graph, aes(x = mean_air_temp, y = mean_burrow_count_total, color=treatment), size=5) +
  #3
  #geom_point(data=gc_glmm, aes(x = treatment, y = burrow_count_comp, color=treatment), size=5) +
  #geom_point(data=df_treat, aes(x=date, y=fit, color=treatment)) +
  #4
  #geom_line(data=df_treat, aes(x=treatment, y=fit), color="blue") +
  #5
  #geom_ribbon(data= df_treat, aes(x=treatment, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  #labs(x=".", y=".")

#test_plot

#Still trying to visualize 

#bc_comp_model_final_rand
#d1 <- coef(bc_comp_model_final_rand)$replicate
#d1$treatment <- rownames(d1)

#fm2 <- lmList(burrow_count_comp~treatment + mean_air_temp|replicate, gc_glmm)
#d2 <- coef(fm2)
#d2$treatment <- rownames(d2)


#gg0 <- ggplot()+
#  geom_point(data=gc_temp_graph, aes(x = mean_air_temp, y = mean_burrow_count_total, color=treatment))+
#  geom_point()+
#  geom_abline(data=d2,
#              mapping=aes(intercept='(Intercept)',
#                          slope=mean_air_temp,colour=treatment))
#  geom_abline(data=d2,linetype=2,
#              mapping=aes(intercept=`(Intercept)`,
#                          slope=Days,colour=Subject))  
#gg0


#Attempt 3 at visualizing 
#bcl_comp_model_final_rand <-lme(burrow_count_comp~treatment + mean_air_temp, random= (~1+treatment|replicate), method = 'REML', data=gc_mod_df)

#bc_comp_model_final_rand

#bc_model_coefs <- coef(bc_comp_model_final_rand)
#bc_model_coefs

#%>% 
#  rename(Intercept = `(Intercept)`, Slope = mean_air_temp) %>% 
#  rownames_to_column("treatment")

#LMER model 
#model <- lmer(burrow_count_comp~treatment + mean_air_temp + (1+treatment|replicate), data=gc_mod_df )

#model
#summary(model)
#anova(model)

#model_coefs <- coef(model)$treatment %>% 
#  rename(Intercept = `(Intercept)`, Slope = mean_air_temp) %>% 
#  rownames_to_column("treatment")

#Attempt 4- predict 

fit_1 <- lme(burrow_count_comp~treatment + mean_air_temp, random= (~1+treatment|replicate), method = 'REML', data=gc_mod_df)
fit_1
predict(fit_1)

p <- ggplot(data=gc_mod_df, aes(x = mean_air_temp, y = burrow_count_comp, colour = treatment)) +
  geom_point(size=3) +
  geom_line(aes(y = predict(fit_1)),size=1) 

p

p <- ggplot(data=gc_mod_df, aes(x = mean_air_temp, y = burrow_count_comp, colour = treatment)) +
  geom_point(size=3) +
  #geom_line(aes(y = predict(bc_comp_model_final_rand)),size=1)  +
  geom_smooth(method="lm")

p

fit_2 <- lme(burrow_count~treatment + mean_air_temp, random= (~1+treatment|replicate), method = 'REML', data=gc_mod_df)
p1 <- ggplot(data=gc_mod_df, aes(x = mean_air_temp, y = burrow_count, colour = treatment)) +
  geom_point(size=3) +
  geom_line(aes(y = predict(fit_2)),size=1)+
  

p1

ggplot(data=gc_glmm, colour=treatment)+
  geom_line(aes(x=mean_air_temp, y = predict(fit_1)),size=1) 

seq <- 1:200
ggplot()+
  geom_line(aes(x=seq , y = predict(fit_1)),size=1)



#Let's stick with predict 


fit_1 <- lme(burrow_count_comp~treatment + mean_air_temp, random= (~1 +treatment|replicate), method = 'REML', data=gc_mod_df)
fit_2 <- lmer(burrow_count_comp~treatment + mean_air_temp + (1 + treatment|replicate), data=gc_mod_df)


gg1 <- ggplot(data=gc_mod_df, aes(x = mean_air_temp, y = burrow_count_comp, colour = treatment)) +
  geom_jitter(data=gc_mod_df, aes(x = mean_air_temp, y = burrow_count_comp, color=treatment), width=0.05, alpha=0.5) +
  geom_line(aes(y = bc_pred),size=1)  
  #geom_smooth(method="lm") 
gg1 

gg2 <- ggplot(data=gc_mod_df, aes(x = mean_air_temp, y = mean_burrow_diameter_comp, colour = treatment)) +
  geom_jitter(data=gc_mod_df, aes(x = mean_air_temp, y = mean_burrow_diameter_comp, color=treatment), width=0.05, alpha=0.5) +
  geom_line(aes(y = bd_pred),size=1)  
gg2

library(tidyverse)
library(lme4)





#ggpredict? 
#nope 



#Attempt 10000000


data(smSplineEx1)
# variable `all' for top level grouping
smSplineEx1$all <- rep(1,nrow(smSplineEx1))
# setup spline Z-matrix
smSplineEx1$Zt <- smspline(~ time, data=smSplineEx1)
fit1s <- lme(y ~ time, data=smSplineEx1,
             random=list(all=pdIdent(~Zt - 1)))
summary(fit1s)
plot(smSplineEx1$time,smSplineEx1$y,pch="o",type="n",
     main="Spline fits: lme(y ~ time, random=list(all=pdIdent(~Zt-1)))",
     xlab="time",ylab="y")
points(smSplineEx1$time,smSplineEx1$y,col=1)
lines(smSplineEx1$time, smSplineEx1$y.true,col=1)
lines(smSplineEx1$time, fitted(fit1s),col=2)

times20 <- seq(1,100,length=20)
Zt20 <- smspline(times20)
smSplineEx1$Zt20 <- approx.Z(Zt20,times20,smSplineEx1$time)
fit1s20 <- lme(y ~ time, data=smSplineEx1,
               random=list(all=pdIdent(~Zt20 - 1)))
# note: virtually identical df, loglik.
anova(fit1s,fit1s20)
summary(fit1s20)

times200 <- seq(1,100,by=0.5)
pred.df <- data.frame(all=rep(1,length(times200)),time=times200)
pred.df$Zt20 <- approx.Z(Zt20, times20,times200)
yp20.200 <- predict(fit1s20,newdata=pred.df)
lines(times200,yp20.200+0.02,col=4)

data(Spruce)

Spruce$Zday <- smspline(~ days, data=Spruce)
Spruce$all <- rep(1,nrow(Spruce))
# overall spline term, random plot and Tree effects
spruce.fit1 <- lme(logSize ~ days, data=Spruce,
                   random=list(all= pdIdent(~Zday -1),
                               plot=~1, Tree=~1))
# try overall spline term plus plot level linear + spline term
spruce.fit2 <- lme(logSize ~ days, data=Spruce,
                   random=list(all= pdIdent(~Zday - 1),
                               plot= pdBlocked(list(~ days,pdIdent(~Zday - 1))),
                               Tree = ~1))
anova(spruce.fit1,spruce.fit2)
summary(spruce.fit1)

#Trying it out 

gc_mod_df$ztemp <- smspline(~mean_air_temp, data=gc_mod_df)
gc_mod_df$all <- rep(1,nrow(gc_mod_df))

gc_fit1 <- lme(burrow_count_comp~treatment + mean_air_temp, random= (~1+treatment|replicate), method = 'REML', data=gc_mod_df)

#Try again 

df2 <- data.frame(x=c(seq(0,1,0.001),1:1000))
df2$pred <- predict(bc_comp_model_final_rand, newdata=df2)

#Attempt 6? 7? 

gc_mod_df
bd_comp_model_final <-lme(mean_burrow_diameter_comp~treatment + mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)

gc_mod_df$bc_pred <- predict(bc_comp_model_final_rand)
gc_mod_df$bd_pred <- predict(bd_comp_model_final)


ggplot(gc_mod_df, aes(x = date, y = burrow_count_comp, colour=factor(treatment))) +
  geom_point(shape = 16, size=1.8) + theme(legend.position = "none") + 
  geom_line(aes(y=bc_pred))

#This is the same gd thing 


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
#Correct way 

#Trying individual plot things 
#Don't do this 
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

goof4 <- lme(burrow_count~treatment+mean_air_temp+date, random= ~1+treatment|replicate, method = 'REML', data=gc_mod_df)
summary(goof4)
##Compare to first day burrow count##
#Chat with ashley 
