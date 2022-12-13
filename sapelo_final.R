# Hello! 
#Here's all the code (hopefully) I used for Sapelo 2022 
#Obviously you won't be able to run all this from the get-go
#If you want to run all this, be sure you have the appropriate files from the repo stored in your working directory
#Feel free to contact me (qcsmith2@wisc.edu) with any questions 

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
library(merTools)
library(AICcmodavg)

####Beginning Figures####

#Boxplots 

ggplot(data=gc_count_dia, aes(x = date, y = burrow_count, factor=treatment, color=treatment)) +
  geom_boxplot(outlier.size=2, outlier.shape=21)+
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(gc_count_dia[which(gc_count_dia$mean_burrow_diameter>0),], aes(x = date, y = mean_burrow_diameter, factor=treatment, color=treatment)) +
  geom_boxplot(outlier.size=2, outlier.shape=21)+
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Adding a second axis (TEMP)
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

#### Big Models ####
#The comparative response was the way to go, head down to the next section

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
#This is what you want 

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

par(mfrow = c(2,3))

bc_comp_model_final <-lme(burrow_count_comp~treatment + mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
hist(residuals(bc_comp_model_final), main="A")
bc_comp_final_residuals <-residuals(bc_comp_model_final, type='pearson')
bc_comp_final_fitted <-fitted.values(bc_comp_model_final)
#Checking assumptions 
qqnorm(bc_comp_final_residuals, main="B")
qqline(bc_comp_final_residuals)

plot(bc_comp_final_fitted,bc_comp_final_residuals, main="C")
abline(h=0)

#The rest 
plot(bc_comp_final_residuals~treatment, data=gc_mod_df, main="D")
abline(h=0)
plot(bc_comp_final_residuals~mean_air_temp, data=gc_mod_df, main="E")
abline(h=0)
plot(bc_comp_final_residuals~replicate, data=gc_mod_df, main="F")
abline(h=0)

summary(bc_comp_model_final)

bc_comp_var_rep_resid <- VarCorr(bc_comp_model_final)
bc_comp_var_rep <- as.numeric(bc_comp_var_rep_resid[1])
bc_comp_var_resid <- as.numeric(bc_comp_var_rep_resid[2])

bc_comp_var_rep/(bc_comp_var_rep + bc_comp_var_resid)

#Correlation between observations is low (0.24)


#Test to see if random slopes were needed 
bc_comp_model_final_rand <-lme(burrow_count_comp~treatment + mean_air_temp, random= (~1+mean_air_temp|replicate), method = 'REML', data=gc_mod_df)

anova(bc_comp_model_final, bc_comp_model_final_rand)
#Thou shalt not use random slopes, only random intercepts

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
hist(residuals(bd_comp_model_final), main="A")
bd_comp_final_residuals <-residuals(bd_comp_model_final, type='pearson')
bd_comp_final_fitted <-fitted.values(bd_comp_model_final)
#Checking assumptions 
qqnorm(bd_comp_final_residuals, main="B")
qqline(bd_comp_final_residuals)
plot(bd_comp_final_fitted,bd_comp_final_residuals, main="C")
abline(h=0)

#The rest 
plot(bd_comp_final_residuals~treatment, data=gc_mod_df, main="D")
abline(h=0)
plot(bd_comp_final_residuals~mean_air_temp, data=gc_mod_df, main="E")
abline(h=0)
plot(bd_comp_final_residuals~replicate, data=gc_mod_df, main="F")
abline(h=0)

summary(bd_comp_model_final)

#https://www.youtube.com/watch?v=XhHLAqd9Qrg @ 45 seconds 

ctrl <- lmeControl(opt='optim')
bd_comp_model_final_rand <-lme(mean_burrow_diameter_comp~treatment + mean_air_temp, random= ~1+treatment|replicate, control=ctrl, method = 'REML', data=gc_mod_df)

anova(bd_comp_model_final, bd_comp_model_final_rand )

summary(bd_comp_model_final)
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

#You can do the same here with burrow density, but I wanted something a little more 
sjPlot::plot_model(bc_comp_model_final)

sjPlot::plot_model(bc_comp_model_final,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(bc_comp_model_final, 
                   axis.labels=c("Temperature", "Rake", "Marine Debris", "Compact"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Treatments on Burrow Count")

sjPlot::tab_model(bc_comp_model_final, 
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)","Compact", "Marine Debris", "Rake", "Temperature"),
                  dv.labels= "Treatment and Temperature Effects on Burrow Density")

sjPlot::tab_model(bd_comp_model_final, 
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)","Compact", "Marine Debris", "Rake", "Temperature"),
                  dv.labels= "Treatment and Temperature Effects on Burrow Diameter")

effects_treatment<- effects::effect(term= "mean_air_temp", mod= bc_comp_model_final)
summary(effects_treatment)

df_treat <- as.data.frame(effects_treatment)

#Using predict as a powerful tool 

temp_d <- expand.grid(
  mean_air_temp = seq(10,25,0.1),
  treatment = gc_mod_df$treatment, 
  replicate = levels(gc_mod_df$replicate)[[1]]
)
temp_d

temp_d$bc_pred <-predict(bc_comp_model_final, newdata = temp_d)
temp_d$bd_pred <-predict(bd_comp_model_final, newdata = temp_d)

gg3 <- ggplot(data=temp_d, aes(x = mean_air_temp, y = bc_pred, colour = treatment)) +
  geom_jitter(data=gc_mod_df, aes(x = mean_air_temp, y = burrow_count_comp, color=treatment), width=0.05, alpha=0.5) +
  geom_line(size=1.5)+
  geom_hline(yintercept=0, size=1, alpha=0.5) +
  labs(x="Average Air Temperature (°C)", y= "Burrow Density Compared to First Day") +
  guides(color=guide_legend(title="Treatment"))+
  scale_colour_manual(labels = c("Control", "Compact", "Marine Debris", "Rake"), values = c("#999999", "#56B4E9","#009E73", "#D55E00")) +
  theme(text = element_text(size = 13))  


gg3

gg4 <- ggplot(data=temp_d, aes(x = mean_air_temp, y = bd_pred, colour = treatment)) +
  geom_jitter(data=gc_mod_df, aes(x = mean_air_temp, y = mean_burrow_diameter_comp, color=treatment), width=0.05, alpha=0.5) +
  geom_line(size=1.5) +
  geom_hline(yintercept=0, size=1, alpha=0.5) +
  labs(x="Average Air Temperature (°C)", y= "Burrow Diameter (mm) Compared to First Day") +
  guides(color=guide_legend(title="Treatment"))+
  scale_colour_manual(labels = c("Control", "Compact", "Marine Debris", "Rake"), values = c("#999999", "#56B4E9","#009E73", "#D55E00")) +
  theme(text = element_text(size = 13)) 
gg4

#No data on top 

gg5 <- ggplot(data=temp_d, aes(x = mean_air_temp, y = bc_pred, colour = treatment)) +
  geom_line(size=2)+
  geom_hline(yintercept=0, size=1) +
  labs(x="Average Air Temperature (°C)", y= "Burrow Density Compared to First Day") +
  guides(color=guide_legend(title="Treatment"))+
  scale_colour_manual(labels = c("Control", "Compact", "Marine Debris", "Rake"), values = c("#999999", "#56B4E9","#009E73", "#D55E00")) +
  theme(text = element_text(size = 13))  

gg5

gg6 <- ggplot(data=temp_d, aes(x = mean_air_temp, y = bd_pred, colour = treatment)) +
  geom_line(size=2) +
  geom_hline(yintercept=0, size=1) +
  labs(x="Average Air Temperature (°C)", y= "Burrow Diameter (mm) Compared to First Day") +
  guides(color=guide_legend(title="Treatment"))+
  scale_colour_manual(labels = c("Control", "Compact", "Marine Debris", "Rake"), values = c("#999999", "#56B4E9","#009E73", "#D55E00")) +
  theme(text = element_text(size = 13)) 
gg6


#### Stats Manuscript Extras ####

#AIC tabs 

Cand.models_bc <- list( )
Cand.models_bc[[1]] <- lme(burrow_count_comp~treatment + mean_air_temp + treatment*mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
Cand.models_bc[[2]] <- lme(burrow_count_comp~treatment + mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)  
Cand.models_bc[[3]] <- lme(burrow_count_comp~mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
Cand.models_bc[[4]] <- lme(burrow_count_comp~treatment, random= ~1|replicate, method = 'REML', data=gc_mod_df)
Cand.models_bc[[5]] <- lme(burrow_count_comp~treatment + mean_air_temp, random= (~1+mean_air_temp|replicate), method = 'REML', data=gc_mod_df)


Modnames_bc <- paste("mod", 1:length(Cand.models_bc), sep = " ")

aictab(cand.set = Cand.models_bc, modnames = Modnames_bc, sort = TRUE)

Cand.models_bd <- list( )
Cand.models_bd[[1]] <- lme(mean_burrow_diameter_comp~treatment + mean_air_temp + treatment*mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
Cand.models_bd[[2]] <- lme(mean_burrow_diameter_comp~treatment + mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)  
Cand.models_bd[[3]] <- lme(mean_burrow_diameter_comp~mean_air_temp, random= ~1|replicate, method = 'REML', data=gc_mod_df)
Cand.models_bd[[4]] <- lme(mean_burrow_diameter_comp~treatment, random= ~1|replicate, method = 'REML', data=gc_mod_df)
Cand.models_bd[[5]] <- lme(mean_burrow_diameter_comp~treatment + mean_air_temp, random= (~1+mean_air_temp|replicate), control=ctrl, method = 'REML', data=gc_mod_df)


Modnames_bd <- paste("mod", 1:length(Cand.models_bd), sep = " ")

aictab(cand.set = Cand.models_bd, modnames = Modnames_bd, sort = TRUE)



