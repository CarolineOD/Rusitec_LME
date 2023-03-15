# New Rusitec script put together after meeting with Cam Fri 27th Jan '23
# We're using the linear mixed model, which doesn't require data to be normally distributed (the residuals should be normally distributed)

#3 parts to this script:
# A: Running LME
# B: Descriptive statistics to ensure data meets model assumptions
# c: Producing nice data outputs (tables, plots etc)

library(dplyr)
library(car)
library(ggplot2)
library(nlme)
library(emmeans)

library(sjstats)
library(lmerTest)
library(MuMIn)

options(scipen = 999) #tells R to use scientific notation

# set wkdir and read in file
setwd("/Users/carolineodonnell/Library/CloudStorage/GoogleDrive-caroline@oflahertylab.com/Shared drives/MEL-GHG Caroline O'Donnell/1. Rusitec /1. Rusitec/2. Run 3 - 5 halides (January 2021)/R folder")

df <-read.csv("Run_3_all_data.csv", head=T)
str(df)
df=na.omit(df) # remove rows with NA. careful with this command


# The linear mxed model can be used to analyse outcome data that are continuous in nature 

# mixed effects models using lme()
# lme = function for linear mixed effects model, which accounts for both fixed effects and random effects


# Our two independent variables are day and Treatment
# vessel we'll test for random interaction

# first thing, linear models are based on intercepts. Our day variable starts
# at 15 which is the correct day of experiment, but for the purposes of the model,
# day 15 is the first day and day 20 the last. So to avoid errors we should reset
# day 15 to = day 0 and so on. Here is link to better explanation. (around 6 min mark)
# https://www.youtube.com/watch?v=cr9RpSgRYVw
# $ To access one variable in a dataset
df$RunDay <- df$Day - 15 # make new variable called RunDay which is day minus 15
head(df)
str(df)
df$RunDay<- as.factor(df$RunDay) # convert it to a factor
df=na.omit(df)
str(df)
df$Vessel <- as.factor(df$Vessel)
str(df)

##################################################################################################


# ML (maximum liklihood) differs from REML (restricted maximum liklihood) in how the random effects are estimated and p-values computed 
# for the fixed effects. (Note that ML is not available for combined split-plot designs.) 
# REML is the default in most cases because it provides unbiased estimates, while 
# ML is only unbiased for large designs.

#using 'RunDay' as random effect will allow for between and within replicate variation over time, but by 
#using 'Vessel' instead of 'RunDay' as random effect, that should account for between and within replicate variation over time also?
#mod1 should tell us the effect of treatment on CH4 controlling for linear change over time
mod1 <- lme(Gas_vol_L~RunDay+Treatment,random=~1|Vessel,
            data=df,
            method='REML')
#mod2 if you want to see if the treatment effect changes over time then fit the interaction model
mod2 <- lme(Gas_vol_L, ~RunDay*Treatment,random=~1|Vessel,
            data=df,
            method='REML')

summary(mod1)
# loglik -75.33533
summary(mod2)
# loglik -66.61038

#what does anova of the model tell us?
anova(mod1)
anova(mod2)

#emmeans and lsmeans are just two different ways to compare treatments and determine whether contrast is significant via p value
emmeans(mod1, pairwise~Treatment) ### post hoc est marginal means
lsmeans(mod1, pairwise~Treatment) ### least square means (used in SAS script)

emmeans(mod2, pairwise~Treatment) ### post hoc est marginal means
lsmeans(mod2, pairwise~Treatment) ### least square means (used in SAS script)


##################################################################################################


# Descriptive statistics to check for normality
# for LME the residuals need to be normally distributed 
df$res = residuals(mod1)
df$pred = predict(mod1, df)

hist(df$res) # should be symmetric
plot(df$pred, df$res) # should be a random scatter about 0


##################################################################################################

# table to output
comp1 <- lsmeans(mod1, pairwise~Treatment, adjust='tukey') ### 
as.data.frame(comp1$contrasts)

tbl_gas_vol <- as.data.frame(comp1$contrasts)
out<-df %>% 
  group_by(Treatment) %>% 
  summarize(mn=mean(Gas_vol_L), 
            sdev=sd(Gas_vol_L), 
            num=n(),
            er=sd(Gas_vol_L/sqrt(num)))
ctrl<- tbl_gas_vol[grep("Control", tbl_gas_vol$contrast),]


library(stringr)

ctrl$Treatment <- sapply(str_split(ctrl$contrast, "-"), "[", 1)
ctrl$Treatment <- gsub(pattern = "\\(",replacement="", ctrl$Treatment) 
ctrl$Treatment <- gsub(pattern = "\\)",replacement="", ctrl$Treatment) 
out$Treatment = trimws(out$Treatment)
ctrl$Treatment = trimws(ctrl$Treatment)
fulltable_gas_vol = merge(ctrl, out, by='Treatment', all=TRUE)



##################################################################################################

# Repeat above 3 steps for all independent variables

# CH4_pc #
#LME
mod3 <- lme(CH4_pc~RunDay+Treatment,random=~1|Vessel,
            data=df,
            method='REML')
mod4 <- lme(CH4_pc~RunDay*Treatment,random=~1|Vessel,
            data=df,
            method='REML')
summary(mod3)
summary(mod4)
emmeans(mod3, pairwise~Treatment)
lsmeans(mod4, pairwise~Treatment) ### Got error message: NOTE: Results may be misleading due to involvement in interactions
emmeans(mod3, pairwise~Treatment) 
lsmeans(mod4, pairwise~Treatment) ### Got error message: NOTE: Results may be misleading due to involvement in interactions
#Checking assumptions
df$res = residuals(mod3)
df$pred = predict(mod3, df)
hist(df$res) 
plot(df$pred, df$res) 
# table to output
comp2 <- lsmeans(mod3, pairwise~Treatment, adjust='tukey') ### 
as.data.frame(comp2$contrasts)

tbl_CH4_pc <- as.data.frame(comp2$contrasts)
out<-df %>% 
  group_by(Treatment) %>% 
  summarize(mn=mean(CH4_pc), 
            sdev=sd(CH4_pc), 
            num=n(),
            er=sd(CH4_pc/sqrt(num)))
ctrl<- tbl_CH4_pc[grep("Control", tbl_CH4_pc$contrast),]

ctrl$Treatment <- sapply(str_split(ctrl$contrast, "-"), "[", 1)
ctrl$Treatment <- gsub(pattern = "\\(",replacement="", ctrl$Treatment) 
ctrl$Treatment <- gsub(pattern = "\\)",replacement="", ctrl$Treatment) 
out$Treatment = trimws(out$Treatment)
ctrl$Treatment = trimws(ctrl$Treatment)
fulltable_CH4_pc = merge(ctrl, out, by='Treatment', all=TRUE)

write.csv(fulltable_CH4_pc, file='Run3_StatsOutput_CH4_pc.csv',row.names=F,quote=F)







########## CH4_vol_L #########
mod4a <- lme(CH4_vol_L~RunDay+Treatment,random=~1|Vessel,
             data=df,
             method='REML',correlation=corCompSymm())

summary(mod4a)

anova(mod4a)
lsmeans(mod4a, pairwise~Treatment) 
comp4 <- lsmeans(mod4a, pairwise~Treatment, adjust='tukey') ### 
as.data.frame(comp4$contrasts)

tbl_CH4_pc <- as.data.frame(comp4$contrasts)
out<-df %>% 
  group_by(Treatment) %>% 
  summarize(mn=mean(CH4_vol_L), 
            sdev=sd(CH4_vol_L), 
            num=n(),
            er=sd(CH4_vol_L/sqrt(num)))
ctrl<- tbl_CH4_pc[grep("Control", tbl4$contrast),]

ctrl$Treatment <- sapply(str_split(ctrl$contrast, "-"), "[", 1)
ctrl$Treatment <- gsub(pattern = "\\(",replacement="", ctrl$Treatment) 
ctrl$Treatment <- gsub(pattern = "\\)",replacement="", ctrl$Treatment) 
out$Treatment = trimws(out$Treatment)
ctrl$Treatment = trimws(ctrl$Treatment)
fulltable_CH4_vol_L = merge(ctrl, out, by='Treatment', all=TRUE)

write.csv(fulltable_CH4_vol_L, file='Run3_StatsOutput_CH4_vol_L.csv',row.names=F,quote=F)

########## CH4mmol_day #########
mod4a <- lme(CH4mmol_day~RunDay+Treatment,random=~1|Vessel,
             data=df,
             method='REML',correlation=corCompSymm())

summary(mod4a)

anova(mod4a)
lsmeans(mod4a, pairwise~Treatment) 
comp4 <- lsmeans(mod4a, pairwise~Treatment, adjust='tukey') ### 
as.data.frame(comp4$contrasts)

tbl_CH4mmol_day <- as.data.frame(comp4$contrasts)
out<-df %>% 
  group_by(Treatment) %>% 
  summarize(mn=mean(CH4mmol_day), 
            sdev=sd(CH4mmol_day), 
            num=n(),
            er=sd(CH4mmol_day/sqrt(num)))
ctrl<- tbl_CH4mmol_day[grep("Control", tbl4$contrast),]

ctrl$Treatment <- sapply(str_split(ctrl$contrast, "-"), "[", 1)
ctrl$Treatment <- gsub(pattern = "\\(",replacement="", ctrl$Treatment) 
ctrl$Treatment <- gsub(pattern = "\\)",replacement="", ctrl$Treatment) 
out$Treatment = trimws(out$Treatment)
ctrl$Treatment = trimws(ctrl$Treatment)
fulltable_CH4mmol_day = merge(ctrl, out, by='Treatment', all=TRUE)

write.csv(fulltable_CH4mmol_day, file='Run3_StatsOutput_CH4mmol_day.csv',row.names=F,quote=F)


########## Total_OMD #########
mod4a <- lme(Total_OMD~RunDay+Treatment,random=~1|Vessel,
             data=df,
             method='REML',correlation=corCompSymm())

summary(mod4a)

anova(mod4a)
lsmeans(mod4a, pairwise~Treatment) 
comp4 <- lsmeans(mod4a, pairwise~Treatment, adjust='tukey') ### 
as.data.frame(comp4$contrasts)

tbl_Total_OMD <- as.data.frame(comp4$contrasts)
out<-df %>% 
  group_by(Treatment) %>% 
  summarize(mn=mean(Total_OMD), 
            sdev=sd(Total_OMD), 
            num=n(),
            er=sd(Total_OMD/sqrt(num)))
ctrl<- tbl_Total_OMD[grep("Control", tbl4$contrast),]

ctrl$Treatment <- sapply(str_split(ctrl$contrast, "-"), "[", 1)
ctrl$Treatment <- gsub(pattern = "\\(",replacement="", ctrl$Treatment) 
ctrl$Treatment <- gsub(pattern = "\\)",replacement="", ctrl$Treatment) 
out$Treatment = trimws(out$Treatment)
ctrl$Treatment = trimws(ctrl$Treatment)
fulltable_Total_OMD = merge(ctrl, out, by='Treatment', all=TRUE)

write.csv(fulltable_Total_OMD, file='Run3_StatsOutput_Total_OMD.csv',row.names=F,quote=F)


########## mmolCH4gDOM #########
mod4a <- lme(mmolCH4gDOM~RunDay+Treatment,random=~1|Vessel,
             data=df,
             method='REML',correlation=corCompSymm())

summary(mod4a)

anova(mod4a)
lsmeans(mod4a, pairwise~Treatment) 
comp4 <- lsmeans(mod4a, pairwise~Treatment, adjust='tukey') ### 
as.data.frame(comp4$contrasts)

tbl_mmolCH4gDOM <- as.data.frame(comp4$contrasts)
out<-df %>% 
  group_by(Treatment) %>% 
  summarize(mn=mean(mmolCH4gDOM), 
            sdev=sd(mmolCH4gDOM), 
            num=n(),
            er=sd(mmolCH4gDOM/sqrt(num)))
ctrl<- tbl_mmolCH4gDOM[grep("Control", tbl4$contrast),]

ctrl$Treatment <- sapply(str_split(ctrl$contrast, "-"), "[", 1)
ctrl$Treatment <- gsub(pattern = "\\(",replacement="", ctrl$Treatment) 
ctrl$Treatment <- gsub(pattern = "\\)",replacement="", ctrl$Treatment) 
out$Treatment = trimws(out$Treatment)
ctrl$Treatment = trimws(ctrl$Treatment)
fulltable_mmolCH4gDOM = merge(ctrl, out, by='Treatment', all=TRUE)

write.csv(fulltable_mmolCH4gDOM, file='Run3_StatsOutput_mmolCH4gDOM.csv',row.names=F,quote=F)


########## Vessel_pH #########
mod4a <- lme(Vessel_pH~RunDay+Treatment,random=~1|Vessel,
             data=df,
             method='REML',correlation=corCompSymm())

summary(mod4a)

anova(mod4a)
lsmeans(mod4a, pairwise~Treatment) 
comp4 <- lsmeans(mod4a, pairwise~Treatment, adjust='tukey') ### 
as.data.frame(comp4$contrasts)

tbl_Vessel_pH <- as.data.frame(comp4$contrasts)
out<-df %>% 
  group_by(Treatment) %>% 
  summarize(mn=mean(Vessel_pH), 
            sdev=sd(Vessel_pH), 
            num=n(),
            er=sd(Vessel_pH/sqrt(num)))
ctrl<- tbl_Vessel_pH[grep("Control", Vessel_pH$contrast),]

ctrl$Treatment <- sapply(str_split(ctrl$contrast, "-"), "[", 1)
ctrl$Treatment <- gsub(pattern = "\\(",replacement="", ctrl$Treatment) 
ctrl$Treatment <- gsub(pattern = "\\)",replacement="", ctrl$Treatment) 
out$Treatment = trimws(out$Treatment)
ctrl$Treatment = trimws(ctrl$Treatment)
fulltable_Vessel_pH = merge(ctrl, out, by='Treatment', all=TRUE)

write.csv(fulltable_Vessel_pH, file='Run3_StatsOutput_Vessel_pH.csv',row.names=F,quote=F)




