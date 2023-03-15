# New Rusitec script put together after meeting with Cam Fri 27th Jan '23
# We're using the linear mixed model, which doesn't require data to be normally distributed (the residuals should be normally distributed (I think))
library(dplyr)
library(car)
library(ggplot2)
library(nlme)
library(emmeans)

library(sjstats)
library(lmerTest)
library(nlme)
library(MuMIn)

options(scipen = 999)

# set wkdir and read in file
setwd("/Users/carolineodonnell/Library/CloudStorage/GoogleDrive-caroline@oflahertylab.com/Shared drives/MEL-GHG Caroline O'Donnell/1. Rusitec /1. Rusitec/2. Run 3 - 5 halides (January 2021)/R folder")

df <-read.csv("Run_3_all_data.csv", head=T)
str(df)
df=na.omit(df) # remove rows with NA. careful with this command

str(df)

# This is the univariate analysis (descriptive statistics) that Teagasc use to check for normality
#
# There are 3 parts to the analysis:
# 1. plot distribution of variable and compare to normal dist
# 2. plot quantile comparison (observed  vs expected (normal) quartiles)
# 3. Test for normality - Shapiro Wilk


# distribution of response variable ie gas vols
ggplot(df, aes(Vessel_pH))  + 
  geom_histogram(colour = "black")

# use max and min to create desired binwidth and number of non-missing obs
bw = (max(df$Vessel_pH) - min(df$Vessel_pH) )/ (nrow(df)/10)
n_obs = sum(!is.na(df$Vessel_pH))

# now plot data with normal curve overlayed
ggplot(df, aes(Vessel_pH))  + 
  geom_histogram(colour = "black", binwidth = bw) + 
  stat_function(fun = function(x) 
    dnorm(x, mean = mean(df$Vessel_pH), sd = sd(df$Vessel_pH)) * bw * n_obs, colour='blue')

# q- q plots showing comparison of actual vs estimated quartiles
qqPlot(df$Vessel_pH)
# if normally dist. most of points should fall along the reference line
# the two numbers printed on console & on plot are the 2 most extreme residuals

# use shapiro wilk to test for normality where H0 is that it is normal
# ie p value > 0.05 means data normaal; pvalue < 0.05 means data not-norma
shapiro.test(df$Vessel_pH)
# p > 0.05 so it's normal distributed.



# The linear mxed model can be used to analyse outcome data that are continuous in nature 
# ****** Question: Does the model I'm using assume normal distribution

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
df$RunDay<- as.factor(df$RunDay) # convert it to a factor
df=na.omit(df)
str(df)
df$Vessel <- as.factor(df$Vessel)

# ML differs from REML in how the random effects are estimated and p-values computed 
# for the fixed effects. (Note that ML is not available for combined split-plot designs.) 
# REML is the default in most cases because it provides unbiased estimates, while 
# ML is only unbiased for large designs.

mod4 <- lme(Gas_vol_L~RunDay+Treatment,random=~1|Vessel,
            data=df,
            method='REML')

summary(mod4)
# loglik -75.33533

anova(mod4)
emmeans(mod4, pairwise~Treatment) ### post hoc est marginal means

lsmeans(mod4, pairwise~Treatment) ### least square means (used in SAS script)


# include compound symmetry thing (need to work out why this was in original
# SAS method and what means)
mod4a <- lme(Gas_vol_L~RunDay+Treatment,random=~1|Vessel,
             data=df,
             method='REML',correlation=corCompSymm())

summary(mod4a)
# loglik -75.33533
# doesnt really improve our model but we'll leave it anyway
anova(mod4a)
lsmeans(mod4a, pairwise~Treatment) ### 
# sig pees b/w ctrl+uhp0.5 and ctrl+LARS0.5

# if we want these as table to output
comp4 <- lsmeans(mod4a, pairwise~Treatment, adjust='tukey') ### 
as.data.frame(comp4$contrasts)

tbl_gas_vol <- as.data.frame(comp4$contrasts)
out<-df %>% 
  group_by(Treatment) %>% 
  summarize(mn=mean(Gas_vol_L), 
            sdev=sd(Gas_vol_L), 
            num=n(),
            er=sd(Gas_vol_L/sqrt(num)))
ctrl<- tbl4[grep("Control", tbl_gas_vol$contrast),]


library(stringr)

ctrl$Treatment <- sapply(str_split(ctrl$contrast, "-"), "[", 1)

ctrl$Treatment <- gsub(pattern = "\\(",replacement="", ctrl$Treatment) 

ctrl$Treatment <- gsub(pattern = "\\)",replacement="", ctrl$Treatment) 

out$Treatment = trimws(out$Treatment)
ctrl$Treatment = trimws(ctrl$Treatment)

fulltable_gas_vol = merge(ctrl, out, by='Treatment', all=TRUE)

########## CH4_pc #########
mod4a <- lme(CH4_pc~RunDay+Treatment,random=~1|Vessel,
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
  summarize(mn=mean(CH4_pc), 
            sdev=sd(CH4_pc), 
            num=n(),
            er=sd(CH4_pc/sqrt(num)))
ctrl<- tbl_CH4_pc[grep("Control", tbl4$contrast),]

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



setwd("/Users/carolineodonnell/Library/CloudStorage/GoogleDrive-caroline@oflahertylab.com/Shared drives/MEL-GHG Caroline O'Donnell/1. Rusitec /1. Rusitec/2. Run 3 - 5 halides (January 2021)/R folder")

NH3 <-read.csv("Ammonia.csv", head=T)

str(NH3)


