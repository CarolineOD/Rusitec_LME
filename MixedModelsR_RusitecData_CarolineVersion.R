
## load libraries


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
setwd("/Volumes/GoogleDrive-106875867960902982089/Shared drives/MEL-GHG Caroline O'Donnell/1. Rusitec /1. Rusitec/2. Run 3 - 5 halides (January 2021)/R folder")

df <-read.csv("Run_3_all_data.csv", head=T)
str(df)
df=na.omit(df) # remove rows with NA. careful with this command

str(df)

### 1. univariate analysis aka descriptive statistics  =====

# 1. plot distribution of variable and compare to normal dist
# 2. plot quantile comparison (observed  vs expected (normal) quartiles)
# 3. Test for normality - Shapiro Wilk
#

dfn <- as.matrix(select_if(df, is.numeric)) # 
pairs(dfn)  # not as relevant in this dataset but can  be used
          
# distribution of response variable ie gas vols

# change 'df' for name of your data frame
# change 'Gas_vol_L' for name of your variable
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

## q- q plots showing comparison of actual vs estimated quartiles

qqPlot(df$Vessel_pH)
# if normally dist. most of points should fall along the reference line
# the two numbers printed on console &on plot are the 2 most extreme residuals

# use shapiro wilk to test for normality where H0 is that it is normal
# ie p value > 0.05 means data normaal; pvalue < 0.05 means data not-norma

shapiro.test(df$Vessel_pH)
# p > 0.05 so it's normall distributed.

## now we can proceed to making our mixed model
##the above is for checking normality of data before proceeding to statistical analysis
############################################################################################

#Total organic matter digested, CH4mmol/gDOM, CH4mmol_day, CH4mmol_day, CH4 volume, CH4% 
#not normally distributed - need to transform data before running mixed effects model.
#transform using log function?






############################################################################################

## mixed effects models using lme() (lme = function for linear mixed effects model, which 
# accounts for both fixed effects and random effects =====

# so our two independent variables are day and Treatment
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

# look at the data
lattice::dotplot(Gas_vol_L~Treatment, data=df) # by treatment
lattice::dotplot(Gas_vol_L~RunDay, data=df) # by day
lattice::dotplot(Gas_vol_L~Vessel, data=df) # by vessel
lattice::dotplot(Gas_vol_L~Machine, data=df) # by machine


# run unconditional means model including our var of interest and
# with fixed effects and our subject 'vessel' as  random intercept

# This unconditional means model has one fixed effect that estimates the grand mean of the response 
# across all occasions and individuals. The main reason to fit this model is to 
# examine the random effects (i.e., the within-person and between-person variance components).


mod1 <- lme(Gas_vol_L~1,random=~1|Vessel,data=df,method='ML')
summary(mod1)
# logLik = -74.24611
# BIC 162.4254
intervals(mod1)

# loglik = The log-likelihood value of a regression model is a way to measure the 
# goodness of fit for a model. The higher the value of the log-likelihood, the 
# better a model fits a dataset. ie closer to 0 the better.


################################################################################
#New code Cam sent 
plot(mod1)
qqnorm(resid(mod1))
qqline(resid(mod1))
qqmath(resid(mod1),id=0.05)

# this next one is a nice wee package as it also shows which of your 
# data points are 'extreme' and might be skewing the data

install.packages("multilevelTools", dependencies = TRUE)

library(JWileymisc)
library(multilevelTools)

md <- modelDiagnostics(mod1)
md$extremeValues
plot(md, ncol = 2, nrow = 2)
plot(md, ncol = 2, nrow = 3)

################################################################################

# calculate intra class correlation coefficient to determine if we should
# proceed with multilevel modelling. 

# Intra class correlation coefficient is a descriptive statistic that can be used when quantitative measurements 
# are made on units that are organized into groups. It describes how strongly 
# units in the same group resemble each other. While it is viewed as a type of 
# correlation, unlike most other correlation measures it operates on data 
# structured as groups, rather than data structured as paired observations.

# Calculate manually using values in
# Random effects output that looked like this eg.

# Random effects:
#   Formula: ~1 | Vessel_sas
#         (Intercept)  Residual
# StdDev:   0.5687577 0.3719964

#(0.5687577^2) / ((0.5687577^2)+(0.3719964^2))

# 0.7 
# means about 70% variability is occurring and needs explaining. This is >5% so gives go ahead 
# the higher the value the more likely multilevel modelling should be used

(0.4603334^2)/((0.4603334^2)+(0.4034653))

# 0.3443556, so 34% variability is occurring, and above 5% so can continue with this model

library(lattice)
# having a quick look at effect of our random effects
# some data from diff vessels varies differently over time
# not so clear with machine
xyplot(Gas_vol_L ~ RunDay|Vessel, data=df, type=c('p','r'),
       group = Treatment, auto.key = TRUE)


xyplot(Gas_vol_L ~ RunDay|Machine, data=df, type=c('p','r'),
       group = Treatment, auto.key = TRUE)


# now we include our first fixed effect, which is day
mod2 <- lme(Gas_vol_L~ RunDay,random=~1|Vessel,data=df,method='ML')
summary(mod2)
# logLik = -72.63915 slight improvement
# no sig p values. RunDay as a fixed slope isn't helping to explain our variance
intervals(mod2)

# try machine as random effect instead of vessel
# updated 01.02.2023 -> won't be using machine as random effect, doesn't explain our model very well

#mod2x <- lme(Gas_vol_L~ RunDay,random=~1|Machine,data=df,method='ML')
# summary(mod2x)

# Data: df 
# AIC      BIC    logLik
# 200.2089 218.7197 -93.10446
# including vessel as our random effect improves our model much more than
# machine does. actually machine makes it worse (reduces loglik)

#https://www.middleprofessor.com/files/applied-biostatistics_bookdown/_book/regression.html
# library(car)
# mm1 <- lm(Gas_vol_L~ Treatment,data=df)
# set.seed(1)
# qqPlot(mm1, id=FALSE)
# spreadLevelPlot(mm1, id=FALSE)
# summary(mm1)$r.squared


# now adding treatment AND run day as fixed effects
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



tbl4 <- as.data.frame(comp4$contrasts)
out<-df %>% 
  group_by(Treatment) %>% 
  summarize(mn=mean(Gas_vol_L), 
            sdev=sd(Gas_vol_L), 
            num=n(),
            er=sd(Gas_vol_L/sqrt(num)))
ctrl<- tbl4[grep("Control", tbl4$contrast),]

ctrl$Treatment <- sapply(str_split(ctrl$contrast, "-"), "[", 1)
library(stringr)

ctrl$Treatment <- gsub(pattern = "\\(",replacement="", ctrl$Treatment) 

ctrl$Treatment <- gsub(pattern = "\\)",replacement="", ctrl$Treatment) 

out$Treatment = trimws(out$Treatment)
ctrl$Treatment = trimws(ctrl$Treatment)

fulltable = merge(ctrl, out, by='Treatment', all=TRUE)

write.csv(fulltable, file='Run3_StatsOutput.csv',row.names=F,quote=F)


################################################################################


### if we run model as done by TEAGASC using machine instead of vessel
# as our random effect (which didnt explain our model very well but anyway!)
mod5 <- lme(Gas_vol_L~RunDay+Treatment,random=~1|Machine,
             data=df,
             method='REML',correlation=corCompSymm())

summary(mod5)

anova(mod5)
lsmeans(mod5, pairwise~Treatment) ### 
# now we get sig p vals for most of the same interactions as original SAS output

comp5 <- lsmeans(mod5, pairwise~Treatment, adjust='tukey') ### 
as.data.frame(comp5$contrasts)

tbl5 <- as.data.frame(comp5$contrasts)
lsmeans(mod5,
        specs = c("Treatment"),
        model = "multivariate")

library()
## to do this on mol CH4 per day just repeat changing Gas_vol_L variable


