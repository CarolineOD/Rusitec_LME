
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
setwd("/Volumes/GoogleDrive-106875867960902982089/Shared drives/MEL-GHG Caroline O'Donnell/2020:2021 Year 1/Rusitec/Run 3 - 5 halides (January 2021)/R folder")

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
ggplot(df, aes(Gas_vol_L))  + 
  geom_histogram(colour = "black")

# use max and min to create desired binwidth and number of non-missing obs
# change 'df' for name of your data frame
# change 'Gas_vol_L' for name of your variable
bw = (max(df$Gas_vol_L) - min(df$Gas_vol_L) )/ (nrow(df)/10)
n_obs = sum(!is.na(df$Gas_vol_L))

# now plot data with normal curve overlayed
ggplot(df, aes(Gas_vol_L))  + 
  geom_histogram(colour = "black", binwidth = bw) + 
  stat_function(fun = function(x) 
    dnorm(x, mean = mean(df$Gas_vol_L), sd = sd(df$Gas_vol_L)) * bw * n_obs, colour='blue')

## q- q plots showing comparison of actual vs estimated quartiles

qqPlot(df$Gas_vol_L)
# if normally dist. most of points should fall along the reference line
# the two numbers printed on console &on plot are the 2 most extreme residuals

# use shapiro wilk to test for normality where H0 is that it is normal
# ie p value > 0.05 means data normaal; pvalue < 0.05 means data not-norma

# change 'df' for name of your data frame
# change 'Gas_vol_L' for name of your variable
shapiro.test(df$Gas_vol_L)
# p > 0.05 so it's normall distributed.

## now we can proceed to making our mixed model

## mixed effects models using lme() =====

# so our two independent variables are day and Treatment
# vessel we'll test for random interaction

# first thing, linear models are based on intercepts. Our day variable starts
# at 15 which is the correct day of experiment, but for the purposes of the model,
# day 15 is the first day and day 20 the last. So to avoid errors we should reset
# day 15 to = day 0 and so on. Here is link to better explanation. (around 6 min mark)
# https://www.youtube.com/watch?v=cr9RpSgRYVw
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
mod1 <- lme(Gas_vol_L~1,random=~1|Vessel,data=df,method='ML')
summary(mod1)
# logLik = -88.39249
intervals(mod1)


# calculate intra class correlation coefficient to determine if we should
# proceed with multilevel modelling. Calculate manually using values in
# Random effects output that looked like this

# Random effects:
#   Formula: ~1 | Vessel_sas
#         (Intercept)  Residual
# StdDev:   0.5687577 0.3719964

#(0.5687577^2) / ((0.5687577^2)+(0.3719964^2))

# 0.7 
# means about 70% variability is occurring and needs explaining. This is >50% so gives go ahead 
# the higher the value the more likely multilevel modelling should be used

(0.4603334^2)/((0.4603334^2)+(0.4034653))
#not above 50%? 

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
# logLik = -86.8 slight improvement
# no sig p values. RunDay as a fixed slope isont helping to explain our variance
intervals(mod2)

# try machine as random effect instead of vessel
mod2x <- lme(Gas_vol_L~ RunDay,random=~1|Machine,data=df,method='ML')
summary(mod2x)
# Data: df 
# AIC      BIC    logLik
# 281.5137 304.6964 -132.7568
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
# loglik -78.36

anova(mod4)
emmeans(mod4, pairwise~Treatment) ### post hoc est marginal means

lsmeans(mod4, pairwise~Treatment) ### least square means (used in SAS script)

# include compound symmetry thing (need to work out why this was in original
# SAS method and what means)
mod4a <- lme(Gas_vol_L~RunDay+Treatment,random=~1|Vessel,
             data=df,
             method='REML',correlation=corCompSymm())

summary(mod4a)
# loglik -78.35
# doesnt really improve our model but we'll leave it anyway
anova(mod4a)
lsmeans(mod4a, pairwise~Treatment) ### 
# sig pees b/w ctrl+uhp0.5 and ctrl+LARS0.5

# if we want these as table to output
comp4 <- lsmeans(mod4a, pairwise~Treatment, adjust='tukey') ### 
as.data.frame(comp4$contrasts)

tbl4 <- as.data.frame(comp4$contrasts)
write.csv(tbl4, file='Run3_StatsOutput.csv',row.names=F,quote=F)

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
as.data.frame(comp5a$contrasts)

tbl5 <- as.data.frame(comp5$contrasts)
lsmeans(mod5,
        specs = c("Treatment"),
        model = "multivariate")


## to do this on mol CH4 per day just repeat changing Gas_vol_L variable


