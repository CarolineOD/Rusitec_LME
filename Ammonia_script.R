# working with VFA file 
setwd("//Volumes/GoogleDrive-106875867960902982089/Shared drives/MEL-GHG Caroline O'Donnell/2020:2021 Year 1/Rusitec/Run 3 - 5 halides (January 2021)/R folder")


VFA_run4 = read.table('Ammonia_R3.csv', header=T, sep=',',na.strings=c("","NA"))
# identify which rows contain NA
row.has.na <- apply(VFA_run4, 1, function(x){any(is.na(x))})
# see how many there are. 
sum(row.has.na)
# now cut them out
#filtered <- VFA_run4[!row.has.na,]

# Omit any negative values in results column 
# return 0 for any negative valu
#filtered.new <- ifelse(filtered$Result < 0, 0, filtered$Result)

#final.filtered <- filtered[filtered >= 0]

#head(final.filtered)


#filtered[apply(filtered[,-1], 1, function(x) !all(x==0)),]

#filtered <- filtered[filtered$Result >= 0, ]



# now rename back to VFA for ease

VFA = VFA_run4

library(stringr)

Sam_id = read.table('ID_key_ammonia_R3.csv', header=T, sep=',',na.strings=c("","NA"))
Meta_data = read.table('Meta_data.txt', header=T, sep=',')
str(VFA)
str(Meta_data)

ALLdataMy = merge(VFA, Sam_id, by = 'Sample_ID')

# can rename back to 'VFA' so don't have to change any script below this
VFA <- ALLdataMy

# write data our of R
write.csv(VFA, "Ammonia.csv")


#now I want to extract the sample number from the replicate value (eg. extract 123A into a separate column with 123)


# we'll use stringr function string_extract() to pull any numbers out of vairable

# prepare regular expression
# this means any digit. and we'll call the pattern regexp
#regexp <- "[[:digit:]]+" 

# use string extract to pull all numbers out of our variable called names
# and put it in a new variable called day
#VFA$Sam_ID <- str_extract(VFA$Sample_ID, regexp)
#head(VFA)
#str(VFA) # use str to check format. At the moment the digits in 'day' are being
# seen as characters. If wanted to do any 'maths' on it would need to convert it to numeric

#VFA$Sam_ID <- as.numeric(as.character(VFA$Sam_ID))
#str(VFA)


#Rename Sample_ID to Sam_ID in the identification_key dataset (to merge with VFA data frame)
#colnames(Identification_key)[colnames(Identification_key) == "Sample_ID"] ="Sam_ID"
#head(Identification_key)
#head(VFA)

#now that I have a new column with sample_ID without letters, I can merge the identification key based on this
#VFA = merge(VFA, Identification_key, by = 'Sam_ID')
#head(VFA)

VFA = merge(VFA, Meta_data, by = 'Tmt')
head(VFA)

# write data our of R
write.csv(VFA, "Ammonia.csv")


library(dplyr)
VFA1 <- group_by(VFA, Treatment)

head(VFA1)

library(ggplot2)
library(readxl)
library(reshape2)
library(stringr)
library(dplyr)


###################################
####### Plotting #######


# as usual, we'll use ggplot2 to make our pretty plots
# like always, these take the same basic format of: 
# ggplot(NameOfMyDataset, aes(x = xAxisVariable, y= yAxisVariable, fill=ColourFactor)) + # remember the PLUS sign!
# geom_boxplot() # or any layer you want can be geom_bar() or geom_line() etc etc


# first,the basic plot 
ggplot(VFA, aes(Treatment, Ammonia, fill = Treatment)) + # define variables
  geom_boxplot(size=1) 

# so, because we have different VFAs (acetate, butyrate etc) & we havent chosen
# these as a variable to plot, the current plot is considering ALL the VFAs at once
# So, we will now make separate plots, or facets, for each of the VFAs. remember
# our different VFAs are stored under the variable 'Name'. So we'll use the ggplot
# function facet_wrap() to repeat the same plot for each level of 'Name' ie for
# each VFA
ggplot(VFA, aes(Treatment, Ammonia, fill = Treatment)) + # define variables
  geom_boxplot() # and facet wrap on the variable 'Name' ie VFA

# this is getting better, but as some VFAs (acetate) have high concentrations and others
# have low concentrations, those with low concentrations are hard to see as everything
# is to the same y axis scale. By adding argument scales="free_y" to facet_wrap() we make each
# facet have their own y axis scale
ggplot(VFA, aes(Treatment, Ammonia, fill = as.factor(Treatment))) + # define variables
  geom_boxplot()

# oooh much better! But, now I notice my treatments are in a weird/illogical order. 
# I'll re order them how i want by listing them in the right order
VFA$Treatment <- factor(VFA$Treatment, levels = c("Control","0.5X UHP+KI", "0.5X UHP", "0.25X UHP+KI", "0.25X UHP", "0.5X MgO2"))

# now when we plot it again, our treatments should be in the order we want.
ggplot(VFA, aes(Treatment, Ammonia, fill = Treatment)) + 
  geom_boxplot() 

# now we'll start making our plot a little prettier looking bit by bit
# normally you'd do this at once, but to make clear what is happening
# we'll do slowly, and add a comment for each line to explain what's happening

# this time we'll assign a name (plot1) to our plot. So, if we want to see them we have to call
# the plot by name (ie by writing name of plot and sending to console)

plot1 = ggplot(VFA, aes(Treatment, Ammonia, fill = factor(Treatment))) + # define variables
  geom_boxplot(size=0.5) + # by adding size=1 the lines for the boxplot become a bit thicker
  # and easier to see
  theme_bw() + # we'll use a built in theme called theme_bw() to remove grey background
  labs(y = "Ammonia (mg/L)")  # i want to change my y axis label

plot1 
# look it's getting prettier!
# next i want to choose different colours and change text size. 
# I'll use scale_fill_manual() to choose the colours I want, calling them by 
# ggplot2 naming convention. . 
# you can find the ggplot2 colours by googling them
# eg here: http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# I can add these layers to my plot simply by calling the plot I made and 
# adding to it. I'll give the resulting plot a new name (plot2)
plot2= plot1 #+
#scale_fill_manual(values = c('chocolate4', 'black', 'olivedrab', 'gold2', 
#  'darkred', 'blue', 'pink')) # choose colours to match other plots  

# call the plot to see it
plot2

# now I'll use the function theme() to change the size and colour of text,
# background etc. There are loads of things you can change under theme. 
# https://ggplot2.tidyverse.org/reference/theme.html

plot3 = plot2 +
  theme(axis.title.x = element_blank(), # remove x axis title
        axis.text.x = element_text(angle = 45, vjust=1,hjust=1), # remove x axis text
        text = element_text(size=12, color = "black"), # increase text size
        axis.text = element_text(color="black"), # change axis labels from default grey to black
        legend.text = element_text(size=12), # increase size of legend text
        strip.text.x = element_text(size = 16, colour = "black"),# change font of facet label
        strip.background =  element_rect(fill = "white"), # remove grey backgroup facet label
        legend.key.size = unit(0.25, "cm") # change legend key size
        
  )

plot3

# we dont need the legend on the side really, as its the same info as on
# x axis labels. So we'll turn off legend with guides(fill=FALSE)
# at the same time, we'll update our x axis labels to be slightly nicer
# we'll call them 'myLabs' -> MAKE SURE these are in the right order or you'll 
# label things incorrectly. 
myLabs = c("Control","0.5X UHP+KI", "0.5X UHP", "0.25X UHP+KI", "0.25X UHP", "0.5X MgO2") 

plot3 + guides(fill=FALSE) + scale_x_discrete(labels= myLabs)


# to export this plot, you can go to the plot window over there -> 
# and chose export > as image
# then, in the pop up that appears, drag the bottom right triangle to choose
# the size you want. Also change the file name and where you want to save it to
# if its not your working dir. then click save. 

# there are more complex ways to save plots but we'll leave it at that for now :) 


##################################################
### statistical analyses =====

## Running ANOVA: for acetate ====

# we use the built in R function aov() which takes the format
# aov(numberVariable ~ FactorVariable, data= NameOfDataset)
# we'll save the outtput to resAc.aov
Ammonia.aov <- aov(Ammonia ~ Treatment, data = VFA)
# Then check if there any stat sig differences by looking at summary of the analysis
summary(Ammonia.aov)

# output to console should look like this:

#             Df Sum Sq Mean Sq F value Pr(>F)   
# Tmt          4 611658  152915   4.829 0.0064 **
# Residuals   21 665046   31669                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# what we are interested in, is the Pr(>F) value
# when a stat sig difference is observed, one, two or three asterisks will 
# be put next to this number, where typically  a number less than 0.05 is seen as 
# significant 

# now, we know that somewhere we have a statisticaly significant difference.
# but we don't know between which treatmetns that difference is. To know this, we do a post-hoc
# test. For ANOVA the most used post hoc is called a Tukey's post hoc.
# we run this on the output from our aov() analysis.


TukeyHSD(Ammonia.aov)

# the output to the console, should look like this:

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = VFA ~ Tmt, data = acetate)
# 
# $Tmt
#                       diff        lwr      upr     p adj
# Negative-Inoculum   -36.02311 -468.88201 396.8358 0.9990939
# Untreated-Inoculum -229.96100 -662.81991 202.8979 0.5238380
# 7mgLARS-Inoculum     32.72322 -400.13569 465.5821 0.9993794
# 8mgLARS-Inoculum    216.23906 -216.61984 649.0980 0.5808070
# Untreated-Negative -193.93790 -500.01536 112.1396 0.3539738
# 7mgLARS-Negative     68.74633 -237.33114 374.8238 0.9609244
# 8mgLARS-Negative    252.26217  -53.81530 558.3396 0.1396144
# 7mgLARS-Untreated   262.68422  -43.39325 568.7617 0.1156419
# 8mgLARS-Untreated   446.20007  140.12260 752.2775 0.0023814
# 8mgLARS-7mgLARS     183.51584 -122.56162 489.5933 0.4071118

# the variable we're interested in is p adj. 
# this is the p value, adjusted for multiple tests. 
# in the example above we can see there is one stat. sig. difference and 
# that is between 8mgLARS-Untreated which has a p value of 0.00238

## This stats analysis isn't perfect - an ANOVA isn't always the right choice
# for every type of data. Usually we do certain tests on our data first (such as
# check for the normality of the distrubution of our data) and depending on that, we
# choose the best statistical test to do. But this is just an introduction so
# we'll stick with ANOVA and Tukey post hoc. 


library(dplyr)
VFA %>%
  group_by(Treatment) %>%
  summarise_at(vars(Ammonia), list(name = mean))

