
setwd("//Volumes/GoogleDrive-106875867960902982089/Shared drives/MEL-GHG Caroline O'Donnell/2020:2021 Year 1/Rusitec/Run 4 - 2 halides (March 2021) /R folder")



df = read.table('Run_3_all_data.csv', header=T, sep=',',na.strings=c("","NA"))
# identify which rows contain NA
row.has.na <- apply(df, 1, function(x){any(is.na(x))})
# see how many there are. 
sum(row.has.na)
# now cut them out
filtered <- df[!row.has.na,]

#library(stringr)
library(ggplot2)
#library(readxl)
#library(reshape2)
#library(stringr)
#library(dplyr)


#str = looking at structure of dataset 
str(filtered)


###################################
####### Plotting #######


# as usual, we'll use ggplot2 to make our pretty plots
# like always, these take the same basic format of: 
# ggplot(NameOfMyDataset, aes(x = xAxisVariable, y= yAxisVariable, fill=ColourFactor)) + # remember the PLUS sign!
# geom_boxplot() # or any layer you want can be geom_bar() or geom_line() etc etc


# first,the basic plot 
ggplot(filtered, aes(Treatment, Total_OMD, fill = Treatment)) + # define variables
  geom_boxplot(size=1) 


# oooh much better! But, now I notice my treatments are in a weird/illogical order. 
# I'll re order them how i want by listing them in the right order
filtered$Treatment <- factor(filtered$Treatment, levels = c("CO","1X LARS", "1X UHP", 
                                                             "0.5X LARS"))

# now when we plot it again, our treatments should be in the order we want.
ggplot(filtered, aes(Treatment, Total_OMD, fill = Treatment)) + 
  geom_boxplot() 

# now we'll start making our plot a little prettier looking bit by bit
# normally you'd do this at once, but to make clear what is happening
# we'll do slowly, and add a comment for each line to explain what's happening

# this time we'll assign a name (plot1) to our plot. So, if we want to see them we have to call
# the plot by name (ie by writing name of plot and sending to console)

plot1 = ggplot(filtered, aes(Treatment, Total_OMD, fill = factor(Treatment))) + # define variables
  geom_boxplot(size=0.5) + # by adding size=1 the lines for the boxplot become a bit thicker
  # and easier to see

  theme_bw()  # we'll use a built in theme called theme_bw() to remove grey background

plot1 

# now I'll use the function theme() to change the size and colour of text,
# background etc. There are loads of things you can change under theme. 
# https://ggplot2.tidyverse.org/reference/theme.html

plot2 = plot1 +
  theme(axis.title.x = element_blank(), # remove x axis title
        axis.text.x = element_text(angle = 45, vjust=1,hjust=1), # remove x axis text
        text = element_text(size=12, color = "black"), # increase text size
        axis.text = element_text(color="black"), # change axis labels from default grey to black
        legend.text = element_text(size=12), # increase size of legend text
        strip.text.x = element_text(size = 16, colour = "black"),# change font of facet label
        strip.background =  element_rect(fill = "white"), # remove grey backgroup facet label
        legend.key.size = unit(0.25, "cm") # change legend key size
        
  )

plot2

# we dont need the legend on the side really, as its the same info as on
# x axis labels. So we'll turn off legend with guides(fill=FALSE)
# at the same time, we'll update our x axis labels to be slightly nicer
# we'll call them 'myLabs' -> MAKE SURE these are in the right order or you'll 
# label things incorrectly. 
myLabs = c("CO","1X LARS", "1X UHP", "0.5X LARS")

plot2 + guides(fill=FALSE) + scale_x_discrete(labels= myLabs)

##################################################
### statistical analyses =====

# now we'll make separate datasets for each of the four VFAs
# we'll use subset() to do this. We could also use grep but it's good to know diff ways!

## Running ANOVA ====

# we use the built in R function aov() which takes the format
# aov(numberVariable ~ FactorVariable, data= NameOfDataset)
# we'll save the outtput to resAc.aov
Total_OMD.aov <- aov(Total_OMD ~ Treatment, data = filtered)
# Then check if there any stat sig differences by looking at summary of the analysis
summary(Total_OMD.aov)

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


TukeyHSD(Total_OMD.aov)

library(dplyr)
filtered %>%
  group_by(Treatment) %>%
  summarise_at(vars(Gas_vol_L), list(name = mean))


#mean(filtered$Total_OMD[filtered$Treatment=="Control"])
#mean(filtered$Total_OMD[filtered$Treatment=="0.5X UHP+KI"])
#mean(filtered$Total_OMD[filtered$Treatment=="0.5X UHP"])
#mean(filtered$Total_OMD[filtered$Treatment=="0.25X UHP+KI"])
#mean(filtered$Total_OMD[filtered$Treatment=="0.25X UHP "])
#mean(filtered$Total_OMD[filtered$Treatment=="0.5X MgO2"])

