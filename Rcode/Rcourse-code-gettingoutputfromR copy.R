# Getting output from R to the real world
# 2015-01-31 CJS Updates
# 2014-05-20 CJS First edition

library(ggplot2)
library(gridExtra)
library(plyr)

#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
# Fun with plotting

cereal <- read.csv('../../sampledata/cereal.csv', 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal[1:5,]



# Saving graphic output
myplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
    geom_point()
myplot
ggsave(myplot, file='../../MyStuff/Images/sample-ggplot-myggplot.png',
       h=4, w=6, units="in", dpi=300) 
 


# Saving text output
sink('../../MyStuff//Images/sample-textoutput.txt', split=TRUE)
  fit <- lm(calories ~ fat, data=cereal)
  anova(fit)
  summary(fit)
  confint(fit)
sink()
