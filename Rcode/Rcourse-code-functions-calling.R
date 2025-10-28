# Calling R Functions.
# 2015-02-01 CJS Split into separate section
# 2014-04-20 CJS First Edition
options(useFancyQuotes=FALSE) # renders summary output corrects
source("schwarz.functions.r")

library(boot)
library(car)
library(ggplot2)
library(gridExtra)
library(emmeans)
library(plyr)

#-------------------------------------------------------------
cereal <- read.csv(file.path('..','sampledata','cereal.csv'), 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal[1:5,]


mean.calories <- mean(cereal$calories)
mean.calories

result.lm <- lm( calories ~ fat, data=cereal)
result.lm

plot1 <- ggplot(data=cereal, aes(y=calories, x=fat))+geom_point()
plot1




#-------------------------------------------------------------
# Create a simple function to compute the cv
#
my.cv <- function( x ){
# Compute the coefficient of variation
    my.mean <- mean(x)
    my.sd      <- sd(x)
    cv   <- my.sd / my.mean
    names(cv) <- "cv"
    return(cv)
} # end of my.cv

my.cv

my.data <- c(1:10)
my.data
my.cv(my.data)
my.cv(x=my.data)
# my.cv(y=my.data) doesn't run because of argument mismatch


# Finding out information about functions
help(mean)
??mean
# google is your friend

# Argument matching rules
mean(cereal$calories)
mean(cereal$calories, trim=.10)
mean(cereal$weight)
mean(cereal$weight, na.rm=TRUE)
mean(cereal$weight, na.rm=TRUE, trim=0.10)
mean(cereal$weight, na.rm=TRUE, trim=5/length(cereal$weight))

mean(na.rm=TRUE, x=cereal$weight, .10)  # AVOID
mean(na.rm=TRUE, x=cereal$weight, trim=.10, blahblah=3)  # AVOID
