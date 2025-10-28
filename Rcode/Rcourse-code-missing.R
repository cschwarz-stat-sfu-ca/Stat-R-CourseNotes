# ---------------------- missing values ---------------------
# 2015-01-30 CJS Update code file.
# 2014-05-20 CJS First Edition

options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(car)
library(ggplot2)
library(gridExtra)
library(emmeans)
library(plyr)

cereal <- read.csv('../sampledata/cereal.csv', 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal[1:5,]


cereal$weight

mean(cereal$weight)
mean(cereal$weight, na.rm=TRUE)
mean(na.omit(cereal$weight))

length(cereal$weight)  # includes missing values
length(na.omit(cereal$weight))
is.na(cereal$weight)
sum( is.na(cereal$weight)) # count num missing

dim(cereal)
dim(na.omit(cereal)) # drop row with missing data

complete.cases(cereal)
dim(cereal[complete.cases(cereal),])



# Using NA in operations leads to NA's
cereal$prop.fat <- cereal$fat /cereal$weight
cereal$prop.fat
is.na(cereal$prop.fat)

# NA is different from Inf; protein/fat ratio
cereal$protein.fat <- cereal$protein / cereal$fat
cereal$protein.fat
is.na(cereal$protein.fat) # Inf is a value
is.infinite(cereal$protein.fat)


# missing values typically don't show up on plots
ggplot(data=cereal, aes(x=weight, y=calories))+
  ggtitle("Calories vs. Weight per serving")+
  xlab("Weight per serving")+ylab("calories")+
  geom_jitter()+
  geom_smooth(method="lm",se=FALSE)
# This gives a warning message in console


# Missing values make modelling more difficult
# Regression of calories vs serving size


fit.cal.serving <- lm(calories ~ weight, 
                     data=cereal)
summary(fit.cal.serving) # note missingness message in output

fitted(fit.cal.serving) # only length 75 despite having index up to 77
length(fitted(fit.cal.serving))
# The following fails
cereal$fitted <- fitted(fit.cal.serving)


# You can force the lm() model to behave more sensibly.
fit.cal.serving2 <- lm(calories ~ weight,
          na.action=na.exclude,
          data=cereal)
summary(fit.cal.serving2)
fitted(fit.cal.serving2) # now padded with NA
length(fitted(fit.cal.serving2))
cereal$fitted <- fitted(fit.cal.serving2)

# I find using predict to safer and easier to use
predict(fit.cal.serving)
predict(fit.cal.serving, newdata=cereal)

predict(fit.cal.serving2)
