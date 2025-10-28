#------------ Split - Apply - Combine
# 2015-01-31 CJS Update to only use plyr rather than Base R function
# 2014-05-20 CJS First Edition

options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(car)
library(ggplot2)
library(gridExtra)
library(emmeans)
library(plyr)


#---------------------------------------------------------------
# summarize + plyr
cereal <- read.csv(
     file.path('..','sampledata','cereal.csv'), 
     header=TRUE, as.is=TRUE, 
     strip.white=TRUE)
cereal[1:5,]
library(plyr)
sumstats <- plyr::ddply(cereal, "shelf", plyr::summarize,
            ncereal=length(name),
            mean.calories=mean(calories))
sumstats


# Find some other statistics
sumstats <- plyr::ddply(cereal, "shelf", plyr::summarize,
            std.calores=sd(calories),
            mean.fcal = mean(fat*9),
            mean.pcal.fat = mean( fat*9 / calories),
            mean.wt=mean(weight))
sumstats


# Find some other statistics - account for missing values
sumstats <- plyr::ddply(cereal, "shelf", plyr::summarize,
            std.calores=sd(calories),
            mean.fcal = mean(fat*9),
            mean.pcal.fat = mean( fat*9 / calories),
            mean.wt=mean(weight, na.rm=TRUE))
sumstats


# Fit a separate line between calories and fat and get the coefficients
# We need to extract one value at a time
sumstats <- plyr::ddply(cereal, "shelf", plyr::summarize,
            intercept=coef(lm(calories ~fat))[1],
            slope    =coef(lm(calories ~fat))[2])
sumstats

# Here is a more advance way to do this using the general
# method
library(plyr)
sumstats <- plyr::ddply(cereal, "shelf", function(x)
      {
        result <- lm(calories ~ fat, data=x) # notice use of x in data= argument   
        intercept <- coef(result)[1]
        slope    <-   coef(result)[2]
        res <- data.frame(intercept, slope, stringsAsFactors=FALSE)
        return(res)
      })
sumstats



#----------------------------------------------------------
# Exercise - do a plot of the accident data with weather as an indicator 

# Read in the accident data and get the date and fatality variables set
accidents <- read.csv(
   file.path('..','sampledata','Accidents','road-accidents-2010.csv'), 
   header=TRUE,
   as.is=TRUE, strip.white=TRUE)
# Convert date to internal date format
accidents$mydate <- as.Date(accidents$Date, format="%d/%m/%Y")
# Create the fatality variable
accidents$Fatality <- accidents$Accident_Severity == 1
accidents[1:5,]

# Get summary data for each data using the summarize method
naccidents <- plyr::ddply(accidents, "mydate", plyr::summarize,
              freq=length(mydate),
              pfatal=mean(Fatality),
              mean.weather=mean(Weather_Conditions),
              dow=format(mydate, "%w")[1])
naccidents[1:5,]
str(naccidents)

# Using the general functionmethods
naccidents <- plyr::ddply(accidents, "mydate", function(x){
              freq <- nrow(x)
              mean.weather <- mean(x$Weather_Conditions)
              pfatal <- mean(x$Fatality)
              dow=format(x$mydate, "%w")[1]
              res <- data.frame(freq, mean.weather, pfatal, dow, stringsAsFactors=FALSE)
              return(res)
              })
naccidents[1:5,]
str(naccidents)


library(ggplot2)
newplot <- ggplot(data=naccidents, 
            aes(x=mydate, y=freq ))+
  ggtitle("Number of accidents by date with mean weather coded")+
  geom_point( aes(size=mean.weather))+
  geom_smooth(method="loess", color="red", se=FALSE)
newplot
ggsave(newplot, 
   file=file.path("..","..","MyStuff","Images","ggplot-accidents-by-date-weather.png"),
   h=4,w=6, units="in", dpi=300)

# plot the number of accidents by the mean weather
newplot <- ggplot(data=naccidents, aes(x=mean.weather, y=freq))+
  geom_point( ) + 
  geom_smooth(method="loess", color="red", se=FALSE) +
  ggtitle("Number of accidents by mean weather coded")
newplot
ggsave(newplot, 
    file=file.path("..","..","MyStuff","Images","ggplot-accidents-by-mean-weather.png"),
    h=4, w=6, units="in", dpi=300)


# Summarize number of accidents by day of the week
# using color and symbol size to code fatality rates and weather
# conditions
newplot <- ggplot(data=naccidents, aes(x=dow, y=freq))+
  geom_boxplot(outlier.size=0) +
  geom_jitter(aes(size=pfatal, color=mean.weather), position=position_jitter(w=.3, h=.0))+
  ggtitle("Number of accident by day of the week")+
  xlab("Day of the week. 0=Sunday") +
  ylab("Number of accidents in a day")
newplot
ggsave(newplot, 
    file=file.path("..","..","MyStuff","Images","ggplot-accidents-by-dow.png"),
    h=4, w=6, units='in', dpi=300)
 


#--------------------------------------------------------
# More advanced features of plyr() package
# Row/column operations

mat <- matrix(1:30, nrow=6)
mat

# Now some operations on rows or columns
aaply(mat, 1, sum)
aaply(mat, 2, mean)
aaply(mat, 1, function(x){
  res <- prod(sin(x))
  return(res)
})



