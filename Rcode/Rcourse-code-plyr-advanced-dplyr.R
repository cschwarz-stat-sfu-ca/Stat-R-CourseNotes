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
library(dplyr)

#---------------------------------------------------------------
# summarize + plyr + dplyr
cereal <- read.csv(file.path("..","sampledata",'cereal.csv'), 
              header=TRUE, as.is=TRUE, 
              strip.white=TRUE)
cereal[1:5,]
library(plyr)
sumstats <- plyr::ddply(cereal, "shelf", plyr::summarize,
            ncereal=length(name),
            mean.calories=mean(calories))
sumstats

sumstats <- cereal %>% 
               group_by(shelf) %>%
                 dplyr::summarize(
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

sumstats <- cereal %>%
             group_by(shelf) %>%
              dplyr::summarize(
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
sumstats <- cereal %>%
             group_by(shelf) %>%
              dplyr::summarize(
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

sumstats <- cereal %>%
            group_by(shelf) %>%
             dplyr::summarize(
              intercept=coef(lm(calories ~fat))[1],
              slope    =coef(lm(calories ~fat))[2]
            )
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

sumstats <- cereal %>% group_by(shelf) %>% do(
        (function(x){
         result <- lm(calories ~ fat, data=x) # notice use of x in data= argument   
         intercept <- coef(result)[1]
         slope    <-   coef(result)[2]
         res <- data.frame(intercept, slope, stringsAsFactors=FALSE)
         res
        })(.)
        )
sumstats



#----------------------------------------------------------
# Exercise - do a plot of the accident data with weather as an indicator 

# Read in the accident data and get the date and fatality variables set
accidents <- read.csv(file.path("..","sampledata",'Accidents','road-accidents-2010.csv'), header=TRUE,
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

naccidents <- 
  accidents %>% 
     group_by(mydate) %>% 
       dplyr::summarize(
              freq=length(mydate),
              pfatal=mean(Fatality),
              mean.weather=mean(Weather_Conditions),
              dow=format(mydate, "%w")[1] 
)
head(naccidents)


# Using the general function methods
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

naccidents <- accidents %>% group_by(mydate) %>% do(
           (function(x){
              freq <- nrow(x)
              mean.weather <- mean(x$Weather_Conditions)
              pfatal <- mean(x$Fatality)
              dow=format(x$mydate, "%w")[1]
              res <- data.frame(freq, mean.weather, pfatal, dow, stringsAsFactors=FALSE)
              return(res)
              })(.)
)
naccidents[1:5,]
str(naccidents)


library(ggplot2)
newplot <- ggplot(data=naccidents, 
            aes(x=mydate, y=freq ))+
  ggtitle("Number of accidents by date with mean weather coded")+
  geom_point( aes(size=mean.weather))+
  geom_smooth(method="loess", color="red", se=FALSE)
newplot
ggsave(newplot, file="../../MyStuff/Images/ggplot-accidents-by-date-weather.png",
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
 


#-----------------------------------------
# More advanced features of dplyr() package
# Passing additonal arguments to the function in dplyr routine
# Major problem is non-standard evaluation, i.e. not putting variables in quotes

# For example, this gets the mean of the fat variable
dplyr::summarize(cereal, mean(fat))

# I would like
var <- "fat"
dplyr::summarize(cereal, mean(var))

# Need to use the quo() and !! functions
var <- quo(fat)
var

dplyr::summarize(cereal, mean(!!var))


sumstat <- function(x, var){
  # Compute some summary statistics for a data frame
  #browser()
  values <- x[,var,drop=TRUE]  # extract the variable values and covert to vector
  n <- length(values)
  nmiss <- sum(is.na(values))
  mean.val  <- mean(values, na.rm=TRUE)
  sd.val    <- sd(values, na.rm=TRUE)
  res  <- data.frame(n,nmiss,mean=mean.val,sd=sd.val,
              stringsAsFactors=FALSE)
  res
} # end of sumstat

sumstat(cereal, "calories")
sumstat(cereal, "weight")

cereal %>% group_by(shelf) %>% do( sumstatfun(., var='calories'))
cereal %>% group_by(shelf) %>% do( sumstatfun(., var='fat'))





#----------------------------
# Write a function that finds the mean, se.mean and 95% ci based on normal
# theory

my.simple.summary <- function(df, variable, conflevel=0.95){
  # Return simple statistics on 'variable' in the dataframe 'df'
  # This is commonly used in a ddply() function to get separate statistics for
  # each group.
  #
  data  <- df[,variable, drop=TRUE]
  n     <- length(data)
  nmiss <- sum(is.na(data)) # number of missing values
  mean  <- mean(data, na.rm=TRUE)
  sd    <- sd  (data, na.rm=TRUE)
  # compute the se and confint using a simple linear model
  fit   <- lm(data ~1)
  se    <- sqrt(diag(vcov(fit)))
  ci    <- confint(fit, level=conflevel)
  res <- data.frame(variable=variable,
             n=n,
             nmiss=nmiss,
             mean=mean,
             sd  =sd,
             se  =se,
             conflevel=conflevel,
             lcl=ci[1],
             ucl=ci[2])
  row.names(res) <- NULL
  res
}
my.simple.summary(cereal, "fat")
my.simple.summary(cereal, "fat", conflevel=.90)

cereal %>% group_by(shelf) %>% do(  my.simple.summary(., variable="fat"))
cereal %>% group_by(shelf) %>% do(  my.simple.summary(., variable="fat", conflevel=0.90))





#-----------------------------------------------------
# Using do to create and process lists

sumstat <- function(x, Yvar, Xvar){
   x <- as.data.frame(x) # convert from tibbles.
   # Do the plot (use aes_string)
   plot <- ggplot(data=x, aes_string(x=Xvar, y=Yvar))+
      ggtitle(paste("Scatterplot of ", Yvar, " vs. ", Xvar," for shelf ", x$shelf[1], sep=""))+
      geom_point(position=position_jitter(h=.1, w=.1))+
      geom_smooth(method="lm", se=FALSE)
   #browser()
   fit <- lm( x[,Yvar] ~ x[, Xvar], data=x)
   list(plot=plot, fit=fit)
}

res<- sumstat(cereal, "calories", "fat")
length(res)

res <- plyr::dlply(cereal, "shelf", sumstat, Yvar="calories", Xvar="fat")
length(res)
res[[1]]
res[[2]]
res[[3]]

se.summary <- plyr::ldply(res, function(x){
    # x is now the list of the flot and the fit
    slope <- summary(x$fit)$coefficients[2,]
    slope
})
se.summary

# notice you have to name the returned object if NOT a data frame!
res.b <- cereal %>% group_by(shelf) %>% do( res=sumstat(., Yvar="calories", Xvar="fat"))

# you get a paired set of lists
names(res.b)
length(res.b$res)
names(res.b$res[[1]])
res.b$res[[1]]$fit

# now extract the slopes
report <- res.b %>% do( data.frame(shelf=.$shelf, t(summary(.$res$fit)$coefficients[2,])) )
report
#-----------------------------------------


#  Exercise III to find the ratio of accidents on weekdays to weekends by month

accidents <- read.csv(file.path("..","sampledata","Accidents",'road-accidents-2010.csv'), 
             header=TRUE,
             as.is=TRUE, strip.white=TRUE)
accidents$mydate <- as.Date(accidents$Date, format="%d/%m/%Y")
accidents$month <- as.numeric(format(accidents$mydate, "%m"))

# get the month for each accident date
accidents[1:5, c("mydate","month")]

mysummary <- function(accidents){
# Compute the number of weekend and weekdays in the month (excluding holidays)
# Compute the number of accidents on weekend/weekdays
# Report the two ratio.
   DaysOfMonth <- unique(accidents$mydate)
   DaysOfWeeks <- format(DaysOfMonth, "%w")
   nDays     <- length(DaysOfMonth)
   nWeekdays <- sum(DaysOfWeeks %in% 1:5)
   nWeekends <- sum(DaysOfWeeks %in% c(0,6))
   
   AccDaysOfWeek <- format(accidents$mydate, "%w")
   nAccTotal    <- length(accidents$mydate)
   nAccWeekdays <- sum(AccDaysOfWeek %in% 1:5)
   nAccWeekends <- sum(AccDaysOfWeek %in% c(0,6))
   rAccWdWe <- nAccWeekdays/nAccWeekends
   rDaysWdWe <- nWeekdays/nWeekends
 
   res <- data.frame(nDays,
            nWeekdays,
            nWeekends,
            nAccTotal,
            nAccWeekdays,
            nAccWeekends,
            rDaysWdWe,
            rAccWdWe, stringsAsFactors=FALSE)
   return(res)
}

testdata <- subset(accidents, accidents$month == 1)
dim(testdata)

mysummary(testdata)

# using plyr
results <- plyr::ddply(accidents, "month", mysummary)
results

# using dplyr
results <- accidents %>% group_by(month) %>% do( mysummary(.) )
results

newplot <- ggplot(data=results, aes(x=month, y=rDaysWdWe))+
  ggtitle("Comparing accidents on weekends/weekdays")+
  xlab("Month")+ylab("Ratio Weekends/Weekdays")+
  geom_line(group=1, color="blue", linetype=2)+
  geom_line(aes(y=rAccWdWe,group=1))
newplot
ggsave(newplot, 
  file=file.path("..","..","MyStuff","Images","accidents-ratio-wd-we-001.png"),
  h=4, w=6, units="in", dpi=300)

newplot <- ggplot(data=results, aes(x=month, y=rAccWdWe/rDaysWdWe))+
  ggtitle("Comparing accidents on weekends/weekdays")+
  xlab("Month")+ylab("Ratio Weekends/Weekdays")+
  geom_line(group=1, color="blue", linetype=2)
newplot
ggsave(newplot, 
  file=file.path("..","..","MyStuff","Images","accidents-ratio-wd-we-002.png"),
  h=4, w=6, units="in", dpi=300)
