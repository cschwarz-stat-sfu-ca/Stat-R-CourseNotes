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

# Get the data sets ready
cereal <- read.csv(file.path("..","sampledata",'cereal.csv'), 
              header=TRUE, as.is=TRUE, 
              strip.white=TRUE)
cereal[1:5,]

accidents <- read.csv(file.path("..","sampledata",'road-accidents-2010.csv'), header=TRUE,
             as.is=TRUE, strip.white=TRUE)
# Convert date to internal date format
accidents$mydate <- as.Date(accidents$Date, format="%d/%m/%Y")
# Create the fatality variable
accidents$Fatality <- accidents$Accident_Severity == 1


#--------------------------------------------------------
# Passing a function to ddply either pre-defined or inline

mysummary <- function(x){
   # compute the mean fat and calories and their ratio
   mean.fat = mean(x$fat, na.rm=TRUE)
   mean.calories=mean(x$calories, na.rm=TRUE)
   ratio = mean.calories / mean.fat
   data.frame(mean.fat, mean.calories, ratio, stringsAsFactors=FALSE)
}
mysummary(cereal)

# now for each shelf
report <- plyr::ddply(cereal, "shelf", mysummary)
report


# Pass the function as an anonymous function;
report <- plyr::ddply(cereal, "shelf", function(x){
   # compute the mean fat and calories and their ratio
   mean.fat = mean(x$fat, na.rm=TRUE)
   mean.calories=mean(x$calories, na.rm=TRUE)
   ratio = mean.calories / mean.fat
   data.frame(mean.fat, mean.calories, ratio, stringsAsFactors=FALSE)
})
report

#-----------------------------------------
# Exercise 
sumstats <- plyr::ddply(cereal, "shelf", function(x) {
        result <- lm(calories ~ fat, data=x)   # notice use of x 
        intercept <- coef(result)[1]
        slope    <-   coef(result)[2]
        rmse      <- summary(result)$sigma
        res <- data.frame(intercept, slope, rmse,
                  stringsAsFactors=FALSE)
        return(res)
      })
sumstats


#-----------------------------------------
# Exercise 
naccidents <- plyr::ddply(accidents, "mydate", plyr::summarize,
              freq=length(mydate),
              pfatal=mean(Fatality),
              mean.weather=mean(Weather_Conditions),
              dow=format(mydate, "%w")[1])
naccidents[1:5,]


naccidents <- plyr::ddply(accidents, "mydate", function(x){
              freq <- nrow(x)
              mean.weather <- mean(x$Weather_Conditions)
              pfatal <- mean(x$Fatality)
              dow=format(x$mydate, "%w")[1]
              res <- data.frame(freq, mean.weather, pfatal,
                       dow, stringsAsFactors=FALSE)
              return(res)
              })
naccidents[1:5,]

newplot <- ggplot(data=naccidents, 
            aes(x=mydate, y=freq ))+
  ggtitle("Number of accidents by date with mean weather coded")+
  geom_point( aes(size=mean.weather))+
  geom_smooth(method="loess", color="red", se=FALSE)
newplot

newplot <- ggplot(data=naccidents,
                 aes(x=mean.weather, y=freq))+
  geom_point( ) + 
  geom_smooth(method="loess", color="red", se=FALSE) +
  ggtitle("Number of accidents by mean weather coded")
newplot


newplot <- ggplot(data=naccidents, aes(x=dow, y=freq))+
  geom_boxplot( ) +
  geom_jitter(aes(size=pfatal, color=mean.weather), 
           position=position_jitter(w=.3, h=.0))+
  ggtitle("Number of accident by day of the week")+
  xlab("Day of the week. 0=Sunday") +
  ylab("Number of accidents in a day")
newplot




#-----------------------------------------
# More advanced features of plyr() package
# Passing additonal arguments to the function in plyr routine


sumstat <- function(x, var){
  # Compute some summary statistics for a data frame
  values <- x[,var]  # extract the variable values
  n <- length(values)
  nmiss <- sum(is.na(values))
  mean  <- mean(values, na.rm=TRUE)
  sd    <- sd(values, na.rm=TRUE)
  res  <- data.frame(n,nmiss,mean,sd,
              stringsAsFactors=FALSE)
  return(res)
} # end of sumstat

sumstat(cereal, "calories")
sumstat(cereal, "weight")

plyr::ddply(cereal, "shelf", sumstat, var="calories")
plyr::ddply(cereal, "shelf", sumstat, var="weight")


#----------------------------
# Write a function that finds the mean, se.mean and 95% ci based on normal
# theory

my.simple.summary <- function(df, variable, conflevel=0.95){
  # Return simple statistics on 'variable' in the dataframe 'df'
  # This is commonly used in a ddply() function to get separate statistics for
  # each group.
  #
  data  <- df[,variable]
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

plyr::ddply(cereal, "shelf", my.simple.summary, variable="fat")
plyr::ddply(cereal, "shelf", my.simple.summary, variable="fat", conflevel=0.90)

plyr::ddply(cereal, "shelf", my.simple.summary, variable="weight")
#-----------------------------------------------------
# dlply() and ldply()

sumstat <- function(x, Yvar, Xvar){
   # Do the plot (use aes_string)
   plot <- ggplot(data=x, aes_string(x=Xvar, y=Yvar))+
      ggtitle(paste("Scatterplot of ", Yvar, " vs. ", Xvar," for shelf ", x$shelf[1], sep=""))+
      geom_point(position=position_jitter(h=.1, w=.1))+
      geom_smooth(method="lm", se=FALSE)
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

# display all of the plots
plyr::l_ply(res, function(x){
   plot(x$plot) # needed because within a function
})


# Extract the plots and put into a single file
pdf('summary.pdf', h=4, w=6 )
plyr::l_ply(res, function(x){
   plot(x$plot) # needed because within a function
})

dev.off()


# Extract residuals and plot for each shelf using faceting
resid <- ldply(res, function(x){
    data.frame(resid= resid(x$fit), fitted=fitted(x$fit))
})
head(resid)
resid.plot <- ggplot(data=resid, aes(x=fitted, y=resid))+
  ggtitle("Residual plots")+
  geom_point()+
  geom_hline(yintercept=0)+
  facet_wrap(~shelf, ncol=2, scales="free")
resid.plot
#ggsave(resid.plot,
#       file=file.path("..","..","MyStuff","Images","ldply-001.png"), h=6, w=6, units="in", dpi=300)

#-----------------------------------------


#  Exercise III to find the ratio of accidents on weekdays to weekends by month

accidents <- read.csv('../sampledata/road-accidents-2010.csv', 
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

results <- plyr::ddply(accidents, "month", mysummary)
results

newplot <- ggplot(data=results, aes(x=month, y=rDaysWdWe))+
  ggtitle("Comparing accidents on weekends/weekdays")+
  xlab("Month")+ylab("Ratio Weekends/Weekdays")+
  geom_line(group=1, color="blue", linetype=2)+
  geom_line(aes(y=rAccWdWe,group=1))
newplot
ggsave(newplot, file="../../MyStuff/Images/accidents-ratio-wd-we-001.png",
  h=4, w=6, units="in", dpi=300)

newplot <- ggplot(data=results, aes(x=month, y=rAccWdWe/rDaysWdWe))+
  ggtitle("Comparing accidents on weekends/weekdays")+
  xlab("Month")+ylab("Ratio Weekends/Weekdays")+
  geom_line(group=1, color="blue", linetype=2)
newplot
ggsave(newplot, file="../../MyStuff/Images/accidents-ratio-wd-we-002.png",
  h=4, w=6, units="in", dpi=300)


#-----------------------------------------------
# running in parallel

doParallel <- TRUE  # should I set up parallel processing 

if(doParallel) {
  library(doMC)  # for parallel model fitting
  # see http://viktoriawagner.weebly.com/blog/five-steps-to-parallel-computing-in-r
  detectCores() 
  cl <- makeCluster(4)
  # Need to export some libraries to the cluster
  # see http://stackoverflow.com/questions/18981932/logging-with-plyr-in-parallel-true
  clusterEvalQ(cl, library(unmarked))
  registerDoMC(5) 
} 

Sys.time()
res <- plyr::ddply(cereal, "shelf", plyr::summarize,
                   mean=sum(log(shelf[1]*(1:10000000)), na.rm=TRUE),
                   .parallel=FALSE)
Sys.time()

Sys.time()
res <- plyr::ddply(cereal, "shelf", plyr::summarize,
                   mean=prod(shelf[1]*(1:100000000), na.rm=TRUE),
                   .parallel=doParallel)
Sys.time()

if(doParallel) stopCluster(cl) # stop parallel processing


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

