# Simulation study
# 2014-05-20 CJS First edition
options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)
library(gridExtra)
library(plyr)


#----------------------------------------------------------
# Simulation Study of a regression model with NO process error
# Process error often exists in regression situations and so this
# power function should be used cautiously. Contact cschwarz@stat.sfu.ca for details.
#
GenData <- function(Ivalue, Trend, SampTimes, SampStdDev){
# Generate data for a power analysis of a trend
# Ivalue - initial value of trend (in year 1)
# Trend  - size of trend  (fractional change/year)
# SamplTimes - vector of when samples take place
# SampStdDev - Sampling standard deviation (fraction of mean)
  
# Generate mean response
   mu <- Ivalue * (1+Trend)**(SampTimes-1)
   Serror <- rnorm(length(mu), mean=0, sd=SampStdDev*mu)
   basey <- mu + Serror
   return(cbind(x=SampTimes, y=basey))
}
      
set.seed(234234) 
test <- GenData(Ivalue=100, 
                Trend=.02, 
                SampTimes=1:10, 
                SampStdDev=.20)
test.fit <- lm(y ~ x, data=as.data.frame(test))

ggplot(data=as.data.frame(test), aes(x=x, y=y))+
  ggtitle("Sample of data for the simulation")+
  geom_point()+
  geom_abline(intercept=coef(test.fit)[1], slope=coef(test.fit)[2])
ggsave(file="Images/gendata-test-001.png")


test <- GenData(Ivalue=100, 
                Trend=.02, 
                SampTimes=1:10, 
                SampStdDev=.01)
ggplot(data=as.data.frame(test), aes(x=x, y=y))+
  ggtitle("Sample of data for the simulation")+
  geom_point()+
  geom_abline(intercept=coef(test.fit)[1], slope=coef(test.fit)[2])


# Making the wrapper for GenData
do.one.sim <- function(Ivalue, Trend, 
            SampTimes,
            SampStdDev,
            alpha=0.05) # default value
{
# generate the test data, average over pseudo-replicates,
# fit the line, extract
# the slope, se, and p-value
# and see if we detected anything?
  mydata <- GenData(Ivalue=Ivalue, 
                Trend=Trend, 
                SampTimes=SampTimes, 
                SampStdDev=SampStdDev)
  myfit <- lm(y ~ x, data=as.data.frame(mydata))
  mycoef <- coef(myfit)
  my.fit.summary <- summary(myfit)
  myslope.se <- my.fit.summary$coefficients[2,2]
  myslope.pvalue <- my.fit.summary$coefficients[2,4]
  detect <- myslope.pvalue < alpha
  myres <- c(mycoef, myslope.se,
         myslope.pvalue,
         detect)
  names(myres)<- c("intercept","slope","se","pvalue","detect")
#  browser()
  return(myres)
}


test <- do.one.sim(Ivalue=100, 
                Trend=.02, 
                SampTimes=1:10, 
                SampStdDev=.20)
test


# Finally do the simlation 1000 times
myres <- rdply(1000, do.one.sim(Ivalue=100, 
                Trend=.02, 
                SampTimes=1:10, 
                SampStdDev=.20))
myres[1:5,]
cat("the power is ", mean(myres$detect),"\n")

ggplot(data=myres)+
  ggtitle("Plot of the simulated lines")+
  xlim(c(1,10))+ylim(c(80,140))+
  xlab("Time")+ylab("y")+
  geom_abline(data=myres, aes(intercept=intercept, slope=slope))
ggsave( file="Images/gendata-lines-001.png")

#  Put the power funtion in a separate wrapper
mypower <- function(Ivalue, Trend, 
            SampTimes, 
            SampStdDev,
            alpha=0.05,
            nsims=1000) # default value
{  #browser()
   myres <- rdply(nsims, do.one.sim(Ivalue=Ivalue, 
                Trend=Trend, 
                SampTimes=SampTimes, 
                SampStdDev=SampStdDev))
   power <- mean(myres$detect)
   names(power)<- "power"
   return(power)
}

mypower(Ivalue=100, 
       Trend=.02, 
       SampTimes=1:10,  
       SampStdDev=.20)

# Finally, try different levels of trend
# This makes use of the ... arguments in functions - give me a call.
powerres <- ddply(data.frame(Trend=seq(0,.20,.02)), "Trend", 
   function(df, Ivalue, SampTimes, SampStdDev){
   mypower(Ivalue=Ivalue, 
       Trend=df$Trend, 
       SampTimes=SampTimes,  
       SampStdDev=SampStdDev)   
}, Ivalue=100, SampTimes=1:10, SampStdDev=.20)

ggplot(data=powerres, aes(x=Trend, y=power))+
  ggtitle("Estimated power curve")+
  xlab("Trend")+ylab("Power")+
  geom_point()+
  geom_line(group=1)+
  geom_hline(yintercept=0.90)
ggsave( file="Images/gendata-power.png")


