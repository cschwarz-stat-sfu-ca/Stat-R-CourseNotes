







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
plot(test[,"x"], test[,"y"]) 
abline(coef(lm(y ~ x, data=as.data.frame(test))))
dev.copy(png, file="Images/gendata-test-001.png"); dev.off()


test <- GenData(Ivalue=100, 
                Trend=.02, 
                SampTimes=1:10, 
                SampStdDev=.01)
plot(test[,"x"], test[,"y"]) 
abline(coef(lm(y ~ x, data=as.data.frame(test))))


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

plot(NULL, NULL, type="n", 
     xlim=c(1,10), ylim=c(80,140),
     main='Estimated lines',
     xlab="Time", ylab="Y")
a_ply(myres, 1, function(x){abline(a=x$intercept, b=x$slope)} )
dev.copy(png, file="Images/gendata-lines-001.png"); dev.off()

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

with(powerres, plot(Trend, power, type="b",
      main="Estimated power curve"))
abline(a=.90, b=0)
dev.copy(png, file="Images/gendata-power.png"); dev.off()



