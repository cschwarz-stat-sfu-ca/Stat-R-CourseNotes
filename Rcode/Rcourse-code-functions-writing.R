# Writing Functions.
# 2015-02-01 CJS split into separate section
# 2014-04-20 CJS First Edition
options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(boot)
library(car)
library(ggplot2)
library(gridExtra)
library(plyr)

cereal <- read.csv(file.path('..','sampledata','cereal.csv'), 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal[1:5,]

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

my.cv(cereal$calories)
my.cv(cereal$weight)


# Adding complexity. Remove the NA before computaton
my.cv <- function( x ,remove.na=FALSE){
# Compute the coefficient of variation
# after removing  na's
    my.mean <- mean(x, na.rm=remove.na)
    my.sd   <- sd(x,   na.rm=remove.na)
    cv   <- my.sd / my.mean
    names(cv) <- 'cv'
    return(cv)
} # end of my.cv
my.cv(cereal$calories)
my.cv(cereal$weight)
my.cv(cereal$weight, remove.na=TRUE)


# Adding complexity. Remove the NA before computaton
# Understanding na.rm=na.rm
my.cv <- function( x ,na.rm=FALSE){
# Compute the coefficient of variation
# after removing  na's
    my.mean <- mean(x, na.rm=na.rm)
    my.sd   <- sd(x,   na.rm=na.rm)
    cv   <- my.sd / my.mean
    names(cv) <- 'cv'
    return(cv)
} # end of my.cv
my.cv(cereal$calories)
my.cv(cereal$weight)
my.cv(cereal$weight, na.rm=TRUE)



# Adding complexity. More arguments to the function
my.cv <- function( x , chop=0, na.rm=FALSE){
# Compute the coefficient of variation
# after removing a fraction 'chop' from top and bottom and
# dealing with na's
    newx <- x[ x >= quantile(x, probs=chop, na.rm=TRUE) &
               x <= quantile(x, probs=1-chop,na.rm=TRUE)]
    my.mean <- mean(newx, na.rm=na.rm)
    my.sd   <- sd(newx, na.rm=na.rm)
    cv   <- my.sd / my.mean
    names(cv) <- 'cv'
    return(cv)
} # end of my.cv


my.cv(cereal$calories)
my.cv(cereal$weight)
my.cv(cereal$weight, na.rm=TRUE)
my.cv(cereal$weight, na.rm=TRUE, chop=0.10)





#------------------------------------------------------
#  Functions - dealing with missing values a simple summary

# Function to make a nice report
my.summary <- function(x){
# Compute the number, number missing, mean, sd, cv 
# with all the NA removed
   nobs <- length(x)
   nmiss <- sum(is.na(x))
   mean <- mean(x, na.rm=TRUE)
   sd   <- sd(x, na.rm=TRUE)
   cv   <- sd/mean
   res  <- data.frame(nobs,nmiss,mean,sd,cv, stringsAsFactors=FALSE)
   return(res)
}# end of my.summary

my.summary(cereal$calories)
my.summary(cereal$weight)


# ------------------------------- Exercise II -------------------------
my.line <- function(df){
  # do a regression of Calories vs Fat and return
  # the slope, its se, and the 95% ci on the slope
  fit <- lm(calories ~ fat, data=df)
  slope <- coef(fit)[2]
  slope.se <- sqrt(diag(vcov(fit)))[2]
  slope.ci <- confint(fit)[2,]
  lcl <- slope.ci[1]
  ucl <- slope.ci[2]
  res <- data.frame(slope, slope.se, lcl, ucl, stringsAsFactors=FALSE)
  return(res)
} # end my.line

my.line(cereal)




# ----- Exercise III 
# Add the X and Y variable to the call
my.line2 <- function(df, Xname, Yname){
  # do a regression of Y vs X and return
  # the slope, its se, and the 95% ci on the slope
  fit <- lm(df[,Yname] ~ df[,Xname], data=df)
  slope <- coef(fit)[2]
  slope.se <- sqrt(diag(vcov(fit)))[2]
  slope.ci <- confint(fit)[2,]
  lcl <- slope.ci[1]
  ucl <- slope.ci[2]
  res <- data.frame(X=Xname, Y=Yname, slope, slope.se, lcl, ucl)
  return(res)
} # end my.line2

my.line2(cereal, Xname="fat",     Yname="calories")
my.line2(cereal, Xname="protein", Yname="calories")


# ----- Exercise IV 
# Add the X and Y variable to the call
# Return a list with the plot, the fit object, and the summary table
my.line3 <- function(df, Xname, Yname){
  # Get the plot
  plot <- ggplot(data=df, aes_string(x=Xname, y=Yname))+
     ggtitle(paste("Plot of ", Yname," vs ", Xname, sep=""))+
     geom_point( position=position_jitter(h=.1, w=.1))+
     geom_smooth(method="lm", se=FALSE)
  
  # do a regression of Y vs X and return
  # the slope, its se, and the 95% ci on the slope
  fit <- lm(df[,Yname] ~ df[,Xname], data=df)
  slope <- coef(fit)[2]
  slope.se <- sqrt(diag(vcov(fit)))[2]
  slope.ci <- confint(fit)[2,]
  lcl <- slope.ci[1]
  ucl <- slope.ci[2]
  res <- data.frame(X=Xname, Y=Yname, slope, slope.se, lcl, ucl)
  
  return(list(plot=plot, fit=fit, res=res))
} # end my.line2

my.line3(cereal, Xname="fat",     Yname="calories")
my.line3(cereal, Xname="protein", Yname="calories")








# ---- ------------------------------------------------------------------
# Passing functions as arguments
# Separate fits for each shelf
ddply(cereal, "shelf", my.line)

ddply(cereal, "shelf", my.line2, Xname="fat", Yname="calories")

results <- dlply(cereal, "shelf", my.line3, Xname="fat", Yname="calories")
names(results)
results[[1]]




#-----------------------------------------------------------------------
# Bootstrapping Example
ratio.meanY.meanX <- function(df, ind, Y,X, na.rm=FALSE){
#  Compute the ratio of the mean of Y to mean of X potentially removing missing values
   res <- mean(df[ind,X],na.rm=na.rm)/
          mean(df[ind,Y],na.rm=na.rm)
   names(res) <- "ratio"
   return(res)
}

ratio.meanY.meanX(df=cereal,1:nrow(cereal), X="fat",Y="calories", na.rm=TRUE )

library(boot)
bootres <- boot(cereal, ratio.meanY.meanX, R=100,
            X="fat",Y="calories", na.rm=TRUE )
bootres
str(bootres)
bootres$t0
bootres$t
quantile(bootres$t, prob=c(0.25, .975))


# Bootstrapping Exercise
r2YX <- function(df, ind, Y,X){
#  Compute the regression of Y on X and then find R2
   fit <- lm( df[ind,Y] ~ df[ind, X]) 
   res <- summary(fit)$r.squared
   names(res) <- "R2"
   return(res)
}  #r2YX

# Test out on full data
fit <- lm( calories ~ fat, data=cereal)
summary(fit)$r.squared

r2YX(cereal, 1:nrow(cereal), Y="calories", X="fat")

bootres <- boot(cereal, r2YX, R=100,
            X="fat",Y="calories" )
bootres
str(bootres)
bootres$t0
bootres$t[1:10]
quantile(bootres$t, prob=c(0.25, .975))

