# Discourse on lm() and glm() models in CRD/SRS design
# 2021-09-27 CJS emmeans::CLD to multcomp::cld
# 2015-02-01 CJS Updates
# 2014-05-19 CJS First Editions

options(useFancyQuotes=FALSE) # renders summary output corrects
source("schwarz.functions.r")
#source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(car)
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(emmeans)
library(multcomp)
library(plyr)
library(readxl)

cereal <- read.csv(file.path('..',"sampledata",'cereal.csv'), 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal[1:5,]


# start a plot. ggplot ONLY uses data frames.
newplot <- ggplot(cereal, aes(x=fat, y=calories))+
   ggtitle("Calories vs Grams of Fat")+
   xlab("Fat/serving (g)")+ylab("Calories/serving")+
   geom_point()
newplot

#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------

#--------------- REGRESSION ----------------------------------
# Fit the regression line
cereal.fit <- lm( calories ~ fat, data=cereal)
str(cereal.fit)
names(cereal.fit)

# The standard methods to extract coefficients etc
summary(cereal.fit)
anova(cereal.fit) # Caution Type I only
coef(cereal.fit)
sqrt(diag(vcov(cereal.fit))) # SE of coefficients
confint(cereal.fit)
summary(cereal.fit)$r.squared
summary(cereal.fit)$sigma

methods(class=class(cereal.fit))  # shows methods available


# CAUTION - lm() gives Type I (incremental) tests - you want 
#    Type III (marginal) tests
# Compare the following cereal.fits
cereal.fit <- lm(calories ~ fat, data=cereal)
anova(cereal.fit)
summary(cereal.fit)

cereal.fit2 <- lm(calories ~ fat + protein, data=cereal)
anova(cereal.fit2)
summary(cereal.fit2)

cereal.fit3 <- lm(calories ~ protein + fat, data=cereal)
anova(cereal.fit3)
summary(cereal.fit3)

# Finding the type III SS for a model with CONTINUOUS variables
car::Anova(cereal.fit2, type=3)
car::Anova(cereal.fit3, type=3)


# Standard Diagnostic plots

library(ggfortify)
plot1 <- autoplot(cereal.fit) # ggplot
plot1
ggsave(#plot1,  # see https://github.com/sinhrks/ggfortify/issues/98
       file="../../MyStuff/Images/cereal-fit-lm-diagplot.png",
       h=4, w=6, unit="in", dpi=300)

library(car)
residualPlots(cereal.fit)


# Obtaining predictions
# First set up the points where you want predictions
newfat <- data.frame(fat=seq(0,6,.1))
newfat[1:5,]
str(newfat)


# Predict the AVERAGE number of calories/serving at each fat level
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(cereal.fit, newdata=newfat, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newfat, predict.avg$fit, se=predict.avg$se.fit)
tail(predict.avg.df)

# Add the fitted line and confidence intervals to the plot
plotfit.avgci <- newplot +
    geom_line( data=predict.avg.df, aes(x=fat, y=fit))+
    geom_ribbon(data=predict.avg.df, 
                aes(x=fat,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci

ggsave(plot=plotfit.avgci, file='../../MyStuff/Images/lm-predict-cereal-001.png',
       h=4, w=6, units="in",dpi=300)


# Predict the INDIVIDUAL number of calories in each cereal
# R does not product the se for individual predictions
predict.indiv <- predict(cereal.fit, newdata=newfat, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newfat, predict.indiv)
tail(predict.indiv.df)

# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df,
                aes(x=fat,y=NULL, ymin=lwr, ymax=upr),
                alpha=0.1)
plotfit.indivci

ggsave(plot=plotfit.indivci, file='../../MyStuff/Images/lm-predict-cereal-002.png',
       h=4, w=6, units="in", dpi=300)





# Exercise with Birds and Butts
# Relationship between parasite load and number of butts in nest

library(readxl)
butts <- readxl::read_excel('../sampledata/bird-butts-data.xlsx', 
                  sheet='Correlational', 
                  col_names = TRUE,
                  trim_ws=TRUE,
                  skip=1,
                  .name_repair='universal')
butts[1:5,]

# Fix the names
names(butts)[ grepl('wieght', names(butts))] <- "Butts.weight"
butts[1:5,]

# Look like we need a log-transform on the number of mites
butts$log.mites <- log(butts$Number.of.mites)
outlier <- butts[butts$log.mites<= 0,]
outlier

prelimplotlog <- ggplot(data=butts, aes(x=Butts.weight, y=log.mites))+
     ggtitle("Number of mites vs. butts weight")+
     xlab("Butts weight (g)")+ylab("log(Number of mites)")+
     geom_point()
prelimplotlog

fit.log.butts <- lm(log.mites ~ Butts.weight, data=butts)
anova(fit.log.butts)
summary(fit.log.butts)

sf.autoplot.lm(fit.log.butts) # ggplot


newbutts <- data.frame(Butts.weight=seq(min(butts$Butts.weight,na.rm=TRUE),
                              max(butts$Butts.weight,na.rm=TRUE),.1))
newbutts[1:5,]
str(newbutts)


# Predict the AVERAGE number of calories/serving at each fat level
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(fit.log.butts, newdata=newbutts, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newbutts, predict.avg$fit, se=predict.avg$se.fit)
tail(predict.avg.df)

# Add the fitted line and confidence intervals to the plot
plotfit.avgci <- prelimplotlog +
    geom_line( data=predict.avg.df, aes(x=Butts.weight, y=fit))+
    geom_ribbon(data=predict.avg.df, 
                aes(x=Butts.weight,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci

ggsave(plot=plotfit.avgci, file='../../MyStuff/Images/lm-predict-nests-001.png',
       h=4, w=6, units="in", dpi=300)


# Predict the INDIVIDUAL number of calories in each cereal
# R does not product the se for individual predictions
predict.indiv <- predict(fit.log.butts, newdata=newbutts, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newbutts, predict.indiv)
tail(predict.indiv.df)

# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df,
                aes(x=Butts.weight,y=NULL, ymin=lwr, ymax=upr),
                alpha=0.1)
plotfit.indivci

ggsave(plot=plotfit.indivci, file='../../MyStuff/Images/lm-predict-nests-002.png',
       h=4, w=6, units="in", dpi=300)


# Plot on the regular scale by taking anti-logs
prelimplot <- ggplot(data=butts, aes(x=Butts.weight, y=Number.of.mites))+
     ggtitle("Number of mites vs. butts weight")+
     xlab("Butts weight (g)")+ylab("Number of mites")+
     geom_point()
prelimplot
plotfit.avgci <- prelimplot +
    geom_line( data=predict.avg.df, aes(x=Butts.weight, y=exp(fit)))+
    geom_ribbon(data=predict.avg.df, 
                aes(x=Butts.weight,y=NULL, ymin=exp(lwr), 
                    ymax=exp(upr)),alpha=0.2)
plotfit.avgci
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df,
                aes(x=Butts.weight,y=NULL, ymin=exp(lwr), ymax=exp(upr)),
                alpha=0.1)
plotfit.indivci
ggsave(plot=plotfit.indivci, file='../../MyStuff/Images/lm-predict-nests-003.png',
       h=4, w=6, units="in", dpi=300)






#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------

#------------------------------ CRD ANOVA -----------------------
# Does MEAN sugars/serving depend on display shelf?

# Declare shelf as a factor
cereal$shelfF <- factor(cereal$shelf)
cereal[1:3,]
str(cereal)

# Preliminary plots
prelimplot <- ggplot(data=cereal, aes(x=shelfF, y=sugars))+
    ggtitle("Compare MEAN sugar among display shelves")+
    xlab("Shelf")+ylab("Sugar (g)")+
    geom_point(position=position_jitter(width=0.05))+
    geom_boxplot(alpha=0.2, notch=TRUE, outlier.size=-1) # -1 removes outiers from plot
prelimplot
ggsave(prelimplot, file='../../MyStuff/Images/lm-prelim-cereal-001.png',
       h=4, w=6, units="in", dpi=300)

# Table of means and SD
library(plyr)
sumstat <- ddply(cereal, "shelfF", sf.simple.summary, 
                 variable="sugars", crd=TRUE)
sumstat


# Fit a single factor CRD anova
shelf.fit <- lm(sugars ~ shelfF, data=cereal)
str(shelf.fit)
anova(shelf.fit) # CAUTION - Type I (incremental)
summary(shelf.fit) # Not useful

# use the emmeans::emmeans() package to get the individual 
# means and the pairwise comparisons and plot them
library(emmeans)
shelf.fit.emmo <- emmeans::emmeans(shelf.fit, ~shelfF)

# Where do difference in marginal means lie?
shelf.fit.cld <- multcomp::cld(shelf.fit.emmo)
shelf.fit.cld


# You can now write ggplot() to display the results or
# use my builtin functions (have a look at them)
sf.cld.plot.bar(shelf.fit.cld, "shelfF")+
  ggtitle("Mean sugars/serving by shelf (with 95% ci)")+
  xlab("Shelf")+ylab("Mean sugars/serving with 95% ci")
ggsave(file='../../MyStuff/Images/lm-cld-cereal-001.png',
       h=4, w=6, units="in",dpi=300)

sf.cld.plot.line(shelf.fit.cld, "shelfF")+
  ggtitle("Mean sugar/serving by shelf (with 95% ci)")+
  xlab("Shelf")+ylab("Mean sugars/serving with 95% ci")
ggsave(file='../../MyStuff/Images/lm-cld-cereal-002.png',
       h=4, w=6, units="in", dpi=300)


# Estimate all of the pairwise differences
pairs(shelf.fit.emmo)
confint(pairs(shelf.fit.emmo))

summary(pairs(shelf.fit.emmo), infer=TRUE)

#---------------------------------------------------------------------------------
# Birds 'n Butts
# Is there a difference in mean weight of buts between 3 nest contents?

library(readxl)
butts <- readxl::read_excel('../sampledata/bird-butts-data.xlsx', 
              sheet='Correlational', 
              col_names=TRUE, 
              trim_ws=TRUE,
              skip=1,
              .name_repair="universal")
butts[1:5,]

# Fix the names
names(butts)[ grepl('wieght', names(butts))] <- "Butts.weight"
butts[1:5,]

# Look like we need a log-transform on the number of mites
butts$log.mites <- log(butts$Number.of.mites)
outlier <- butts[butts$log.mites<= 0,]
outlier



# Declare Nest.Contents as a factor
butts$Nest.contentF <- factor(butts$Nest.content,
                    levels=c("eggs","chicks","empty"),
                    order=TRUE)
butts[1:3,]
str(butts)

# Fit a single factor CRD anova
nc.fit <- lm(Butts.weight ~ Nest.contentF, data=butts)
str(nc.fit)
anova(nc.fit) # CAUTION - Type I (incremental)
summary(nc.fit) # Not useful

# use the emmeans::emmeans() package to get the individual 
# means and the pairwise comparisons and plot them
library(emmeans)
nc.fit.emmo <- emmeans::emmeans(nc.fit, ~Nest.contentF)

# Where do difference in marginal means lie?
nc.fit.cld <- multcomp::cld(nc.fit.emmo)
nc.fit.cld

# You can now write ggplot() to display the results or
# use my builtin functions (have a look at them)
sf.cld.plot.bar(nc.fit.cld, "Nest.contentF")+
  ggtitle("Mean Butts.weight by nest contents(with 95% ci)")+
  xlab("Nest Content")+ylab("Mean Butts.weight with 95% ci")
ggsave(file='../../MyStuff/Images/lm-cld-butts-001.png',
       h=4, w=6,unit="in", dpi=300)

sf.cld.plot.line(nc.fit.cld, "Nest.contentF")+
  ggtitle("Mean Butts.weight by nest content (with 95% ci)")+
  xlab("Nest Content")+ylab("Mean Butts.weight with 95% ci")
ggsave(file='../../MyStuff/Images/lm-cld-butts-002.png',
       h=4, w=6,unit="in", dpi=300)


# Estimate all of the pairwise differences
pairs(nc.fit.emmo)
confint(pairs(nc.fit.emmo))



#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------
#  CRD ANCOVA

# Is the relationship between calories and fat the same for all display shelves?\\

# Declare shelf as a factor
cereal$shelfF <- factor(cereal$shelf)
cereal[1:3,]
str(cereal)

# Preliminary plots
prelimplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelfF))+
    ggtitle("Compare calories vs fat by shelf")+
    xlab("Calories")+ylab("Calories")+
    geom_point(position=position_jitter(width=0.05))+
    geom_smooth(method="lm", se=FALSE)
prelimplot
ggsave(prelimplot, file='../../MyStuff/Images/lm-ancova-prelim-cereal-001.png',
       h=4, w=6, units="in", dpi=300)


# Fit a single factor CRD ancova
library(car)
shelf.fit <- lm(calories ~ shelfF + fat + shelfF:fat, data=cereal,
                contrasts=list(shelfF=contr.sum))
anova(shelf.fit) # CAUTION - Type I (incremental)
car::Anova(shelf.fit, type=3) # marginal tests

# summary table is useless
summary(shelf.fit)

# use the emmeans::emmeans() package to get the individual 
# means and the pairwise comparisons and plot them
library(emmeans)
shelf.fit.emmo <- emmeans::emtrends(shelf.fit, ~shelfF, var="fat")
shelf.fit.cld <- multcomp::cld(shelf.fit.emmo)
shelf.fit.cld


# Fit the parallel slope model
library(car)
shelf.fit2 <- lm(calories ~ shelfF + fat, data=cereal,
                contrasts=list(shelfF=contr.sum))
car::Anova(shelf.fit2, type=3) # marginal tests

# summary table is useless except for estimate of common slope
summary(shelf.fit2)

# use the emmeans::emmeans() package to get the individual 
# means and the pairwise comparisons and plot them
library(emmeans)
shelf.fit2.emmo <- emmeans::emmeans(shelf.fit2, ~shelfF)

# Where do difference in marginal means lie?
shelf.fit2.cld <- multcomp::cld(shelf.fit2.emmo)
shelf.fit2.cld



#---------------------------------------------------------------------------------
# Birds 'n Butts
# Is there a difference in relationship between $\log{{Number of Mites}$ and $Butts Weight$ the same for all 3 nest contents?


library(readxl)
butts <- readxl::read_excel('../sampledata/bird-butts-data.xlsx', 
                            sheet='Correlational', 
                            col_names=TRUE,
                            trim_ws=TRUE,
                            skip=1,
                            .name_repair="universal")
butts[1:5,]

# Fix the names
names(butts)[ grepl('wieght', names(butts))] <- "Butts.weight"
butts[1:5,]

# Look like we need a log-transform on the number of mites
butts$log.mites <- log(butts$Number.of.mites)
outlier <- butts[butts$log.mites<= 0,]
outlier



# Declare Nest.Contents as a factor
butts$Nest.contentF <- factor(butts$Nest.content,
                    levels=c("eggs","chicks","empty"),
                    order=TRUE)
butts[1:3,]
str(butts)

# preliminary plot
plot <- ggplot(data=butts, aes(x=Butts.weight, y=log.mites, color=Nest.contentF))+
   ggtitle("Non-parallel slopes")+
   geom_point( position=position_jitter(h=.1, w=.1))+
   geom_smooth(method="lm", se=FALSE)
plot
ggsave(plot=plot, file='../../MyStuff/Images/lm-ancova-butts-001.png',
       h=4, w=6,unit="in", dpi=300)


# Fit a single factor CRD ancova
np.fit <- lm(log.mites ~ Nest.contentF + Butts.weight + Butts.weight:Nest.contentF, data=butts,
              contrasts=list(Nest.contentF=contr.sum))
car::Anova(np.fit, type=3)


# use the emmeans::emmeans() package to get the individual 
# means and plot them
library(emmeans)
np.fit.emmo <- emmeans::emtrends(np.fit, ~Nest.contentF, var='Butts.weight')
np.fit.cld <- multcomp::cld(np.fit.emmo)
np.fit.cld


# Fit a single factor CRD ancova with parallel slopes
p.fit <- lm(log.mites ~ Nest.contentF + Butts.weight, data=butts,
              contrasts=list(Nest.contentF=contr.sum))
car::Anova(p.fit, type=3)

summary(p.fit)

# use the emmeans::emmeans() package to get the individual 
# means and the pairwise comparisons and plot them
library(emmeans)
p.fit.emmo <- emmeans::emmeans(p.fit, ~Nest.contentF)
p.fit.cld <- multcomp::cld(p.fit.emmo)
p.fit.cld






#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------
# Logistic ANOVA
#
# Does PROPORTION of accidents that result in fatality differ by day of the week?

# Read in the accidents dataset
radf <- read.csv(file.path("..","sampledata","Accidents","road-accidents-2010.csv"),
                header=TRUE,
                as.is=TRUE,
                strip.white=TRUE)
dim(radf)
names(radf)

# Declare day of week as a factor
radf$Day_of_WeekF <- factor(radf$Day_of_Week)

# Recode AccidentSeverity as fatal or not
xtabs(~Accident_Severity, data=radf)
library(car)
radf$fatal <- recode(radf$Accident_Severity,
              ' 1="yes";2:3="no"  ')
xtabs(~fatal+Accident_Severity, data=radf)


# Need to also declare response as a factor (because it is categorical)
radf$fatalF <- factor(radf$fatal, level=c("no","yes"), order=TRUE)

# Preliminary tabulation
xtabs(~fatalF+Day_of_WeekF, data=radf)
round(prop.table(xtabs(~fatalF+Day_of_WeekF, data=radf),2),3)


# Now to fit the glm model
fatal.fit <- glm(fatalF ~ Day_of_WeekF, family=binomial, data=radf)

anova(fatal.fit, test='Chi') # CAUTION Type I tests
summary(fatal.fit) # not useful

# We get the emmeans (eventhough these are logits of proportions)
fatal.fit.emmo <- emmeans::emmeans(fatal.fit, ~Day_of_WeekF)

# where do the p(fatal) differ?
fatal.fit.cld <- multcomp::cld(fatal.fit.emmo)
fatal.fit.cld

multcomp::cld(fatal.fit.emmo, type="response")

sf.cld.plot.bar(fatal.fit.cld, "Day_of_WeekF")+
  ggtitle("logit(fatality) by day of week (with 95% ci)")+
  xlab("Day of Week (1=Sunday)")+ylab("logit(fatality rate) with 95% ci")
ggsave(file='../../MyStuff/Images/glm-cld-radf-001.png',
       h=4, w=6, units="in", dpi=300)

sf.cld.plot.line(fatal.fit.cld, "Day_of_WeekF")+
  ggtitle("logit(fatality) by day of week (with 95% ci)")+
  xlab("Day of Week (1=Sunday)")+ylab("logit(fatality rate) with 95% ci")
ggsave(file='../../MyStuff/Images/glm-cld-radf-002.png',
       h=4, w=6, units="in", dpi=300)


summary(fatal.fit.emmo, type="response")


# Estimate the odds ratio
pairs(fatal.fit.emmo)
summary(pairs(fatal.fit.emmo), infer=TRUE, type="response")



# Exercise - look at proportion of fatal by weather conditions
dim(radf)
radf2 <- radf[ radf$Weather_Conditions %in% 1:6,]
dim(radf2)

# Define the weather conditons as a factor
radf2$WeatherF <- factor(radf2$Weather_Conditions)
xtabs(~fatalF + WeatherF, data=radf2)
round(prop.table(xtabs(~fatalF+WeatherF, data=radf2),2),3)

# Now to fit the glm model
fatal.fit <- glm(fatalF ~ WeatherF, family=binomial, data=radf2)

anova(fatal.fit, test='Chi') # CAUTION Type I tests
summary(fatal.fit) # not useful

# We get the emmeans (eventhough these are logits of proportions)
fatal.fit.emmo <- emmeans::emmeans(fatal.fit, ~WeatherF)

# where do the p(fatal) differ?
fatal.fit.cld <- multcomp::cld(fatal.fit.emmo)
fatal.fit.cld  # interesting what happens

sf.cld.plot.bar(fatal.fit.cld, "WeatherF")+
  ggtitle("logit(fatality) by Weather Conditions (with 95% ci)")+
  xlab("Weather Conditions")+ylab("logit(fatality rate) with 95% ci")
ggsave(file='../../MyStuff/Images/glm-cld-radf-weather-001.png',
    h=4, w=6, units='in', dpi=300)

sf.cld.plot.line(fatal.fit.cld, "WeatherF")+
  ggtitle("logit(fatality) by Weather Conditions (with 95% ci)")+
  xlab("Weather Conditions")+ylab("logit(fatality rate) with 95% ci")
ggsave(file='../../MyStuff/Images/glm-cld-radf-weather-002.png',
    h=4, w=6, units='in', dpi=300)


#-----------------------------------------------------
# Exercise - look at proportion of fatal by speed
dim(radf)
names(radf)
xtabs(~Speed_limit+fatalF, data=radf, exclude=NULL, na.action=na.pass)
round(prop.table(xtabs(~fatalF+Speed_limit, data=radf),2),3)

radf2 <- radf[ radf$Speed_limit >= 20,]
radf2$Speed_limitF <- factor(radf2$Speed_limit)

# Now to fit the glm model
fatal.fit <- glm(fatalF ~ Speed_limitF, family=binomial, data=radf2)

anova(fatal.fit, test='Chi') # CAUTION Type I tests
summary(fatal.fit) # not useful

# We get the emmeans (eventhough these are logits of proportions)
fatal.fit.emmo <- emmeans::emmeans(fatal.fit, ~Speed_limitF)

# where do the p(fatal) differ?
fatal.fit.cld <- multcomp::cld(fatal.fit.emmo, type="response")
fatal.fit.cld  # interesting what happens

sf.cld.plot.bar(fatal.fit.cld, "Speed_limitF")+
  ggtitle("Prob(fatality) by Speed Limit (with 95% ci)")+
  xlab("Speed limit")+ylab("Prob(fatality rate) with 95% ci")

ggsave(file='../../MyStuff/Images/glm-cld-radf-speed-001.png',
    h=4, w=6, units='in', dpi=300)

sf.cld.plot.line(fatal.fit.cld, "Speed_limitF")+
  ggtitle("Prob(fatality) by Speed limit (with 95% ci)")+
  xlab("Speed limit")+ylab("Prob(fatality rate) with 95% ci")
ggsave(file='../../MyStuff/Images/glm-cld-radf-speed-002.png',
    h=4, w=6, units='in', dpi=300)


