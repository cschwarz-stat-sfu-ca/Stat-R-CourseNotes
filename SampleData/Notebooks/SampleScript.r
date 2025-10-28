# This will be used as an example of creating a notebook in HTML, DOC, or PDF formats.

# See 
#     http://rmarkdown.rstudio.com/articles_report_from_r_script.html 
# for more informaton




options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')


#--------------------------------------------------------
# This script will read in the cereal data set, 
#    do a simple listing,
#    fit a regression line, 
#       draw a scatter plot and add the line to the plot
#    do a single factor crd anova
#       get the compact letter display
#       make some plots


# load required libraries
library(ggplot2)
library(emmeans)
library(readxl)

# Read in the cereal data from a csv file
cereal <- read.csv('cereal.csv', 
              header=TRUE, as.is=TRUE, strip.white=TRUE)


# Define new variables and factors (for categorical variables). CHeck the structure of the data frame
cereal$shelfF <- factor(cereal$shelf)
cereal$Calories.fr.Protein <- cereal$protein * 4;

str(cereal)

# List  the first few records
cereal[1:5,]

# List some variables
cereal$calories
cereal[,"calories"]
cereal$fat
cereal[1:5,c("name","fat","calories")]

# Make a basic scatter plot
plotbasic <- ggplot(data=cereal, aes(x=fat, y=calories))+
      ggtitle("Calories vs Fat in cereals")+
      xlab("Grams of Fat")+ylab("Calories/serving")+
      geom_point()
plotbasic

plotbasic2 <- ggplot(data=cereal, aes(x=fat, y=calories))+
      ggtitle("Calories vs Fat in cereals")+
      xlab("Grams of Fat")+ylab("Calories/serving")+
      geom_jitter()
plotbasic2


# Same plot in base R graphics (ugh) Try to avoid using Base R graphics
plot(jitter(cereal$fat), jitter(cereal$calories),
   main="Plot of calories vs. grams of fat",
   xlab="Grams of fat", ylab='Calories/servince')


# Fit a regression between calories and grams of fat
fit.calories.fat <- lm( calories ~ fat, data=cereal)
summary(fit.calories.fat)
anova(fit.calories.fat) # careful Type I SS
coef(fit.calories.fat)
sqrt(diag(vcov(fit.calories.fat))) # extract the SE
confint(fit.calories.fat) # confidence intervals on parameters

names(summary(fit.calories.fat))
summary(fit.calories.fat)$r.squared
summary(fit.calories.fat)$sigma

class(fit.calories.fat)
methods(class=class(fit.calories.fat))


# Add the fitted line to the scatter plot; and save
plotline <- plotbasic2 +
  geom_abline(intercept=coef(fit.calories.fat)[1],
              slope    =coef(fit.calories.fat)[2])
plotline


# Or, if you don't want' to do the actual fit, use ggplot directly
plot.calories.fat <- ggplot(data=cereal, aes(x=fat, y=calories)) +
    geom_jitter(shape=1) +    # Use hollow circles
    geom_smooth(method=lm,   # Add linear regression line
                se=FALSE)    # Don't add shaded confidence region
plot.calories.fat


# Make a nicer scatter plot and add the fitted line in base R graphics. Ugh. Not recommended to use Base R graphics
png("cal-vs-fat3-base.png")
plot(jitter(cereal$fat), jitter(cereal$calories),
   main="Plot of calories vs. grams of fat",
   xlab="Grams of fat", ylab='Calories/servince')
abline(fit.calories.fat)
dev.off()


# Do a simple single factor ANOVA
# Is the mean number of calories the same for all shelves
# Need to use a FACTOR variable for the categorical variable
fit.sugars.shelf <- lm( sugars ~ shelfF, data=cereal)
anova(fit.sugars.shelf)

# Estimate the marginal means along with confidence limits and Tukey multiple comparison.
fit.sugars.shelf.lsmo <- emmeans::emmeans(fit.sugars.shelf, ~shelfF)
fit.sugars.shelf.cld <- CLD(fit.sugars.shelf.lsmo, adjust='tukey')
fit.sugars.shelf.cld
cld.plot <- sf.cld.plot.bar(fit.sugars.shelf.cld, "shelfF", order=FALSE)
cld.plot
# Estimate the pairwise differences
pairs(fit.sugars.shelf.lsmo)












